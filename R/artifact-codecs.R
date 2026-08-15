# Durable codecs for deliverable specifications and typed artifacts

tempest_artifact_codec_abort <- function(
  message,
  ...,
  class = "tempest_artifact_codec_error",
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = unique(c(
      class,
      "tempest_artifact_codec_error",
      "tempest_persistence_error",
      "tempest_error"
    )),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_canonical_value <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    is.function(value) ||
      is.environment(value) ||
      typeof(value) %in% c("externalptr", "weakref")
  ) {
    tempest_artifact_codec_abort(
      "Canonical records cannot contain functions, environments, or external pointers."
    )
  }
  if (inherits(value, "S7_object")) {
    tempest_artifact_codec_abort(
      "S7 objects must be converted to explicit records before encoding."
    )
  }
  if (is.object(value)) {
    tempest_artifact_codec_abort(
      "Canonical JSON content must use plain JSON-compatible R values."
    )
  }
  if (is.list(value)) {
    value_names <- names(value)
    if (
      !is.null(value_names) &&
        length(value_names) > 0L &&
        any(nzchar(value_names)) &&
        any(!nzchar(value_names))
    ) {
      tempest_artifact_codec_abort(
        "Canonical lists must be either fully named or fully unnamed."
      )
    }
    if (!is.null(value_names) && length(value_names) > 0L) {
      order <- order(value_names)
      value <- value[order]
    }
    return(lapply(value, tempest_canonical_value))
  }
  if (!is.atomic(value)) {
    tempest_artifact_codec_abort(
      "Unsupported canonical value type {.val {typeof(value)}}."
    )
  }
  if (is.complex(value) || is.raw(value)) {
    tempest_artifact_codec_abort(
      "Canonical records do not support complex or raw values."
    )
  }
  if (
    anyNA(value) ||
      (is.numeric(value) && any(!is.finite(value)))
  ) {
    tempest_artifact_codec_abort(
      "Canonical JSON content cannot contain missing or non-finite values."
    )
  }
  value_names <- names(value)
  if (!is.null(value_names) && length(value_names) > 0L) {
    tempest_artifact_codec_abort(
      "Named atomic vectors are ambiguous; use a named list for a JSON object."
    )
  }
  unclass(value)
}

tempest_canonical_json <- function(value) {
  tryCatch(
    as.character(jsonlite::toJSON(
      tempest_canonical_value(value),
      auto_unbox = TRUE,
      null = "null",
      na = "string",
      digits = NA,
      pretty = FALSE,
      force = TRUE
    )),
    error = function(error) {
      if (inherits(error, "tempest_artifact_codec_error")) {
        stop(error)
      }
      tempest_artifact_codec_abort(
        "Could not encode a canonical JSON record.",
        parent = error
      )
    }
  )
}

tempest_deliverable_spec_checksum <- function(spec_or_data) {
  data <- if (S7::S7_inherits(spec_or_data, TempestDeliverableSpec)) {
    tempest_deliverable_spec_data(spec_or_data)
  } else {
    spec_or_data
  }
  if (
    is.list(data) &&
      !is.null(data$operation_versions) &&
      is.atomic(data$operation_versions)
  ) {
    data$operation_versions <- as.list(data$operation_versions)
  }
  digest::digest(
    tempest_canonical_json(data),
    algo = "sha256",
    serialize = FALSE
  )
}

tempest_artifact_codec_media_supported <- function(patterns, media_type) {
  any(vapply(
    patterns,
    function(pattern) {
      if (identical(pattern, "*/*") || identical(pattern, media_type)) {
        return(TRUE)
      }
      if (endsWith(pattern, "/*")) {
        return(startsWith(
          media_type,
          paste0(sub("/\\*$", "", pattern), "/")
        ))
      }
      if (grepl("/\\*\\+", pattern)) {
        parts <- strsplit(pattern, "/\\*\\+")[[1]]
        media_parts <- strsplit(media_type, "/", fixed = TRUE)[[1]]
        return(
          length(media_parts) == 2L &&
            identical(media_parts[[1]], parts[[1]]) &&
            endsWith(media_parts[[2]], paste0("+", parts[[2]]))
        )
      }
      FALSE
    },
    logical(1)
  ))
}

#' Define a typed artifact codec
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Codec definitions keep executable encode/decode functions in a runtime
#' registry while exposing only serializable identity and media metadata in
#' durable listings.
#'
#' @param codec_id Stable codec identifier.
#' @param encode,decode Runtime functions for inline content. `encode` returns
#'   raw bytes or a list containing `bytes` and an optional `extension`;
#'   `decode` returns reconstructed inline content.
#' @param version Stable codec version.
#' @param media_types Supported media types or patterns such as `"text/*"`.
#' @param extension Default filename extension without a leading dot.
#' @param supports Optional runtime predicate for content selection.
#' @param external Whether the codec represents an external storage reference
#'   rather than inline bytes.
#' @param priority Numeric automatic-selection priority.
#' @param metadata Canonical JSON-compatible descriptive metadata.
#' @return A runtime artifact codec definition.
#' @export
tempest_artifact_codec_definition <- function(
  codec_id,
  encode = NULL,
  decode = NULL,
  version = "1",
  media_types = "*/*",
  extension = "bin",
  supports = NULL,
  external = FALSE,
  priority = 0,
  metadata = list()
) {
  codec_id <- tempest_contract_id(codec_id, "codec_id")
  version <- tempest_workflow_version(version, "version")
  media_types <- tempest_workflow_character(media_types, "media_types")
  valid_media <- grepl(
    "^[^[:space:]/]+/[^[:space:]/]+$",
    media_types
  )
  if (length(media_types) == 0L || any(!valid_media)) {
    tempest_artifact_codec_abort(
      "{.arg media_types} must contain valid media-type patterns."
    )
  }
  extension <- tempest_workflow_scalar(extension, "extension")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._+-]*$", extension)) {
    tempest_artifact_codec_abort(
      "{.arg extension} must be a safe filename extension without a leading dot."
    )
  }
  external <- tempest_workflow_flag(external, "external")
  if (
    !external &&
      (!is.function(encode) || !is.function(decode))
  ) {
    tempest_artifact_codec_abort(
      "Inline codecs require both {.arg encode} and {.arg decode} functions."
    )
  }
  if (!is.null(supports) && !is.function(supports)) {
    tempest_artifact_codec_abort(
      "{.arg supports} must be NULL or a function."
    )
  }
  if (
    !is.numeric(priority) ||
      length(priority) != 1L ||
      is.na(priority) ||
      !is.finite(priority)
  ) {
    tempest_artifact_codec_abort(
      "{.arg priority} must be one finite number."
    )
  }
  metadata <- tempest_workflow_serializable_list(metadata, "metadata")
  structure(
    list(
      codec_id = codec_id,
      version = version,
      media_types = unique(media_types),
      extension = extension,
      encode = encode,
      decode = decode,
      supports = supports,
      external = external,
      priority = as.numeric(priority),
      metadata = metadata
    ),
    class = c("tempest_artifact_codec_definition", "list")
  )
}

#' @rdname tempest_artifact_codec_definition
#' @param ... Arguments forwarded to [tempest_artifact_codec_definition()].
#' @export
tempest_artifact_codec <- function(...) {
  tempest_artifact_codec_definition(...)
}

tempest_artifact_codec_description <- function(codec) {
  codec[c(
    "codec_id",
    "version",
    "media_types",
    "extension",
    "external",
    "priority",
    "metadata"
  )]
}

tempest_artifact_codec_text_decode <- function(bytes) {
  text <- tryCatch(
    rawToChar(bytes),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Artifact content is not valid UTF-8 text.",
        parent = error
      )
    }
  )
  if (!isTRUE(validUTF8(text))) {
    tempest_artifact_codec_abort(
      "Artifact content is not valid UTF-8 text."
    )
  }
  enc2utf8(text)
}

tempest_builtin_artifact_codecs <- function() {
  list(
    tempest_artifact_codec_definition(
      "tempest.text.utf8",
      media_types = "text/*",
      extension = "txt",
      priority = 100,
      supports = function(content) {
        is.character(content) &&
          length(content) == 1L &&
          !is.na(content)
      },
      encode = function(content, media_type) {
        list(
          bytes = charToRaw(enc2utf8(content)),
          extension = if (identical(media_type, "text/markdown")) {
            "md"
          } else {
            "txt"
          }
        )
      },
      decode = function(bytes) {
        tempest_artifact_codec_text_decode(bytes)
      }
    ),
    tempest_artifact_codec_definition(
      "tempest.json.canonical",
      media_types = c("application/json", "application/*+json"),
      extension = "json",
      priority = 100,
      supports = function(content) {
        tryCatch(
          {
            tempest_canonical_json(content)
            TRUE
          },
          error = function(error) FALSE
        )
      },
      encode = function(content) {
        charToRaw(enc2utf8(tempest_canonical_json(content)))
      },
      decode = function(bytes) {
        text <- tempest_artifact_codec_text_decode(bytes)
        value <- tryCatch(
          jsonlite::fromJSON(text, simplifyVector = FALSE),
          error = function(error) {
            tempest_artifact_codec_abort(
              "Artifact content is not valid canonical JSON.",
              parent = error
            )
          }
        )
        if (
          !identical(
            charToRaw(enc2utf8(tempest_canonical_json(value))),
            bytes
          )
        ) {
          tempest_artifact_codec_abort(
            "Artifact JSON bytes are not in canonical form."
          )
        }
        value
      }
    ),
    tempest_artifact_codec_definition(
      "tempest.external.reference",
      media_types = "*/*",
      extension = "ref",
      external = TRUE,
      priority = -1000
    )
  )
}

TempestArtifactCodecRegistry <- R6::R6Class(
  "TempestArtifactCodecRegistry",
  public = list(
    initialize = function(codecs = list(), include_builtins = TRUE) {
      include_builtins <- tempest_workflow_flag(
        include_builtins,
        "include_builtins"
      )
      private$codecs <- new.env(parent = emptyenv())
      if (include_builtins) {
        self$register_many(tempest_builtin_artifact_codecs())
      }
      self$register_many(codecs)
      invisible(self)
    },

    register = function(codec, replace = FALSE) {
      if (!inherits(codec, "tempest_artifact_codec_definition")) {
        tempest_artifact_codec_abort(
          "{.arg codec} must be created by {.fn tempest_artifact_codec_definition}."
        )
      }
      replace <- tempest_workflow_flag(replace, "replace")
      codec_id <- codec$codec_id
      if (
        exists(codec_id, private$codecs, inherits = FALSE) &&
          !replace
      ) {
        tempest_artifact_codec_abort(
          "Artifact codec {.val {codec_id}} is already registered."
        )
      }
      assign(codec_id, codec, private$codecs)
      invisible(codec_id)
    },

    register_many = function(codecs) {
      codecs <- codecs %||% list()
      if (!is.list(codecs) || is.data.frame(codecs)) {
        tempest_artifact_codec_abort(
          "{.arg codecs} must be a list of codec definitions."
        )
      }
      for (codec in codecs) {
        self$register(codec)
      }
      invisible(self)
    },

    resolve = function(
      codec_id,
      version = NULL,
      media_type = NULL,
      external = NULL
    ) {
      codec_id <- tempest_contract_id(codec_id, "codec_id")
      if (!exists(codec_id, private$codecs, inherits = FALSE)) {
        tempest_artifact_codec_abort(
          "Artifact codec {.val {codec_id}} is not registered.",
          class = "tempest_artifact_codec_missing_error"
        )
      }
      codec <- get(codec_id, private$codecs, inherits = FALSE)
      if (
        !is.null(version) &&
          !identical(
            codec$version,
            tempest_workflow_version(version, "codec_version")
          )
      ) {
        tempest_artifact_codec_abort(
          "Artifact codec {.val {codec_id}} has an incompatible version.",
          class = "tempest_artifact_codec_version_error"
        )
      }
      if (
        !is.null(media_type) &&
          !tempest_artifact_codec_media_supported(
            codec$media_types,
            tempest_workflow_scalar(media_type, "media_type")
          )
      ) {
        tempest_artifact_codec_abort(
          "Artifact codec {.val {codec_id}} does not support media type {.val {media_type}}.",
          class = "tempest_artifact_codec_media_error"
        )
      }
      if (!is.null(external) && !identical(codec$external, external)) {
        tempest_artifact_codec_abort(
          "Artifact codec {.val {codec_id}} has an incompatible storage mode.",
          class = "tempest_artifact_codec_mode_error"
        )
      }
      codec
    },

    describe = function(codec_id, version = NULL, media_type = NULL) {
      tempest_artifact_codec_description(self$resolve(
        codec_id,
        version = version,
        media_type = media_type
      ))
    },

    list = function() {
      codec_ids <- sort(ls(private$codecs, all.names = TRUE))
      stats::setNames(
        lapply(codec_ids, function(codec_id) {
          tempest_artifact_codec_description(
            get(codec_id, private$codecs, inherits = FALSE)
          )
        }),
        codec_ids
      )
    },

    select = function(content, media_type, external = FALSE) {
      media_type <- tempest_workflow_scalar(media_type, "media_type")
      external <- tempest_workflow_flag(external, "external")
      codec_ids <- sort(ls(private$codecs, all.names = TRUE))
      candidates <- lapply(codec_ids, function(codec_id) {
        get(codec_id, private$codecs, inherits = FALSE)
      })
      candidates <- Filter(
        function(codec) {
          identical(codec$external, external) &&
            tempest_artifact_codec_media_supported(
              codec$media_types,
              media_type
            ) &&
            (is.null(codec$supports) ||
              isTRUE(tryCatch(
                tempest_call_operation(
                  codec$supports,
                  list(content = content, media_type = media_type)
                ),
                error = function(error) FALSE
              )))
        },
        candidates
      )
      if (length(candidates) == 0L) {
        storage_mode <- if (external) "external" else "inline"
        tempest_artifact_codec_abort(
          "No registered {storage_mode} artifact codec supports media type {.val {media_type}} and the supplied content.",
          class = "tempest_artifact_codec_missing_error"
        )
      }
      priorities <- vapply(
        candidates,
        \(codec) codec$priority,
        numeric(1)
      )
      candidates[[order(
        -priorities,
        vapply(
          candidates,
          \(codec) codec$codec_id,
          character(1)
        )
      )[[1]]]]
    },

    encode = function(
      content,
      media_type,
      codec_id = NULL,
      codec_version = NULL
    ) {
      codec <- if (is.null(codec_id)) {
        self$select(content, media_type)
      } else {
        self$resolve(
          codec_id,
          version = codec_version,
          media_type = media_type,
          external = FALSE
        )
      }
      encoded <- tryCatch(
        tempest_call_operation(
          codec$encode,
          list(content = content, media_type = media_type)
        ),
        error = function(error) {
          if (inherits(error, "tempest_artifact_codec_error")) {
            stop(error)
          }
          tempest_artifact_codec_abort(
            "Artifact codec {.val {codec$codec_id}} failed to encode content.",
            class = "tempest_artifact_codec_encode_error",
            parent = error
          )
        }
      )
      if (is.raw(encoded)) {
        encoded <- list(bytes = encoded)
      }
      if (!is.list(encoded) || !is.raw(encoded$bytes)) {
        tempest_artifact_codec_abort(
          "Artifact codec {.val {codec$codec_id}} returned invalid encoded bytes.",
          class = "tempest_artifact_codec_encode_error"
        )
      }
      extension <- tempest_workflow_scalar(
        encoded$extension %||% codec$extension,
        "extension"
      )
      if (!grepl("^[A-Za-z0-9][A-Za-z0-9._+-]*$", extension)) {
        tempest_artifact_codec_abort(
          "Artifact codec returned an unsafe filename extension.",
          class = "tempest_artifact_codec_encode_error"
        )
      }
      bytes <- encoded$bytes
      list(
        codec_id = codec$codec_id,
        codec_version = codec$version,
        media_type = media_type,
        extension = extension,
        bytes = bytes,
        byte_size = length(bytes),
        sha256 = digest::digest(
          bytes,
          algo = "sha256",
          serialize = FALSE
        )
      )
    },

    validate_record = function(record, external = NULL) {
      if (!is.list(record) || is.data.frame(record)) {
        tempest_artifact_codec_abort(
          "{.arg record} must be an artifact codec record."
        )
      }
      self$resolve(
        codec_id = record$codec_id,
        version = record$codec_version,
        media_type = record$media_type %||% NULL,
        external = external
      )
    },

    decode = function(record, bytes) {
      codec <- self$validate_record(record, external = FALSE)
      if (!is.raw(bytes)) {
        tempest_artifact_codec_abort(
          "{.arg bytes} must be a raw vector."
        )
      }
      byte_size <- suppressWarnings(
        as.numeric(record$byte_size %||% NA_real_)
      )
      expected_sha <- tempest_workflow_scalar(record$sha256, "sha256")
      actual_sha <- digest::digest(
        bytes,
        algo = "sha256",
        serialize = FALSE
      )
      if (
        length(byte_size) != 1L ||
          is.na(byte_size) ||
          byte_size != length(bytes) ||
          !identical(expected_sha, actual_sha)
      ) {
        tempest_artifact_codec_abort(
          "Artifact content failed byte-size or checksum validation.",
          class = "tempest_artifact_codec_checksum_error"
        )
      }
      tryCatch(
        tempest_call_operation(
          codec$decode,
          list(
            bytes = bytes,
            media_type = record$media_type %||% NULL,
            record = record
          )
        ),
        error = function(error) {
          if (inherits(error, "tempest_artifact_codec_error")) {
            stop(error)
          }
          tempest_artifact_codec_abort(
            "Artifact codec {.val {codec$codec_id}} failed to decode content.",
            class = "tempest_artifact_codec_decode_error",
            parent = error
          )
        }
      )
    }
  ),
  private = list(
    codecs = NULL
  ),
  cloneable = FALSE
)

#' Create a typed artifact codec registry
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Registries resolve codecs by stable id, version, and media type. Their
#' `list()` method deliberately excludes executable functions.
#'
#' @param codecs Artifact codec definitions to register.
#' @param include_builtins Include Tempest UTF-8 text, canonical JSON, and
#'   external-reference codecs.
#' @return A `TempestArtifactCodecRegistry`.
#' @export
tempest_artifact_codec_registry <- function(
  codecs = list(),
  include_builtins = TRUE
) {
  TempestArtifactCodecRegistry$new(
    codecs = codecs,
    include_builtins = include_builtins
  )
}

tempest_artifact_codec_registry_validate <- function(registry = NULL) {
  registry <- registry %||% tempest_artifact_codec_registry()
  if (!inherits(registry, "TempestArtifactCodecRegistry")) {
    tempest_artifact_codec_abort(
      "{.arg codec_registry} must be created by {.fn tempest_artifact_codec_registry}."
    )
  }
  registry
}

tempest_artifact_codec_encode <- function(
  content,
  media_type,
  registry = NULL,
  codec_id = NULL,
  codec_version = NULL
) {
  registry <- tempest_artifact_codec_registry_validate(registry)
  registry$encode(
    content = content,
    media_type = media_type,
    codec_id = codec_id,
    codec_version = codec_version
  )
}

tempest_artifact_codec_decode <- function(
  record,
  bytes,
  registry = NULL
) {
  registry <- tempest_artifact_codec_registry_validate(registry)
  registry$decode(record, bytes)
}

tempest_artifact_content_checksum <- function(
  content,
  storage_ref,
  media_type
) {
  if (is.null(content)) {
    storage_ref <- tempest_workflow_scalar(storage_ref, "storage_ref")
    return(digest::digest(
      enc2utf8(storage_ref),
      algo = "sha256",
      serialize = FALSE
    ))
  }
  bytes <- if (
    is.character(content) &&
      length(content) == 1L &&
      !is.na(content)
  ) {
    charToRaw(enc2utf8(content))
  } else {
    charToRaw(enc2utf8(tempest_canonical_json(content)))
  }
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

tempest_codec_character <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(character())
  }
  flattened <- unlist(value, use.names = TRUE)
  result <- as.character(flattened)
  names(result) <- names(flattened)
  result
}

tempest_codec_list <- function(value) {
  value %||% list()
}

tempest_deliverable_spec_record <- function(deliverable) {
  data <- tempest_deliverable_spec_data(deliverable)
  data$operation_versions <- as.list(data$operation_versions)
  data$spec_fingerprint <- tempest_deliverable_fingerprint(deliverable)
  data
}

tempest_deliverable_spec_from_data <- function(data) {
  if (!is.list(data)) {
    tempest_artifact_codec_abort(
      "{.arg data} must be a deliverable specification record."
    )
  }
  expected_fingerprint <- data$spec_fingerprint %||% NULL
  data$spec_fingerprint <- NULL
  deliverable <- tryCatch(
    tempest_deliverable_spec(
      deliverable_id = data$deliverable_id,
      title = data$title,
      purpose = data$purpose,
      instructions = data$instructions,
      version = data$version %||% "1",
      content_schema = tempest_codec_list(data$content_schema),
      required_fields = tempest_codec_character(data$required_fields),
      evidence_policy = data$evidence_policy %||% "source_attributed",
      generator_id = data$generator_id,
      validator_ids = tempest_codec_character(data$validator_ids),
      renderer_ids = tempest_codec_character(data$renderer_ids),
      exporter_ids = tempest_codec_character(data$exporter_ids),
      operation_versions = tempest_codec_character(
        data$operation_versions
      ),
      content_type = data$content_type %||% "text",
      media_types = tempest_codec_character(data$media_types),
      filename_policy = tempest_codec_list(data$filename_policy),
      requires_approval = isTRUE(data$requires_approval),
      metadata = tempest_codec_list(data$metadata)
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a deliverable specification record.",
        parent = error
      )
    }
  )
  actual_fingerprint <- tempest_deliverable_fingerprint(deliverable)
  if (
    !is.null(expected_fingerprint) &&
      !identical(expected_fingerprint, actual_fingerprint)
  ) {
    tempest_artifact_codec_abort(
      "Deliverable specification fingerprint validation failed."
    )
  }
  deliverable
}

tempest_validation_result_record <- function(result) {
  tempest_validation_result_data(result)
}

tempest_validation_result_from_data <- function(data) {
  if (!is.list(data)) {
    tempest_artifact_codec_abort(
      "{.arg data} must be a validation-result record."
    )
  }
  tryCatch(
    tempest_validation_result(
      validator_id = data$validator_id,
      status = data$status %||% "passed",
      message = data$message %||% NA_character_,
      details = tempest_codec_list(data$details),
      created_at = data$created_at
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a validation-result record.",
        parent = error
      )
    }
  )
}

tempest_artifact_record <- function(artifact, include_content = TRUE) {
  tempest_artifact_data(artifact, include_content = include_content)
}

tempest_artifact_from_data <- function(
  data,
  deliverable,
  content = if ("content" %in% names(data)) data$content else NULL
) {
  if (!is.list(data)) {
    tempest_artifact_codec_abort(
      "{.arg data} must be an artifact record."
    )
  }
  if (!S7::S7_inherits(deliverable, TempestDeliverableSpec)) {
    tempest_artifact_codec_abort(
      "{.arg deliverable} must be a deliverable specification."
    )
  }
  expected_fingerprint <- data$spec_fingerprint %||% NULL
  actual_fingerprint <- tempest_deliverable_fingerprint(deliverable)
  if (
    !identical(data$deliverable_id, deliverable@deliverable_id) ||
      !identical(data$deliverable_version, deliverable@version) ||
      !identical(expected_fingerprint, actual_fingerprint)
  ) {
    tempest_artifact_codec_abort(
      "Artifact deliverable identity or fingerprint validation failed."
    )
  }
  storage_ref <- data$storage_ref %||% NA_character_
  media_type <- data$media_type
  expected_checksum <- data$checksum
  actual_checksum <- tempest_artifact_content_checksum(
    content,
    storage_ref,
    media_type
  )
  if (!identical(expected_checksum, actual_checksum)) {
    tempest_artifact_codec_abort(
      "Artifact content checksum validation failed."
    )
  }
  validation_results <- lapply(
    data$validation_results %||% list(),
    tempest_validation_result_from_data
  )
  optional_id <- function(value) value %||% NA_character_
  tryCatch(
    tempest_artifact(
      deliverable = deliverable,
      content = content,
      storage_ref = storage_ref,
      artifact_id = data$artifact_id,
      artifact_kind = data$artifact_kind %||% "primary",
      media_type = media_type,
      schema_version = as.integer(data$schema_version %||% 1L),
      producer_operation_id = optional_id(data$producer_operation_id),
      run_id = optional_id(data$run_id),
      step_id = optional_id(data$step_id),
      expert_id = optional_id(data$expert_id),
      resource_ids = tempest_codec_character(data$resource_ids),
      claim_ids = tempest_codec_character(data$claim_ids),
      evidence_span_ids = tempest_codec_character(data$evidence_span_ids),
      parent_artifact_ids = tempest_codec_character(
        data$parent_artifact_ids
      ),
      validation_results = validation_results,
      status = data$status %||% "draft",
      checksum = actual_checksum,
      created_at = data$created_at,
      updated_at = data$updated_at %||% data$created_at,
      metadata = tempest_codec_list(data$metadata)
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a typed artifact record.",
        parent = error
      )
    }
  )
}
