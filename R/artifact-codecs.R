# Durable codecs for deliverable specifications and typed artifacts

tempest_artifact_codec_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c(
      "tempest_artifact_codec_error",
      "tempest_persistence_error",
      "tempest_error"
    ),
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

tempest_artifact_codec_encode <- function(content, media_type) {
  media_type <- tempest_workflow_scalar(media_type, "media_type")
  if (
    is.character(content) &&
      length(content) == 1L &&
      !is.na(content)
  ) {
    text <- enc2utf8(content)
    bytes <- charToRaw(text)
    extension <- if (identical(media_type, "text/markdown")) "md" else "txt"
    codec_id <- "tempest.text.utf8"
  } else if (
    is.list(content) ||
      (is.atomic(content) && !is.raw(content) && !is.complex(content))
  ) {
    bytes <- charToRaw(enc2utf8(tempest_canonical_json(content)))
    extension <- "json"
    codec_id <- "tempest.json.canonical"
  } else {
    tempest_artifact_codec_abort(
      "No built-in artifact codec supports content of type {.val {typeof(content)}}."
    )
  }
  list(
    codec_id = codec_id,
    codec_version = "1",
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
}

tempest_artifact_codec_decode <- function(record, bytes) {
  if (!is.list(record)) {
    tempest_artifact_codec_abort(
      "{.arg record} must be an artifact codec record."
    )
  }
  if (!is.raw(bytes)) {
    tempest_artifact_codec_abort("{.arg bytes} must be a raw vector.")
  }
  codec_id <- tempest_workflow_scalar(record$codec_id, "codec_id")
  codec_version <- tempest_workflow_version(
    record$codec_version,
    "codec_version"
  )
  if (!identical(codec_version, "1")) {
    tempest_artifact_codec_abort(
      "Unsupported artifact codec version {.val {codec_version}} for {.val {codec_id}}."
    )
  }
  byte_size <- suppressWarnings(as.numeric(record$byte_size %||% NA_real_))
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
      "Artifact content failed byte-size or checksum validation."
    )
  }
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
  if (identical(codec_id, "tempest.text.utf8")) {
    return(enc2utf8(text))
  }
  if (identical(codec_id, "tempest.json.canonical")) {
    return(tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(error) {
        tempest_artifact_codec_abort(
          "Artifact content is not valid canonical JSON.",
          parent = error
        )
      }
    ))
  }
  tempest_artifact_codec_abort(
    "Unknown artifact codec {.val {codec_id}}."
  )
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
  tempest_artifact_codec_encode(content, media_type)$sha256
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
