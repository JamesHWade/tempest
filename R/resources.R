# Typed evidence resources

tempest_resource_data_fields <- function() {
  c(
    "resource_id",
    "resource_kind",
    "locator",
    "title",
    "media_type",
    "content",
    "storage_ref",
    "origin_connection_id",
    "scope_metadata",
    "content_hash",
    "retrieved_at",
    "redaction",
    "retention",
    "metadata",
    "schema_version"
  )
}

tempest_resource_record_fields <- function() {
  c(tempest_resource_data_fields(), "fingerprint")
}

tempest_resource_web_identity_valid <- function(
  resource_kind,
  locator,
  resource_id
) {
  !identical(resource_kind, "web") ||
    identical(resource_id, tempest_source_id(locator))
}

tempest_resource_assert_web_identity <- function(
  resource_kind,
  locator,
  resource_id
) {
  if (
    !tempest_resource_web_identity_valid(
      resource_kind,
      locator,
      resource_id
    )
  ) {
    tempest_product_validation_abort(
      paste0(
        "Web evidence resources must use the deterministic resource id ",
        "derived from their locator."
      )
    )
  }
  invisible(NULL)
}

tempest_resource_assert_inline_content_hash <- function(
  content,
  media_type,
  content_hash
) {
  if (is.null(content)) {
    return(invisible(NULL))
  }
  expected <- tempest_product_content_hash(content, media_type)
  if (is.na(content_hash) || !identical(content_hash, expected)) {
    tempest_product_validation_abort(
      paste0(
        "Inline resource content_hash must exactly match the captured ",
        "content and media type."
      )
    )
  }
  invisible(NULL)
}

TempestResource <- S7::new_class(
  "tempest_resource",
  properties = list(
    resource_id = tempest_product_prop_chr(),
    resource_kind = tempest_product_prop_chr(),
    locator = tempest_product_prop_chr(),
    title = tempest_product_prop_chr(),
    media_type = tempest_product_prop_chr(),
    content = S7::new_property(S7::class_any, default = NULL),
    storage_ref = tempest_product_prop_chr(NA_character_),
    origin_connection_id = tempest_product_prop_chr(NA_character_),
    scope_metadata = tempest_product_prop_list(),
    content_hash = tempest_product_prop_chr(NA_character_),
    retrieved_at = tempest_product_prop_chr(),
    redaction = tempest_product_prop_list(),
    retention = tempest_product_prop_list(),
    metadata = tempest_product_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  ),
  validator = function(self) {
    required_ids <- c(self@resource_id, self@resource_kind)
    if (
      !all(vapply(
        required_ids,
        tempest_opaque_identifier_valid,
        logical(1)
      ))
    ) {
      return(
        "Resource identity fields must be bounded credential-free identifiers"
      )
    }
    optional_ids <- c(
      self@storage_ref,
      self@origin_connection_id,
      self@content_hash
    )
    valid_optional <- vapply(
      optional_ids,
      \(value) is.na(value) || tempest_opaque_identifier_valid(value),
      logical(1)
    )
    if (!all(valid_optional)) {
      return(
        "Resource reference fields must be NA or credential-free identifiers"
      )
    }
    if (
      !tempest_resource_single_line_valid(self@locator) ||
        !tempest_resource_single_line_valid(self@title)
    ) {
      return("Resource locator and title must be non-empty single-line text")
    }
    if (
      !tempest_resource_web_identity_valid(
        self@resource_kind,
        self@locator,
        self@resource_id
      )
    ) {
      return("Web resource identity must be derived from its locator")
    }
    if (!identical(self@schema_version, 1L)) {
      return("Resource schema_version must be exact integer 1")
    }
    if (!tempest_ledger_timestamp_valid(self@retrieved_at)) {
      return("retrieved_at must be an exact canonical UTC timestamp")
    }
  }
)

tempest_is_exact_resource <- function(x) {
  identical(S7::S7_class(x), TempestResource)
}

tempest_resource_safe_scalar <- function(value, arg, identifier = FALSE) {
  value <- tempest_product_scalar(value, arg)
  if (tempest_contract_sensitive_scalar(value)) {
    tempest_product_validation_abort(
      "{.arg {arg}} cannot contain a credential or secret value."
    )
  }
  if (
    isTRUE(identifier) && !tempest_research_workspace_reference_id_valid(value)
  ) {
    tempest_product_validation_abort(
      "{.arg {arg}} must be a bounded credential-free identifier."
    )
  }
  value
}

tempest_resource_single_line_valid <- function(value) {
  rlang::is_string(value) &&
    !is.na(value) &&
    nzchar(value) &&
    !grepl("\r", value, fixed = TRUE) &&
    !grepl("\n", value, fixed = TRUE)
}

tempest_resource_single_line_scalar <- function(value, arg) {
  value <- tempest_resource_safe_scalar(value, arg)
  if (!tempest_resource_single_line_valid(value)) {
    tempest_product_validation_abort(
      "{.arg {arg}} must be non-empty single-line text."
    )
  }
  value
}

tempest_resource_optional_scalar <- function(value, arg, identifier = FALSE) {
  if (
    is.null(value) ||
      (is.character(value) && length(value) == 1L && is.na(value))
  ) {
    return(NA_character_)
  }
  tempest_resource_safe_scalar(value, arg, identifier = identifier)
}

tempest_resource_canonical_live_scalar <- function(value, canonical, field) {
  if (!identical(value, canonical)) {
    tempest_product_validation_abort(
      "Live resource field {.field {field}} must already be canonical."
    )
  }
  canonical
}

tempest_resource_content <- function(content) {
  if (is.null(content)) {
    return(NULL)
  }
  # Validate canonical encodability without rewriting caller-visible content.
  tryCatch(
    tempest_product_canonical_value(content),
    error = function(error) {
      tempest_product_validation_abort(
        "{.arg content} must be canonical JSON-compatible content or a single string.",
        parent = error
      )
    }
  )
  content
}

tempest_resource_metadata <- function(value, arg) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_product_validation_abort(
      "{.arg {arg}} must be an explicit non-data-frame list."
    )
  }
  tryCatch(
    tempest_product_canonical_json(value),
    error = function(error) {
      tempest_product_validation_abort(
        "{.arg {arg}} must contain only canonical JSON-compatible values.",
        parent = error
      )
    }
  )
  tempest_research_reference_value(
    value,
    path = arg,
    abort = tempest_product_validation_abort,
    noun = "Resource metadata"
  )
  value_for_scan <- value
  evidence_fields <- c(
    "snippet",
    "content_text",
    "context_text",
    "citation_context",
    "answer_context"
  )
  if (!is.null(names(value_for_scan))) {
    value_for_scan <- value_for_scan[
      !names(value_for_scan) %in% evidence_fields
    ]
  }
  sensitive <- tempest_contract_sensitive_values(value_for_scan, arg)
  if (length(sensitive) > 0L) {
    tempest_product_validation_abort(c(
      "{.arg {arg}} cannot contain credential or secret values.",
      x = "Sensitive field: {.field {sensitive[[1]]}}."
    ))
  }
  value
}

#' Create a typed evidence resource
#'
#' `r lifecycle::badge("experimental")`
#'
#' Resources identify provisional scientific evidence without requiring a
#' public URL. The durable value may describe a web page, file, lab record, or
#' database result used during research. Authenticated clients and credentials
#' remain host-owned and are never stored here. This record is not a generic
#' connection-management contract; its 0.2 role narrows to scientific source
#' and context evidence in a research workspace.
#'
#' @param resource_kind Stable resource-kind identifier such as `"web"`,
#'   `"file"`, or `"scientific.document"`.
#' @param locator Opaque locator or URI. Tempest records it but does not resolve
#'   it outside a resource-kind adapter.
#' @param title Display title.
#' @param media_type IANA media type.
#' @param resource_id Optional stable resource identifier. By default it is
#'   derived from `resource_kind` and `locator`.
#' @param content Optional inline canonical content.
#' @param storage_ref Optional opaque reference to externally stored content.
#' @param origin_connection_id Optional host connection reference identifier.
#' @param scope_metadata Serializable tenant or project scope metadata.
#'   Credential-like field names and values are rejected recursively.
#' @param content_hash Optional content checksum. Tempest computes one for
#'   inline content when omitted.
#' @param retrieved_at Retrieval timestamp.
#' @param redaction Serializable redaction metadata. Credential-like field names
#'   and values are rejected recursively.
#' @param retention Serializable retention metadata. Credential-like field names
#'   and values are rejected recursively.
#' @param metadata Serializable namespaced host metadata. Credential-like field
#'   names and values are rejected recursively.
#' @return A `tempest_resource` S7 object.
#' @keywords internal
tempest_resource <- function(
  resource_kind,
  locator,
  title,
  media_type,
  resource_id = NULL,
  content = NULL,
  storage_ref = NULL,
  origin_connection_id = NULL,
  scope_metadata = list(),
  content_hash = NULL,
  retrieved_at = NULL,
  redaction = list(),
  retention = list(),
  metadata = list()
) {
  resource_kind <- tempest_resource_safe_scalar(
    resource_kind,
    "resource_kind",
    identifier = TRUE
  )
  locator <- tempest_resource_single_line_scalar(locator, "locator")
  title <- tempest_resource_single_line_scalar(title, "title")
  media_type <- tempest_resource_safe_scalar(media_type, "media_type")
  derived_resource_id <- if (identical(resource_kind, "web")) {
    tempest_source_id(locator)
  } else {
    paste0(
      "R",
      substr(
        digest::digest(
          paste(resource_kind, locator, sep = "\n"),
          algo = "xxhash64",
          serialize = FALSE
        ),
        1L,
        16L
      )
    )
  }
  resource_id <- tempest_resource_safe_scalar(
    resource_id %||% derived_resource_id,
    "resource_id",
    identifier = TRUE
  )
  tempest_resource_assert_web_identity(
    resource_kind,
    locator,
    resource_id
  )
  content <- tempest_resource_content(content)
  storage_ref <- tempest_resource_optional_scalar(
    storage_ref,
    "storage_ref",
    identifier = TRUE
  )
  origin_connection_id <- tempest_resource_optional_scalar(
    origin_connection_id,
    "origin_connection_id",
    identifier = TRUE
  )
  scope_metadata <- tempest_resource_metadata(
    scope_metadata,
    "scope_metadata"
  )
  redaction <- tempest_resource_metadata(redaction, "redaction")
  retention <- tempest_resource_metadata(retention, "retention")
  metadata <- tempest_resource_metadata(metadata, "metadata")
  retrieved_at <- tempest_resource_safe_scalar(
    retrieved_at %||% tempest_now_utc(),
    "retrieved_at"
  )
  if (is.null(content_hash) && !is.null(content)) {
    content_hash <- tempest_product_content_hash(content, media_type)
  }
  content_hash <- tempest_resource_optional_scalar(
    content_hash,
    "content_hash",
    identifier = TRUE
  )
  tempest_resource_assert_inline_content_hash(
    content,
    media_type,
    content_hash
  )
  TempestResource(
    resource_id = resource_id,
    resource_kind = resource_kind,
    locator = locator,
    title = title,
    media_type = media_type,
    content = content,
    storage_ref = storage_ref,
    origin_connection_id = origin_connection_id,
    scope_metadata = scope_metadata,
    content_hash = content_hash,
    retrieved_at = retrieved_at,
    redaction = redaction,
    retention = retention,
    metadata = metadata,
    schema_version = 1L
  )
}

tempest_resource_data <- function(resource, include_content = TRUE) {
  if (!tempest_is_exact_resource(resource)) {
    tempest_product_validation_abort(
      paste0(
        "{.arg resource} must be an exact TempestResource record ",
        "created by {.fn tempest_resource}."
      )
    )
  }
  S7::validate(resource)
  tempest_resource_assert_inline_content_hash(
    resource@content,
    resource@media_type,
    resource@content_hash
  )
  include_content <- tempest_product_flag(include_content, "include_content")
  fields <- tempest_resource_data_fields()
  if (!include_content) {
    fields <- setdiff(fields, "content")
  }
  data <- stats::setNames(
    lapply(fields, function(field) S7::prop(resource, field)),
    fields
  )
  canonical <- list(
    resource_kind = tempest_resource_safe_scalar(
      data$resource_kind,
      "resource_kind",
      identifier = TRUE
    ),
    locator = tempest_resource_single_line_scalar(data$locator, "locator"),
    title = tempest_resource_single_line_scalar(data$title, "title"),
    media_type = tempest_resource_safe_scalar(data$media_type, "media_type"),
    resource_id = tempest_resource_safe_scalar(
      data$resource_id,
      "resource_id",
      identifier = TRUE
    ),
    retrieved_at = tempest_resource_safe_scalar(
      data$retrieved_at,
      "retrieved_at"
    ),
    storage_ref = tempest_resource_optional_scalar(
      data$storage_ref,
      "storage_ref",
      identifier = TRUE
    ),
    origin_connection_id = tempest_resource_optional_scalar(
      data$origin_connection_id,
      "origin_connection_id",
      identifier = TRUE
    ),
    content_hash = tempest_resource_optional_scalar(
      data$content_hash,
      "content_hash",
      identifier = TRUE
    )
  )
  for (field in names(canonical)) {
    data[[field]] <- tempest_resource_canonical_live_scalar(
      data[[field]],
      canonical[[field]],
      field
    )
  }
  tempest_resource_assert_web_identity(
    data$resource_kind,
    data$locator,
    data$resource_id
  )
  if (!identical(data$schema_version, 1L)) {
    tempest_product_validation_abort(
      "Resource schema_version must be exact integer 1."
    )
  }
  for (field in c("scope_metadata", "redaction", "retention", "metadata")) {
    data[[field]] <- tempest_resource_metadata(data[[field]], field)
  }
  for (field in c("storage_ref", "origin_connection_id", "content_hash")) {
    if (!is.null(data[[field]]) && is.na(data[[field]])) {
      data[field] <- list(NULL)
    }
  }
  data
}

tempest_resource_fingerprint <- function(resource_or_data) {
  data <- if (tempest_is_exact_resource(resource_or_data)) {
    tempest_resource_data(resource_or_data)
  } else {
    resource_or_data
  }
  data$fingerprint <- NULL
  tempest_product_record_hash(data)
}

tempest_resource_record <- function(resource, include_content = TRUE) {
  data <- tempest_resource_data(resource, include_content = include_content)
  data$fingerprint <- tempest_resource_fingerprint(resource)
  data
}

tempest_resource_from_data <- function(data) {
  expected_fields <- tempest_resource_record_fields()
  if (
    !is.list(data) ||
      is.data.frame(data) ||
      !identical(names(data), expected_fields)
  ) {
    tempest_product_hash_abort(
      paste0(
        "{.arg data} must be a typed evidence-resource record in exact ",
        "current field order."
      )
    )
  }
  if (!identical(data$schema_version, 1L)) {
    tempest_product_hash_abort(
      "Evidence-resource data must carry exact integer schema_version 1."
    )
  }
  metadata_fields <- c(
    "scope_metadata",
    "redaction",
    "retention",
    "metadata"
  )
  for (field in metadata_fields) {
    if (!is.list(data[[field]]) || is.data.frame(data[[field]])) {
      tempest_product_hash_abort(
        "Evidence-resource field {.field {field}} must be an explicit list."
      )
    }
  }
  expected <- tempest_product_scalar(data$fingerprint, "fingerprint")
  data$fingerprint <- NULL
  value <- tryCatch(
    tempest_resource(
      resource_kind = data$resource_kind,
      locator = data$locator,
      title = data$title,
      media_type = data$media_type,
      resource_id = data$resource_id,
      content = data$content %||% NULL,
      storage_ref = data$storage_ref %||% NULL,
      origin_connection_id = data$origin_connection_id %||% NULL,
      scope_metadata = data$scope_metadata,
      content_hash = data$content_hash %||% NULL,
      retrieved_at = data$retrieved_at,
      redaction = data$redaction,
      retention = data$retention,
      metadata = data$metadata
    ),
    error = function(error) {
      tempest_product_hash_abort(
        "Could not restore a typed evidence-resource record.",
        parent = error
      )
    }
  )
  if (!identical(tempest_resource_fingerprint(value), expected)) {
    tempest_product_hash_abort(
      "Evidence-resource fingerprint validation failed."
    )
  }
  value
}

tempest_resource_identity <- function(resource) {
  data <- tempest_resource_data(resource, include_content = FALSE)
  data$resource_id
}

tempest_resource_as_source <- function(resource) {
  data <- tempest_resource_data(resource, include_content = TRUE)
  is_web <- identical(data$resource_kind, "web")
  authoritative_meta_fields <- c(
    "resource_kind",
    "locator",
    "media_type",
    "storage_ref",
    "origin_connection_id",
    "scope_metadata",
    "redaction",
    "retention"
  )
  metadata <- data$metadata
  if (!is.null(names(metadata))) {
    metadata <- metadata[!names(metadata) %in% authoritative_meta_fields]
  }
  content_text <- if (
    is.character(data$content) &&
      length(data$content) == 1L
  ) {
    data$content
  } else {
    NA_character_
  }
  snippet <- data$metadata$snippet %||% NA_character_
  if (!is.character(snippet) || length(snippet) != 1L) {
    snippet <- NA_character_
  }
  context_text <- data$metadata$context_text %||% content_text
  if (!is.character(context_text) || length(context_text) != 1L) {
    context_text <- content_text
  }
  list(
    id = data$resource_id,
    url = if (is_web) data$locator else NA_character_,
    title = data$title,
    snippet = snippet,
    content_text = content_text,
    context_text = context_text,
    fetched_at = data$retrieved_at,
    content_hash = data$content_hash %||% NA_character_,
    meta = c(
      metadata,
      list(
        resource_kind = data$resource_kind,
        locator = data$locator,
        media_type = data$media_type,
        storage_ref = data$storage_ref %||% NA_character_,
        origin_connection_id = data$origin_connection_id %||% NA_character_,
        scope_metadata = data$scope_metadata,
        redaction = data$redaction,
        retention = data$retention
      )
    )
  )
}
