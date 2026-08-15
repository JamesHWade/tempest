# Typed evidence resources

TempestResource <- S7::new_class(
  "tempest_resource",
  properties = list(
    resource_id = tempest_workflow_prop_chr(),
    resource_kind = tempest_workflow_prop_chr(),
    locator = tempest_workflow_prop_chr(),
    title = tempest_workflow_prop_chr(),
    media_type = tempest_workflow_prop_chr(),
    content = S7::new_property(S7::class_any, default = NULL),
    storage_ref = tempest_workflow_prop_chr(NA_character_),
    origin_connection_id = tempest_workflow_prop_chr(NA_character_),
    scope_metadata = tempest_workflow_prop_list(),
    content_hash = tempest_workflow_prop_chr(NA_character_),
    retrieved_at = tempest_workflow_prop_chr(),
    redaction = tempest_workflow_prop_list(),
    retention = tempest_workflow_prop_list(),
    metadata = tempest_workflow_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

tempest_resource_optional_scalar <- function(value, arg) {
  if (
    is.null(value) ||
      (is.character(value) && length(value) == 1L && is.na(value))
  ) {
    return(NA_character_)
  }
  tempest_workflow_scalar(value, arg)
}

tempest_resource_content <- function(content) {
  if (is.null(content)) {
    return(NULL)
  }
  # Validate canonical encodability without rewriting caller-visible content.
  tryCatch(
    tempest_canonical_value(content),
    error = function(error) {
      tempest_workflow_abort(
        "{.arg content} must be canonical JSON-compatible content or a single string.",
        parent = error
      )
    }
  )
  content
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
#' @param content_hash Optional content checksum. Tempest computes one for
#'   inline content when omitted.
#' @param retrieved_at Retrieval timestamp.
#' @param redaction Serializable redaction metadata.
#' @param retention Serializable retention metadata.
#' @param metadata Serializable namespaced host metadata.
#' @param schema_version Positive resource schema version.
#' @return A `tempest_resource` S7 object.
#' @examples
#' resource <- tempest_resource(
#'   resource_kind = "scientific.document",
#'   locator = "protocols/assay-42",
#'   title = "Reviewed assay protocol",
#'   media_type = "text/plain",
#'   content = "Measure the response after 24 hours."
#' )
#' @export
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
  metadata = list(),
  schema_version = 1L
) {
  resource_kind <- tempest_workflow_scalar(
    resource_kind,
    "resource_kind"
  )
  locator <- tempest_workflow_scalar(locator, "locator")
  title <- tempest_workflow_scalar(title, "title")
  media_type <- tempest_workflow_scalar(media_type, "media_type")
  resource_id <- tempest_workflow_scalar(
    resource_id %||%
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
      ),
    "resource_id"
  )
  content <- tempest_resource_content(content)
  storage_ref <- tempest_resource_optional_scalar(storage_ref, "storage_ref")
  origin_connection_id <- tempest_resource_optional_scalar(
    origin_connection_id,
    "origin_connection_id"
  )
  scope_metadata <- tempest_workflow_serializable_list(
    scope_metadata,
    "scope_metadata"
  )
  redaction <- tempest_workflow_serializable_list(redaction, "redaction")
  retention <- tempest_workflow_serializable_list(retention, "retention")
  metadata <- tempest_workflow_serializable_list(metadata, "metadata")
  retrieved_at <- tempest_workflow_scalar(
    retrieved_at %||% tempest_now_utc(),
    "retrieved_at"
  )
  if (is.null(content_hash) && !is.null(content)) {
    content_hash <- tempest_artifact_codec_encode(content, media_type)$sha256
  }
  content_hash <- tempest_resource_optional_scalar(content_hash, "content_hash")
  if (
    !is.numeric(schema_version) ||
      length(schema_version) != 1L ||
      is.na(schema_version) ||
      schema_version < 1L ||
      schema_version != as.integer(schema_version)
  ) {
    tempest_workflow_abort(
      "{.arg schema_version} must be a positive whole number."
    )
  }

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
    schema_version = as.integer(schema_version)
  )
}

tempest_resource_data <- function(resource, include_content = TRUE) {
  if (!S7::S7_inherits(resource, TempestResource)) {
    tempest_workflow_abort(
      "{.arg resource} must be created by {.fn tempest_resource}."
    )
  }
  include_content <- tempest_workflow_flag(include_content, "include_content")
  fields <- S7::prop_names(resource)
  if (!include_content) {
    fields <- setdiff(fields, "content")
  }
  data <- stats::setNames(
    lapply(fields, function(field) S7::prop(resource, field)),
    fields
  )
  for (field in c("storage_ref", "origin_connection_id", "content_hash")) {
    if (!is.null(data[[field]]) && is.na(data[[field]])) {
      data[[field]] <- NULL
    }
  }
  data
}

tempest_resource_fingerprint <- function(resource_or_data) {
  data <- if (S7::S7_inherits(resource_or_data, TempestResource)) {
    tempest_resource_data(resource_or_data)
  } else {
    resource_or_data
  }
  data$fingerprint <- NULL
  tempest_deliverable_spec_checksum(data)
}

tempest_resource_record <- function(resource, include_content = TRUE) {
  data <- tempest_resource_data(resource, include_content = include_content)
  data$fingerprint <- tempest_resource_fingerprint(resource)
  data
}

tempest_resource_from_data <- function(data) {
  if (!is.list(data) || is.data.frame(data)) {
    tempest_artifact_codec_abort(
      "{.arg data} must be a typed evidence-resource record."
    )
  }
  expected <- tempest_workflow_scalar(data$fingerprint, "fingerprint")
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
      scope_metadata = tempest_codec_list(data$scope_metadata),
      content_hash = data$content_hash %||% NULL,
      retrieved_at = data$retrieved_at,
      redaction = tempest_codec_list(data$redaction),
      retention = tempest_codec_list(data$retention),
      metadata = tempest_codec_list(data$metadata),
      schema_version = data$schema_version %||% 1L
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a typed evidence-resource record.",
        parent = error
      )
    }
  )
  if (!identical(tempest_resource_fingerprint(value), expected)) {
    tempest_artifact_codec_abort(
      "Evidence-resource fingerprint validation failed."
    )
  }
  value
}

tempest_resource_identity <- function(resource) {
  if (S7::S7_inherits(resource, TempestResource)) {
    return(resource@resource_id)
  }
  resource$id
}

tempest_resource_as_source <- function(resource) {
  if (!S7::S7_inherits(resource, TempestResource)) {
    return(resource)
  }
  is_web <- identical(resource@resource_kind, "web")
  content_text <- if (
    is.character(resource@content) &&
      length(resource@content) == 1L
  ) {
    resource@content
  } else {
    NA_character_
  }
  list(
    id = resource@resource_id,
    url = if (is_web) resource@locator else NA_character_,
    title = resource@title,
    snippet = resource@metadata$snippet %||% NA_character_,
    content_text = content_text,
    context_text = resource@metadata$context_text %||% content_text,
    fetched_at = resource@retrieved_at,
    content_hash = resource@content_hash,
    meta = c(
      resource@metadata,
      list(
        resource_kind = resource@resource_kind,
        locator = resource@locator,
        media_type = resource@media_type,
        storage_ref = resource@storage_ref,
        origin_connection_id = resource@origin_connection_id,
        scope_metadata = resource@scope_metadata,
        redaction = resource@redaction,
        retention = resource@retention
      )
    )
  )
}

tempest_source_as_resource <- function(source) {
  source <- tempest_validate_source(source)
  content <- tempest_source_context_text(source)
  if (is.na(content)) {
    content <- NULL
  }
  metadata <- source$meta
  if (!is.na(source$snippet)) {
    metadata$snippet <- source$snippet
  }
  if (!is.na(source$content_text)) {
    metadata$content_text <- source$content_text
  }
  tempest_resource(
    resource_kind = "web",
    locator = source$url,
    title = tempest_source_scalar(source$title, source$url),
    media_type = "text/html",
    resource_id = source$id,
    content = content,
    content_hash = if (is.na(source$content_hash)) {
      NULL
    } else {
      source$content_hash
    },
    retrieved_at = tempest_source_scalar(
      source$fetched_at,
      tempest_now_utc()
    ),
    metadata = metadata
  )
}
