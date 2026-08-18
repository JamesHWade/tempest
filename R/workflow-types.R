# Application-neutral workflow value contracts

tempest_workflow_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_workflow_spec_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_workflow_scalar <- function(
  value,
  arg,
  allow_na = FALSE,
  default = NULL
) {
  if (is.null(value) && !is.null(default)) {
    value <- default
  }
  valid <- is.character(value) && length(value) == 1L
  if (valid && is.na(value)) {
    valid <- isTRUE(allow_na)
  }
  if (valid && !is.na(value)) {
    value <- tempest_trim(value)
    valid <- nzchar(value)
  }
  if (!valid) {
    tempest_workflow_abort(
      "{.arg {arg}} must be a single non-empty string."
    )
  }
  value
}

tempest_workflow_character <- function(value, arg) {
  value <- value %||% character()
  if (!is.character(value) || anyNA(value)) {
    tempest_workflow_abort(
      "{.arg {arg}} must be a character vector without missing values."
    )
  }
  value <- unique(tempest_trim(value))
  if (any(!nzchar(value))) {
    tempest_workflow_abort(
      "{.arg {arg}} cannot contain empty strings."
    )
  }
  value
}

tempest_workflow_list <- function(value, arg) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_workflow_abort("{.arg {arg}} must be a list.")
  }
  value
}

tempest_workflow_serializable_list <- function(value, arg) {
  value <- tempest_workflow_list(value, arg)
  tryCatch(
    tempest_canonical_json(value),
    error = function(error) {
      tempest_workflow_abort(
        "{.arg {arg}} must contain only canonical JSON-compatible values.",
        parent = error
      )
    }
  )
  value
}

tempest_workflow_flag <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    tempest_workflow_abort("{.arg {arg}} must be `TRUE` or `FALSE`.")
  }
  value
}

tempest_workflow_version <- function(value, arg = "version") {
  value <- tempest_workflow_scalar(value, arg)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._+-]*$", value)) {
    tempest_workflow_abort(
      "{.arg {arg}} must contain only letters, numbers, `.`, `_`, `+`, or `-`."
    )
  }
  value
}

tempest_workflow_prop_chr <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

tempest_workflow_prop_chr_vec <- function() {
  S7::new_property(S7::class_character, default = character())
}

tempest_workflow_prop_list <- function() {
  S7::new_property(S7::class_list, default = list())
}

TempestObjective <- S7::new_class(
  "tempest_objective",
  properties = list(
    objective_id = tempest_workflow_prop_chr(),
    title = tempest_workflow_prop_chr(),
    description = tempest_workflow_prop_chr(),
    context = tempest_workflow_prop_list(),
    constraints = tempest_workflow_prop_chr_vec(),
    acceptance_criteria = tempest_workflow_prop_chr_vec(),
    input_resource_ids = tempest_workflow_prop_chr_vec(),
    deliverable_ids = tempest_workflow_prop_chr_vec(),
    metadata = tempest_workflow_prop_list(),
    created_at = tempest_workflow_prop_chr(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

TempestDeliverableSpec <- S7::new_class(
  "tempest_deliverable_spec",
  properties = list(
    deliverable_id = tempest_workflow_prop_chr(),
    version = tempest_workflow_prop_chr("1"),
    title = tempest_workflow_prop_chr(),
    purpose = tempest_workflow_prop_chr(),
    instructions = tempest_workflow_prop_chr(),
    content_schema = tempest_workflow_prop_list(),
    required_fields = tempest_workflow_prop_chr_vec(),
    evidence_policy = prop_enum(
      c("none", "source_attributed", "claim_verified", "strict"),
      "source_attributed"
    ),
    generator_id = tempest_workflow_prop_chr(),
    validator_ids = tempest_workflow_prop_chr_vec(),
    renderer_ids = tempest_workflow_prop_chr_vec(),
    exporter_ids = tempest_workflow_prop_chr_vec(),
    operation_versions = tempest_workflow_prop_chr_vec(),
    content_type = tempest_workflow_prop_chr("text"),
    media_types = tempest_workflow_prop_chr_vec(),
    filename_policy = tempest_workflow_prop_list(),
    requires_approval = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    metadata = tempest_workflow_prop_list()
  )
)

TempestArtifact <- S7::new_class(
  "tempest_artifact",
  properties = list(
    artifact_id = tempest_workflow_prop_chr(),
    deliverable_id = tempest_workflow_prop_chr(),
    deliverable_version = tempest_workflow_prop_chr("1"),
    spec_fingerprint = tempest_workflow_prop_chr(),
    artifact_kind = tempest_workflow_prop_chr("primary"),
    media_type = tempest_workflow_prop_chr(),
    schema_version = S7::new_property(S7::class_integer, default = 1L),
    content = S7::new_property(S7::class_any, default = NULL),
    storage_ref = tempest_workflow_prop_chr(NA_character_),
    producer_operation_id = tempest_workflow_prop_chr(NA_character_),
    run_id = tempest_workflow_prop_chr(NA_character_),
    step_id = tempest_workflow_prop_chr(NA_character_),
    expert_id = tempest_workflow_prop_chr(NA_character_),
    resource_ids = tempest_workflow_prop_chr_vec(),
    claim_ids = tempest_workflow_prop_chr_vec(),
    evidence_span_ids = tempest_workflow_prop_chr_vec(),
    parent_artifact_ids = tempest_workflow_prop_chr_vec(),
    validation_results = tempest_workflow_prop_list(),
    status = prop_enum(
      c(
        "draft",
        "valid",
        "invalid",
        "awaiting_approval",
        "approved",
        "rejected"
      ),
      "draft"
    ),
    checksum = tempest_workflow_prop_chr(),
    created_at = tempest_workflow_prop_chr(),
    updated_at = tempest_workflow_prop_chr(),
    metadata = tempest_workflow_prop_list()
  )
)

#' Create a Tempest objective
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' An objective describes an application-neutral requested outcome, its
#' constraints, approved inputs, completion criteria, and requested
#' deliverables.
#'
#' @param description Requested outcome.
#' @param title Short display title. Defaults to `description`.
#' @param objective_id Optional stable identifier.
#' @param context Serializable host-provided context.
#' @param constraints Character vector of requirements and exclusions.
#' @param acceptance_criteria Character vector of observable completion
#'   conditions.
#' @param input_resource_ids Approved input resource identifiers.
#' @param deliverable_ids Requested deliverable specification identifiers.
#' @param metadata Serializable, namespaced host metadata.
#' @param created_at Optional creation timestamp.
#' @param schema_version Positive objective schema version.
#' @return A `tempest_objective` S7 object.
#' @examples
#' objective <- tempest_objective(
#'   "Prepare an evidence-backed response",
#'   acceptance_criteria = "Every recommendation cites supporting evidence",
#'   deliverable_ids = "customer-response"
#' )
#' @export
#' @noRd
tempest_objective <- function(
  description,
  title = description,
  objective_id = NULL,
  context = list(),
  constraints = character(),
  acceptance_criteria = character(),
  input_resource_ids = character(),
  deliverable_ids = character(),
  metadata = list(),
  created_at = NULL,
  schema_version = 1L
) {
  description <- tempest_workflow_scalar(description, "description")
  title <- tempest_workflow_scalar(title, "title")
  objective_id <- tempest_workflow_scalar(
    objective_id %||% tempest_uuid("objective"),
    "objective_id"
  )
  context <- tempest_workflow_serializable_list(context, "context")
  constraints <- tempest_workflow_character(constraints, "constraints")
  acceptance_criteria <- tempest_workflow_character(
    acceptance_criteria,
    "acceptance_criteria"
  )
  input_resource_ids <- tempest_workflow_character(
    input_resource_ids,
    "input_resource_ids"
  )
  deliverable_ids <- tempest_workflow_character(
    deliverable_ids,
    "deliverable_ids"
  )
  metadata <- tempest_workflow_serializable_list(metadata, "metadata")
  created_at <- tempest_workflow_scalar(
    created_at %||% tempest_now_utc(),
    "created_at"
  )
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

  TempestObjective(
    objective_id = objective_id,
    title = title,
    description = description,
    context = context,
    constraints = constraints,
    acceptance_criteria = acceptance_criteria,
    input_resource_ids = input_resource_ids,
    deliverable_ids = deliverable_ids,
    metadata = metadata,
    created_at = created_at,
    schema_version = as.integer(schema_version)
  )
}

#' Create a Tempest deliverable specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' A deliverable specification separates serializable output requirements from
#' runtime generator, validator, renderer, and exporter implementations.
#'
#' @param deliverable_id Stable specification identifier.
#' @param title Display title.
#' @param purpose What the deliverable is intended to accomplish.
#' @param instructions Generation instructions.
#' @param version Stable specification version.
#' @param content_schema Serializable canonical JSON content schema. Tempest
#'   records this contract but enforces it only through validators named in
#'   `validator_ids`.
#' @param required_fields Required content fields or sections.
#' @param evidence_policy Evidence policy.
#' @param generator_id Runtime generator operation identifier.
#' @param validator_ids Runtime validator operation identifiers.
#' @param renderer_ids Runtime renderer operation identifiers.
#' @param exporter_ids Runtime exporter operation identifiers.
#' @param operation_versions Optional named character vector mapping operation
#'   identifiers to required versions.
#' @param content_type Canonical content type.
#' @param media_types Artifact media types this specification may produce.
#' @param filename_policy Serializable filename policy.
#' @param requires_approval Whether output requires approval.
#' @param metadata Serializable host metadata.
#' @return A `tempest_deliverable_spec` S7 object.
#' @examples
#' spec <- tempest_deliverable_spec(
#'   "customer-response",
#'   title = "Customer response",
#'   purpose = "Answer the customer's request with evidence",
#'   instructions = "Be concise and preserve uncertainty.",
#'   required_fields = c("response", "risks"),
#'   generator_id = "tempest.generator.provided_content",
#'   renderer_ids = "tempest.renderer.markdown"
#' )
#' @export
#' @noRd
tempest_deliverable_spec <- function(
  deliverable_id,
  title,
  purpose,
  instructions,
  version = "1",
  content_schema = list(),
  required_fields = character(),
  evidence_policy = "source_attributed",
  generator_id,
  validator_ids = character(),
  renderer_ids,
  exporter_ids = character(),
  operation_versions = character(),
  content_type = "text",
  media_types = "text/markdown",
  filename_policy = list(),
  requires_approval = FALSE,
  metadata = list()
) {
  deliverable_id <- tempest_workflow_scalar(
    deliverable_id,
    "deliverable_id"
  )
  version <- tempest_workflow_version(version)
  title <- tempest_workflow_scalar(title, "title")
  purpose <- tempest_workflow_scalar(purpose, "purpose")
  instructions <- tempest_workflow_scalar(instructions, "instructions")
  content_schema <- tempest_workflow_serializable_list(
    content_schema,
    "content_schema"
  )
  required_fields <- tempest_workflow_character(
    required_fields,
    "required_fields"
  )
  evidence_policy <- tempest_workflow_scalar(
    evidence_policy,
    "evidence_policy"
  )
  if (
    !evidence_policy %in%
      c("none", "source_attributed", "claim_verified", "strict")
  ) {
    tempest_workflow_abort(
      "{.arg evidence_policy} must be one of {.val {c('none', 'source_attributed', 'claim_verified', 'strict')}}."
    )
  }
  generator_id <- tempest_workflow_scalar(generator_id, "generator_id")
  validator_ids <- tempest_workflow_character(
    validator_ids,
    "validator_ids"
  )
  renderer_ids <- tempest_workflow_character(renderer_ids, "renderer_ids")
  if (length(renderer_ids) == 0L) {
    tempest_workflow_abort(
      "{.arg renderer_ids} must contain at least one operation id."
    )
  }
  exporter_ids <- tempest_workflow_character(exporter_ids, "exporter_ids")
  if (!is.character(operation_versions) || anyNA(operation_versions)) {
    tempest_workflow_abort(
      "{.arg operation_versions} must be a named character vector without missing values."
    )
  }
  version_ids <- names(operation_versions)
  operation_versions <- tempest_trim(operation_versions)
  names(operation_versions) <- version_ids
  if (any(!nzchar(operation_versions))) {
    tempest_workflow_abort(
      "{.arg operation_versions} cannot contain empty versions."
    )
  }
  operation_ids <- c(
    generator_id,
    validator_ids,
    renderer_ids,
    exporter_ids
  )
  if (length(operation_versions) > 0L) {
    if (
      is.null(version_ids) ||
        anyNA(version_ids) ||
        any(!nzchar(tempest_trim(version_ids))) ||
        anyDuplicated(version_ids) ||
        any(!version_ids %in% operation_ids)
    ) {
      tempest_workflow_abort(
        c(
          "{.arg operation_versions} must be named by operation id.",
          i = "Every name must identify an operation in this specification."
        )
      )
    }
    operation_versions <- stats::setNames(
      vapply(
        operation_versions,
        tempest_workflow_version,
        character(1),
        arg = "operation_versions"
      ),
      version_ids
    )
  }
  content_type <- tempest_workflow_scalar(content_type, "content_type")
  media_types <- tempest_workflow_character(media_types, "media_types")
  if (length(media_types) == 0L) {
    tempest_workflow_abort(
      "{.arg media_types} must contain at least one media type."
    )
  }
  filename_policy <- tempest_workflow_serializable_list(
    filename_policy,
    "filename_policy"
  )
  requires_approval <- tempest_workflow_flag(
    requires_approval,
    "requires_approval"
  )
  metadata <- tempest_workflow_serializable_list(metadata, "metadata")

  TempestDeliverableSpec(
    deliverable_id = deliverable_id,
    version = version,
    title = title,
    purpose = purpose,
    instructions = instructions,
    content_schema = content_schema,
    required_fields = required_fields,
    evidence_policy = evidence_policy,
    generator_id = generator_id,
    validator_ids = validator_ids,
    renderer_ids = renderer_ids,
    exporter_ids = exporter_ids,
    operation_versions = operation_versions,
    content_type = content_type,
    media_types = media_types,
    filename_policy = filename_policy,
    requires_approval = requires_approval,
    metadata = metadata
  )
}

tempest_deliverable_spec_data <- function(spec) {
  if (!S7::S7_inherits(spec, TempestDeliverableSpec)) {
    tempest_workflow_abort(
      "{.arg spec} must be created by {.fn tempest_deliverable_spec}."
    )
  }
  stats::setNames(
    lapply(S7::prop_names(spec), function(name) S7::prop(spec, name)),
    S7::prop_names(spec)
  )
}

tempest_deliverable_fingerprint <- function(spec) {
  tempest_deliverable_spec_checksum(spec)
}

#' Create a typed Tempest artifact
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' @param deliverable A `tempest_deliverable_spec` object.
#' @param content Inline artifact content: a single UTF-8 string or canonical
#'   JSON-compatible lists and atomic values. JSON content restores with JSON
#'   object and array semantics; use `storage_ref` for other representations.
#' @param storage_ref Optional external storage reference.
#' @param artifact_id Optional stable artifact identifier.
#' @param artifact_kind Artifact role within the deliverable.
#' @param media_type Artifact media type.
#' @param schema_version Positive artifact schema version.
#' @param producer_operation_id Producing operation identifier.
#' @param run_id,step_id,expert_id Optional provenance identifiers.
#' @param resource_ids,claim_ids,evidence_span_ids Evidence identifiers.
#' @param parent_artifact_ids Parent artifact identifiers.
#' @param validation_results Validation result objects.
#' @param status Artifact lifecycle status.
#' @param checksum Optional content checksum.
#' @param created_at,updated_at Optional timestamps.
#' @param metadata Serializable metadata.
#' @return A `tempest_artifact` S7 object.
#' @examples
#' spec <- tempest_deliverable_spec(
#'   "brief",
#'   title = "Brief",
#'   purpose = "Summarize findings",
#'   instructions = "Use verified evidence.",
#'   generator_id = "tempest.generator.provided_content",
#'   renderer_ids = "tempest.renderer.markdown"
#' )
#' artifact <- tempest_artifact(spec, content = "# Brief")
#' @export
#' @noRd
tempest_artifact <- function(
  deliverable,
  content = NULL,
  storage_ref = NA_character_,
  artifact_id = NULL,
  artifact_kind = "primary",
  media_type = NULL,
  schema_version = 1L,
  producer_operation_id = NA_character_,
  run_id = NA_character_,
  step_id = NA_character_,
  expert_id = NA_character_,
  resource_ids = character(),
  claim_ids = character(),
  evidence_span_ids = character(),
  parent_artifact_ids = character(),
  validation_results = list(),
  status = c(
    "draft",
    "valid",
    "invalid",
    "awaiting_approval",
    "approved",
    "rejected"
  ),
  checksum = NULL,
  created_at = NULL,
  updated_at = created_at,
  metadata = list()
) {
  if (!S7::S7_inherits(deliverable, TempestDeliverableSpec)) {
    tempest_workflow_abort(
      "{.arg deliverable} must be created by {.fn tempest_deliverable_spec}."
    )
  }
  storage_ref <- if (is.null(storage_ref)) NA_character_ else storage_ref
  if (
    !is.character(storage_ref) ||
      length(storage_ref) != 1L ||
      (!is.na(storage_ref) && !nzchar(tempest_trim(storage_ref)))
  ) {
    tempest_workflow_abort(
      "{.arg storage_ref} must be a non-empty string or `NA`."
    )
  }
  if (is.null(content) && is.na(storage_ref)) {
    tempest_workflow_abort(
      "An artifact must contain {.arg content} or a {.arg storage_ref}."
    )
  }
  artifact_id <- tempest_workflow_scalar(
    artifact_id %||% tempest_uuid("artifact"),
    "artifact_id"
  )
  artifact_kind <- tempest_workflow_scalar(
    artifact_kind,
    "artifact_kind"
  )
  media_type <- tempest_workflow_scalar(
    media_type %||% deliverable@media_types[[1]],
    "media_type"
  )
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
  optional_ids <- list(
    producer_operation_id = producer_operation_id,
    run_id = run_id,
    step_id = step_id,
    expert_id = expert_id
  )
  optional_ids <- lapply(names(optional_ids), function(name) {
    value <- optional_ids[[name]]
    if (is.null(value) || is.na(value)) {
      return(NA_character_)
    }
    tempest_workflow_scalar(value, name)
  }) |>
    stats::setNames(names(optional_ids))
  resource_ids <- tempest_workflow_character(resource_ids, "resource_ids")
  claim_ids <- tempest_workflow_character(claim_ids, "claim_ids")
  evidence_span_ids <- tempest_workflow_character(
    evidence_span_ids,
    "evidence_span_ids"
  )
  parent_artifact_ids <- tempest_workflow_character(
    parent_artifact_ids,
    "parent_artifact_ids"
  )
  validation_results <- tempest_validation_results(validation_results)
  status <- match.arg(status)
  metadata <- tempest_workflow_serializable_list(metadata, "metadata")
  checksum <- tempest_workflow_scalar(
    checksum %||%
      tempest_artifact_content_checksum(
        content,
        storage_ref,
        media_type
      ),
    "checksum"
  )
  created_at <- tempest_workflow_scalar(
    created_at %||% tempest_now_utc(),
    "created_at"
  )
  updated_at <- tempest_workflow_scalar(
    updated_at %||% created_at,
    "updated_at"
  )

  TempestArtifact(
    artifact_id = artifact_id,
    deliverable_id = deliverable@deliverable_id,
    deliverable_version = deliverable@version,
    spec_fingerprint = tempest_deliverable_fingerprint(deliverable),
    artifact_kind = artifact_kind,
    media_type = media_type,
    schema_version = as.integer(schema_version),
    content = content,
    storage_ref = storage_ref,
    producer_operation_id = optional_ids$producer_operation_id,
    run_id = optional_ids$run_id,
    step_id = optional_ids$step_id,
    expert_id = optional_ids$expert_id,
    resource_ids = resource_ids,
    claim_ids = claim_ids,
    evidence_span_ids = evidence_span_ids,
    parent_artifact_ids = parent_artifact_ids,
    validation_results = validation_results,
    status = status,
    checksum = checksum,
    created_at = created_at,
    updated_at = updated_at,
    metadata = metadata
  )
}
