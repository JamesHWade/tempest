# Serializable expert, skill, capability, and connection contracts

tempest_contract_id <- function(value, arg) {
  value <- tempest_workflow_scalar(value, arg)
  if (!tempest_opaque_identifier_valid(value)) {
    tempest_workflow_abort(
      paste0(
        "{.arg {arg}} must be a bounded opaque identifier, not prose or ",
        "credentials."
      )
    )
  }
  value
}

tempest_contract_ids <- function(value, arg) {
  value <- tempest_workflow_character(value, arg)
  if (length(value) == 0L) {
    return(value)
  }
  unname(vapply(value, tempest_contract_id, character(1), arg = arg))
}

tempest_contract_optional_id <- function(value, arg) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }
  tempest_contract_id(value, arg)
}

tempest_contract_schema_version <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      value != 1L
  ) {
    tempest_workflow_abort(
      "{.arg schema_version} must be the supported version `1`."
    )
  }
  as.integer(value)
}

tempest_contract_state <- function(value) {
  value <- tempest_workflow_scalar(value, "state")
  if (!value %in% c("active", "retired")) {
    tempest_workflow_abort(
      "{.arg state} must be one of {.val {c('active', 'retired')}}."
    )
  }
  value
}


tempest_contract_serializable_list <- function(value, arg) {
  value <- tempest_workflow_serializable_list(value, arg)
  sensitive <- c(
    tempest_contract_sensitive_names(value, arg),
    tempest_contract_sensitive_values(value, arg)
  )
  if (length(sensitive) > 0L) {
    tempest_workflow_abort(c(
      "{.arg {arg}} cannot contain credential or secret fields or values.",
      i = "Store authenticated material in a host connection provider.",
      x = "Sensitive field: {.field {sensitive[[1]]}}."
    ))
  }
  value
}

tempest_contract_operation_versions <- function(
  operation_versions,
  operation_ids
) {
  if (!is.character(operation_versions) || anyNA(operation_versions)) {
    tempest_workflow_abort(
      paste0(
        "{.arg operation_versions} must be a named character vector ",
        "without missing values."
      )
    )
  }
  if (length(operation_versions) == 0L) {
    return(character())
  }
  version_ids <- names(operation_versions)
  if (
    is.null(version_ids) ||
      anyNA(version_ids) ||
      any(!nzchar(tempest_trim(version_ids))) ||
      anyDuplicated(version_ids) ||
      any(!version_ids %in% operation_ids)
  ) {
    tempest_workflow_abort(c(
      "{.arg operation_versions} must be named by operation id.",
      i = "Every name must identify an operation in this specification."
    ))
  }
  versions <- stats::setNames(
    vapply(
      operation_versions,
      tempest_workflow_version,
      character(1),
      arg = "operation_versions"
    ),
    version_ids
  )
  versions
}

tempest_contract_prop_chr <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

tempest_contract_prop_chr_vec <- function() {
  S7::new_property(S7::class_character, default = character())
}

tempest_contract_prop_list <- function() {
  S7::new_property(S7::class_list, default = list())
}

TempestSkill <- S7::new_class(
  "tempest_skill",
  properties = list(
    skill_id = tempest_contract_prop_chr(),
    version = tempest_contract_prop_chr("1"),
    title = tempest_contract_prop_chr(),
    purpose = tempest_contract_prop_chr(),
    instructions = tempest_contract_prop_chr(),
    input_schema = tempest_contract_prop_list(),
    output_schema = tempest_contract_prop_list(),
    required_capability_ids = tempest_contract_prop_chr_vec(),
    operation_ids = tempest_contract_prop_chr_vec(),
    operation_versions = tempest_contract_prop_chr_vec(),
    state = prop_enum(c("active", "retired"), "active"),
    metadata = tempest_contract_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

TempestCapabilitySpec <- S7::new_class(
  "tempest_capability_spec",
  properties = list(
    capability_id = tempest_contract_prop_chr(),
    version = tempest_contract_prop_chr("1"),
    title = tempest_contract_prop_chr(),
    purpose = tempest_contract_prop_chr(),
    instructions = tempest_contract_prop_chr(),
    operation_id = tempest_contract_prop_chr(),
    operation_version = tempest_contract_prop_chr("1"),
    connection_ref_ids = tempest_contract_prop_chr_vec(),
    model_roles = tempest_contract_prop_chr_vec(),
    input_schema = tempest_contract_prop_list(),
    output_schema = tempest_contract_prop_list(),
    side_effecting = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    state = prop_enum(c("active", "retired"), "active"),
    metadata = tempest_contract_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

TempestConnectionRef <- S7::new_class(
  "tempest_connection_ref",
  properties = list(
    connection_id = tempest_contract_prop_chr(),
    version = tempest_contract_prop_chr("1"),
    provider_id = tempest_contract_prop_chr(),
    connection_type = tempest_contract_prop_chr(),
    title = tempest_contract_prop_chr(),
    description = tempest_contract_prop_chr(),
    scopes = tempest_contract_prop_chr_vec(),
    state = prop_enum(c("active", "retired"), "active"),
    metadata = tempest_contract_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

#' Create a Tempest skill specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' Skills are serializable procedures. They identify required capabilities and
#' runtime skill operations without storing executable functions.
#'
#' @param skill_id Stable skill identifier.
#' @param purpose Outcome the skill is intended to accomplish.
#' @param instructions Procedure an expert should follow.
#' @param version Stable skill version.
#' @param title Display title. Defaults to `skill_id`.
#' @param input_schema,output_schema Canonical JSON-compatible contracts.
#' @param required_capability_ids Capability identifiers needed by the skill.
#' @param operation_ids Runtime skill operation identifiers.
#' @param operation_versions Optional named character vector mapping operation
#'   identifiers to required versions.
#' @param state Definition state, either `"active"` or `"retired"`.
#' @param metadata Canonical JSON-compatible host metadata. Metadata cannot
#'   contain credentials or executable values.
#' @param schema_version Serializable record schema version.
#' @return A `tempest_skill` S7 object.
#' @examples
#' skill <- tempest_skill(
#'   "evidence-synthesis",
#'   purpose = "Synthesize verified evidence",
#'   instructions = "Compare sources and preserve disagreements.",
#'   required_capability_ids = "evidence.search"
#' )
#' @export
#' @noRd
tempest_skill <- function(
  skill_id,
  purpose,
  instructions,
  version = "1",
  title = skill_id,
  input_schema = list(),
  output_schema = list(),
  required_capability_ids = character(),
  operation_ids = character(),
  operation_versions = character(),
  state = "active",
  metadata = list(),
  schema_version = 1L
) {
  skill_id <- tempest_contract_id(skill_id, "skill_id")
  version <- tempest_workflow_version(version)
  title <- tempest_workflow_scalar(title, "title")
  purpose <- tempest_workflow_scalar(purpose, "purpose")
  instructions <- tempest_workflow_scalar(instructions, "instructions")
  input_schema <- tempest_contract_serializable_list(
    input_schema,
    "input_schema"
  )
  output_schema <- tempest_contract_serializable_list(
    output_schema,
    "output_schema"
  )
  required_capability_ids <- tempest_contract_ids(
    required_capability_ids,
    "required_capability_ids"
  )
  operation_ids <- tempest_contract_ids(operation_ids, "operation_ids")
  operation_versions <- tempest_contract_operation_versions(
    operation_versions,
    operation_ids
  )
  state <- tempest_contract_state(state)
  metadata <- tempest_contract_serializable_list(metadata, "metadata")
  schema_version <- tempest_contract_schema_version(schema_version)

  TempestSkill(
    skill_id = skill_id,
    version = version,
    title = title,
    purpose = purpose,
    instructions = instructions,
    input_schema = input_schema,
    output_schema = output_schema,
    required_capability_ids = required_capability_ids,
    operation_ids = operation_ids,
    operation_versions = operation_versions,
    state = state,
    metadata = metadata,
    schema_version = schema_version
  )
}

#' Create a Tempest capability specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' A capability specification declares permissioned callable behavior. Runtime
#' implementations and authenticated connections are resolved separately.
#'
#' @param capability_id Stable capability identifier.
#' @param purpose Outcome the capability supports.
#' @param instructions Usage and safety instructions.
#' @param operation_id Runtime capability operation identifier.
#' @param version Stable capability version.
#' @param title Display title. Defaults to `capability_id`.
#' @param operation_version Required runtime operation version.
#' @param connection_ref_ids Opaque connection reference identifiers required
#'   at runtime.
#' @param model_roles Model roles allowed to receive this capability. An empty
#'   vector does not restrict roles.
#' @param input_schema,output_schema Canonical JSON-compatible contracts.
#' @param side_effecting Whether the capability can change external state.
#' @param state Definition state, either `"active"` or `"retired"`.
#' @param metadata Canonical JSON-compatible host metadata. Metadata cannot
#'   contain credentials or executable values.
#' @param schema_version Serializable record schema version.
#' @return A `tempest_capability_spec` S7 object.
#' @examples
#' capability <- tempest_capability_spec(
#'   "evidence.search",
#'   purpose = "Find approved evidence",
#'   instructions = "Use only the granted connection.",
#'   operation_id = "tempest.capability.search",
#'   connection_ref_ids = "knowledge-base"
#' )
#' @export
#' @noRd
tempest_capability_spec <- function(
  capability_id,
  purpose,
  instructions,
  operation_id,
  version = "1",
  title = capability_id,
  operation_version = "1",
  connection_ref_ids = character(),
  model_roles = character(),
  input_schema = list(),
  output_schema = list(),
  side_effecting = FALSE,
  state = "active",
  metadata = list(),
  schema_version = 1L
) {
  capability_id <- tempest_contract_id(capability_id, "capability_id")
  version <- tempest_workflow_version(version)
  title <- tempest_workflow_scalar(title, "title")
  purpose <- tempest_workflow_scalar(purpose, "purpose")
  instructions <- tempest_workflow_scalar(instructions, "instructions")
  operation_id <- tempest_contract_id(operation_id, "operation_id")
  operation_version <- tempest_workflow_version(
    operation_version,
    "operation_version"
  )
  connection_ref_ids <- tempest_contract_ids(
    connection_ref_ids,
    "connection_ref_ids"
  )
  model_roles <- tempest_contract_ids(model_roles, "model_roles")
  input_schema <- tempest_contract_serializable_list(
    input_schema,
    "input_schema"
  )
  output_schema <- tempest_contract_serializable_list(
    output_schema,
    "output_schema"
  )
  side_effecting <- tempest_workflow_flag(side_effecting, "side_effecting")
  state <- tempest_contract_state(state)
  metadata <- tempest_contract_serializable_list(metadata, "metadata")
  schema_version <- tempest_contract_schema_version(schema_version)

  TempestCapabilitySpec(
    capability_id = capability_id,
    version = version,
    title = title,
    purpose = purpose,
    instructions = instructions,
    operation_id = operation_id,
    operation_version = operation_version,
    connection_ref_ids = connection_ref_ids,
    model_roles = model_roles,
    input_schema = input_schema,
    output_schema = output_schema,
    side_effecting = side_effecting,
    state = state,
    metadata = metadata,
    schema_version = schema_version
  )
}

#' Create an opaque Tempest connection reference
#'
#' `r lifecycle::badge("experimental")`
#'
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned; see
#' [tempest-generic-kernel-retirement].
#'
#' A connection reference identifies a host-owned authenticated binding without
#' storing credentials or a live client in a durable workflow definition.
#'
#' @param connection_id Stable, opaque connection identifier.
#' @param provider_id Host connection-provider identifier.
#' @param connection_type Host-defined connection type.
#' @param title Display title.
#' @param description Non-secret description of the connection's purpose.
#' @param version Stable reference version.
#' @param scopes Non-secret scope labels used for capability resolution.
#' @param state Reference state, either `"active"` or `"retired"`.
#' @param metadata Canonical JSON-compatible non-secret metadata.
#' @param schema_version Serializable record schema version.
#' @return A `tempest_connection_ref` S7 object.
#' @examples
#' connection <- tempest_connection_ref(
#'   "knowledge-base",
#'   provider_id = "host.connections",
#'   connection_type = "document-search",
#'   title = "Approved knowledge base",
#'   description = "Read-only customer documentation"
#' )
#' @export
#' @noRd
tempest_connection_ref <- function(
  connection_id,
  provider_id,
  connection_type,
  title,
  description,
  version = "1",
  scopes = character(),
  state = "active",
  metadata = list(),
  schema_version = 1L
) {
  connection_id <- tempest_contract_id(connection_id, "connection_id")
  version <- tempest_workflow_version(version)
  provider_id <- tempest_contract_id(provider_id, "provider_id")
  connection_type <- tempest_contract_id(
    connection_type,
    "connection_type"
  )
  title <- tempest_workflow_scalar(title, "title")
  description <- tempest_workflow_scalar(description, "description")
  scopes <- tempest_contract_ids(scopes, "scopes")
  state <- tempest_contract_state(state)
  metadata <- tempest_contract_serializable_list(metadata, "metadata")
  schema_version <- tempest_contract_schema_version(schema_version)

  TempestConnectionRef(
    connection_id = connection_id,
    version = version,
    provider_id = provider_id,
    connection_type = connection_type,
    title = title,
    description = description,
    scopes = scopes,
    state = state,
    metadata = metadata,
    schema_version = schema_version
  )
}

tempest_contract_data <- function(value, class, arg) {
  if (!S7::S7_inherits(value, class)) {
    tempest_workflow_abort(
      "{.arg {arg}} is not a valid Tempest contract."
    )
  }
  stats::setNames(
    lapply(S7::prop_names(value), function(name) S7::prop(value, name)),
    S7::prop_names(value)
  )
}

tempest_contract_revalidated_data <- function(value, class, arg, constructor) {
  data <- tempest_contract_data(value, class, arg)
  validated <- do.call(constructor, data)
  data <- tempest_contract_data(validated, class, arg)
  sensitive <- c(
    tempest_contract_sensitive_names(data, arg),
    tempest_contract_sensitive_values(data, arg)
  )
  if (length(sensitive) > 0L) {
    tempest_workflow_abort(c(
      "{.arg {arg}} cannot contain credential or secret values.",
      i = "Store authenticated material in a host connection provider.",
      x = "Sensitive field: {.field {sensitive[[1]]}}."
    ))
  }
  data
}

tempest_contract_record <- function(data, fingerprint) {
  for (field in c("model_role", "model_policy_ref")) {
    if (!is.null(data[[field]]) && is.na(data[[field]])) {
      data[field] <- list(NULL)
    }
  }
  for (field in c("operation_versions", "skill_versions")) {
    if (!is.null(data[[field]]) && is.atomic(data[[field]])) {
      data[[field]] <- as.list(data[[field]])
    }
  }
  data$fingerprint <- fingerprint
  data
}

tempest_contract_checksum <- function(data) {
  data$fingerprint <- NULL
  tempest_deliverable_spec_checksum(data)
}

tempest_skill_data <- function(skill) {
  tempest_contract_revalidated_data(
    skill,
    TempestSkill,
    "skill",
    tempest_skill
  )
}

tempest_skill_fingerprint <- function(skill_or_data) {
  data <- if (S7::S7_inherits(skill_or_data, TempestSkill)) {
    tempest_skill_data(skill_or_data)
  } else {
    skill_or_data
  }
  tempest_contract_checksum(tempest_contract_record(data, NULL))
}

tempest_skill_record <- function(skill) {
  tempest_contract_record(
    tempest_skill_data(skill),
    tempest_skill_fingerprint(skill)
  )
}

tempest_capability_spec_data <- function(capability) {
  tempest_contract_revalidated_data(
    capability,
    TempestCapabilitySpec,
    "capability",
    tempest_capability_spec
  )
}

tempest_capability_spec_fingerprint <- function(capability_or_data) {
  data <- if (S7::S7_inherits(capability_or_data, TempestCapabilitySpec)) {
    tempest_capability_spec_data(capability_or_data)
  } else {
    capability_or_data
  }
  tempest_contract_checksum(tempest_contract_record(data, NULL))
}

tempest_capability_spec_record <- function(capability) {
  tempest_contract_record(
    tempest_capability_spec_data(capability),
    tempest_capability_spec_fingerprint(capability)
  )
}

tempest_connection_ref_data <- function(connection) {
  tempest_contract_revalidated_data(
    connection,
    TempestConnectionRef,
    "connection",
    tempest_connection_ref
  )
}

tempest_connection_ref_fingerprint <- function(connection_or_data) {
  data <- if (S7::S7_inherits(connection_or_data, TempestConnectionRef)) {
    tempest_connection_ref_data(connection_or_data)
  } else {
    connection_or_data
  }
  tempest_contract_checksum(tempest_contract_record(data, NULL))
}

tempest_connection_ref_record <- function(connection) {
  tempest_contract_record(
    tempest_connection_ref_data(connection),
    tempest_connection_ref_fingerprint(connection)
  )
}

tempest_contract_restore_data <- function(data, type) {
  if (!is.list(data) || is.data.frame(data)) {
    tempest_artifact_codec_abort(
      "{.arg data} must be a {.val {type}} record."
    )
  }
  fingerprint <- data$fingerprint %||% NULL
  if (
    !is.character(fingerprint) ||
      length(fingerprint) != 1L ||
      is.na(fingerprint) ||
      !grepl("^[a-f0-9]{64}$", fingerprint)
  ) {
    tempest_artifact_codec_abort(
      "{.val {type}} records must include a valid fingerprint."
    )
  }
  data$fingerprint <- NULL
  list(data = data, fingerprint = fingerprint)
}

tempest_contract_verify_restored <- function(
  value,
  expected_fingerprint,
  fingerprint,
  type
) {
  if (!identical(fingerprint(value), expected_fingerprint)) {
    tempest_artifact_codec_abort(
      "{.val {type}} record fingerprint validation failed."
    )
  }
  value
}

tempest_skill_from_data <- function(data) {
  restored <- tempest_contract_restore_data(data, "skill")
  value <- tryCatch(
    tempest_skill(
      skill_id = restored$data$skill_id,
      purpose = restored$data$purpose,
      instructions = restored$data$instructions,
      version = restored$data$version %||% "1",
      title = restored$data$title,
      input_schema = tempest_codec_list(restored$data$input_schema),
      output_schema = tempest_codec_list(restored$data$output_schema),
      required_capability_ids = tempest_codec_character(
        restored$data$required_capability_ids
      ),
      operation_ids = tempest_codec_character(
        restored$data$operation_ids
      ),
      operation_versions = tempest_codec_character(
        restored$data$operation_versions
      ),
      state = restored$data$state %||% "active",
      metadata = tempest_codec_list(restored$data$metadata),
      schema_version = restored$data$schema_version %||% 1L
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a skill record.",
        parent = error
      )
    }
  )
  tempest_contract_verify_restored(
    value,
    restored$fingerprint,
    tempest_skill_fingerprint,
    "Skill"
  )
}

tempest_capability_spec_from_data <- function(data) {
  restored <- tempest_contract_restore_data(data, "capability")
  value <- tryCatch(
    tempest_capability_spec(
      capability_id = restored$data$capability_id,
      purpose = restored$data$purpose,
      instructions = restored$data$instructions,
      operation_id = restored$data$operation_id,
      version = restored$data$version %||% "1",
      title = restored$data$title,
      operation_version = restored$data$operation_version %||% "1",
      connection_ref_ids = tempest_codec_character(
        restored$data$connection_ref_ids
      ),
      model_roles = tempest_codec_character(restored$data$model_roles),
      input_schema = tempest_codec_list(restored$data$input_schema),
      output_schema = tempest_codec_list(restored$data$output_schema),
      side_effecting = isTRUE(restored$data$side_effecting),
      state = restored$data$state %||% "active",
      metadata = tempest_codec_list(restored$data$metadata),
      schema_version = restored$data$schema_version %||% 1L
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a capability specification record.",
        parent = error
      )
    }
  )
  tempest_contract_verify_restored(
    value,
    restored$fingerprint,
    tempest_capability_spec_fingerprint,
    "Capability"
  )
}

tempest_connection_ref_from_data <- function(data) {
  restored <- tempest_contract_restore_data(data, "connection reference")
  value <- tryCatch(
    tempest_connection_ref(
      connection_id = restored$data$connection_id,
      provider_id = restored$data$provider_id,
      connection_type = restored$data$connection_type,
      title = restored$data$title,
      description = restored$data$description,
      version = restored$data$version %||% "1",
      scopes = tempest_codec_character(restored$data$scopes),
      state = restored$data$state %||% "active",
      metadata = tempest_codec_list(restored$data$metadata),
      schema_version = restored$data$schema_version %||% 1L
    ),
    error = function(error) {
      tempest_artifact_codec_abort(
        "Could not restore a connection reference record.",
        parent = error
      )
    }
  )
  tempest_contract_verify_restored(
    value,
    restored$fingerprint,
    tempest_connection_ref_fingerprint,
    "Connection reference"
  )
}
