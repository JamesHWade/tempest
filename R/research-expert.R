# Product-owned scientific expert profiles

tempest_research_expert_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_research_expert_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_research_expert_id <- function(value, arg) {
  value <- tempest_product_scalar(value, arg)
  if (!tempest_opaque_identifier_valid(value)) {
    tempest_research_expert_abort(
      paste0(
        "{.arg {arg}} must be a bounded opaque identifier, not prose or ",
        "credentials."
      )
    )
  }
  value
}

tempest_research_expert_ids <- function(value, arg) {
  value <- tempest_product_character(value, arg)
  if (length(value) == 0L) {
    return(value)
  }
  unname(vapply(value, tempest_research_expert_id, character(1), arg = arg))
}

tempest_research_expert_optional_id <- function(value, arg) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }
  tempest_research_expert_id(value, arg)
}

tempest_research_expert_state <- function(value) {
  value <- tempest_product_scalar(value, "state")
  if (!value %in% c("active", "retired")) {
    tempest_research_expert_abort(
      "{.arg state} must be one of {.val {c('active', 'retired')}}."
    )
  }
  value
}

tempest_research_expert_canonical_list <- function(value, arg) {
  value <- tempest_product_canonical_list(value, arg)
  sensitive <- c(
    tempest_contract_sensitive_names(value, arg),
    tempest_contract_sensitive_values(value, arg)
  )
  if (length(sensitive) > 0L) {
    tempest_research_expert_abort(c(
      "{.arg {arg}} cannot contain credential or secret fields or values.",
      i = "Store authenticated material in a host connection provider.",
      x = "Sensitive field: {.field {sensitive[[1]]}}."
    ))
  }
  value
}

tempest_research_expert_versions <- function(versions, ids) {
  if (!is.character(versions) || anyNA(versions)) {
    tempest_research_expert_abort(
      paste0(
        "{.arg skill_versions} must be a named character vector ",
        "without missing values."
      )
    )
  }
  if (length(versions) == 0L) {
    return(character())
  }
  version_ids <- names(versions)
  if (
    is.null(version_ids) ||
      anyNA(version_ids) ||
      any(!nzchar(tempest_trim(version_ids))) ||
      anyDuplicated(version_ids) ||
      any(!version_ids %in% ids)
  ) {
    tempest_research_expert_abort(c(
      "{.arg skill_versions} must be named by skill id.",
      i = "Every name must identify an assigned skill."
    ))
  }
  stats::setNames(
    vapply(
      versions,
      tempest_product_version,
      character(1),
      arg = "skill_versions"
    ),
    version_ids
  )
}

tempest_research_expert_prop_chr_vec <- function() {
  S7::new_property(S7::class_character, default = character())
}

tempest_research_expert_model_roles <- function() {
  c("coordinator", "expert", "writer", "mindmap", "judge")
}

tempest_research_expert_fields <- function() {
  c(
    "expert_id",
    "version",
    "name",
    "title",
    "description",
    "instructions",
    "focus_areas",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "model_role",
    "model_policy_ref",
    "selection_metadata",
    "initial_work_items",
    "initial_questions",
    "state",
    "metadata",
    "schema_version"
  )
}

TempestExpertProfile <- S7::new_class(
  "tempest_expert",
  properties = list(
    expert_id = tempest_product_prop_chr(),
    version = tempest_product_prop_chr("1"),
    name = tempest_product_prop_chr(),
    title = tempest_product_prop_chr(),
    description = tempest_product_prop_chr(),
    instructions = tempest_product_prop_chr(),
    focus_areas = tempest_research_expert_prop_chr_vec(),
    skill_ids = tempest_research_expert_prop_chr_vec(),
    skill_versions = tempest_research_expert_prop_chr_vec(),
    required_capability_ids = tempest_research_expert_prop_chr_vec(),
    optional_capability_ids = tempest_research_expert_prop_chr_vec(),
    model_role = tempest_product_prop_chr(),
    model_policy_ref = tempest_product_prop_chr(),
    selection_metadata = tempest_product_prop_list(),
    initial_work_items = tempest_research_expert_prop_chr_vec(),
    initial_questions = tempest_research_expert_prop_chr_vec(),
    state = prop_enum(c("active", "retired"), "active"),
    metadata = tempest_product_prop_list(),
    schema_version = S7::new_property(S7::class_integer, default = 1L)
  )
)

#' Create a Tempest expert profile
#'
#' `r lifecycle::badge("experimental")`
#'
#' Expert profiles are serializable definitions of scientific identity and
#' procedure. Runtime chats, fixed role tools, clients, and credentials are
#' resolved separately for each execution context.
#'
#' @param expert_id Stable expert identifier.
#' @param name Expert display name.
#' @param title Short title or area of expertise.
#' @param description Description of the expert's perspective and scope.
#' @param instructions Instructions the expert should follow.
#' @param version Stable expert-profile version.
#' @param focus_areas Character vector of focus areas.
#' @param skill_ids,skill_versions Reserved current-schema fields. They must be
#'   empty because scientific experts use fixed product roles and tools.
#' @param required_capability_ids,optional_capability_ids Reserved
#'   current-schema fields that must be empty.
#' @param model_role One fixed scientific model role: `"coordinator"`,
#'   `"expert"`, `"writer"`, `"mindmap"`, or `"judge"`.
#' @param model_policy_ref Reserved current-schema field that must be `NA`.
#' @param selection_metadata Serializable metadata for host-side expert
#'   selection.
#' @param initial_work_items,initial_questions Optional startup work.
#' @param state Definition state, either `"active"` or `"retired"`.
#' @param metadata Canonical JSON-compatible host metadata. Metadata cannot
#'   contain credentials or executable values.
#' @return A `tempest_expert` S7 object.
#' @examples
#' expert <- tempest_expert(
#'   expert_id = "expert.battery-policy",
#'   name = "Dr. Rivera",
#'   title = "Battery policy analyst",
#'   description = "Policy and market incentives",
#'   instructions = "Compare policy mechanisms and preserve uncertainty."
#' )
#' @export
tempest_expert <- function(
  expert_id,
  name,
  title,
  description,
  instructions,
  version = "1",
  focus_areas = character(),
  skill_ids = character(),
  skill_versions = character(),
  required_capability_ids = character(),
  optional_capability_ids = character(),
  model_role = "expert",
  model_policy_ref = NA_character_,
  selection_metadata = list(),
  initial_work_items = character(),
  initial_questions = character(),
  state = "active",
  metadata = list()
) {
  expert_id <- tempest_research_expert_id(expert_id, "expert_id")
  version <- tempest_product_version(version)
  name <- tempest_product_scalar(name, "name")
  title <- tempest_product_scalar(title, "title")
  description <- tempest_product_scalar(description, "description")
  instructions <- tempest_product_scalar(instructions, "instructions")
  focus_areas <- tempest_product_character(focus_areas, "focus_areas")
  skill_ids <- tempest_research_expert_ids(skill_ids, "skill_ids")
  skill_versions <- tempest_research_expert_versions(
    skill_versions,
    skill_ids
  )
  required_capability_ids <- tempest_research_expert_ids(
    required_capability_ids,
    "required_capability_ids"
  )
  optional_capability_ids <- tempest_research_expert_ids(
    optional_capability_ids,
    "optional_capability_ids"
  )
  generic_fields <- c(
    skill_ids = length(skill_ids),
    skill_versions = length(skill_versions),
    required_capability_ids = length(required_capability_ids),
    optional_capability_ids = length(optional_capability_ids)
  )
  if (any(generic_fields > 0L)) {
    field <- names(generic_fields)[generic_fields > 0L][[1L]]
    tempest_research_expert_abort(
      "{.arg {field}} is unavailable for fixed scientific expert profiles."
    )
  }
  model_role <- tempest_research_expert_id(model_role, "model_role")
  if (!model_role %in% tempest_research_expert_model_roles()) {
    tempest_research_expert_abort(
      "{.arg model_role} must be one of {.val {tempest_research_expert_model_roles()}}."
    )
  }
  model_policy_ref <- tempest_research_expert_optional_id(
    model_policy_ref,
    "model_policy_ref"
  )
  if (!is.na(model_policy_ref)) {
    tempest_research_expert_abort(
      "{.arg model_policy_ref} is unavailable for fixed scientific expert profiles."
    )
  }
  selection_metadata <- tempest_research_expert_canonical_list(
    selection_metadata,
    "selection_metadata"
  )
  initial_work_items <- tempest_product_character(
    initial_work_items,
    "initial_work_items"
  )
  initial_questions <- tempest_product_character(
    initial_questions,
    "initial_questions"
  )
  state <- tempest_research_expert_state(state)
  metadata <- tempest_research_expert_canonical_list(metadata, "metadata")

  TempestExpertProfile(
    expert_id = expert_id,
    version = version,
    name = name,
    title = title,
    description = description,
    instructions = instructions,
    focus_areas = focus_areas,
    skill_ids = skill_ids,
    skill_versions = skill_versions,
    required_capability_ids = required_capability_ids,
    optional_capability_ids = optional_capability_ids,
    model_role = model_role,
    model_policy_ref = model_policy_ref,
    selection_metadata = selection_metadata,
    initial_work_items = initial_work_items,
    initial_questions = initial_questions,
    state = state,
    metadata = metadata,
    schema_version = 1L
  )
}

tempest_validate_experts <- function(
  experts,
  arg = "experts",
  active_only = TRUE
) {
  if (!is.list(experts)) {
    tempest_research_expert_abort(
      "{.arg {arg}} must be a list of {.cls tempest_expert} profiles."
    )
  }
  if (length(experts) == 0L) {
    return(experts)
  }
  valid <- vapply(
    experts,
    S7::S7_inherits,
    logical(1),
    class = TempestExpertProfile
  )
  if (!all(valid)) {
    tempest_research_expert_abort(
      "Every entry in {.arg {arg}} must be created by {.fn tempest_expert}."
    )
  }
  expert_ids <- vapply(experts, \(expert) expert@expert_id, character(1))
  if (anyDuplicated(expert_ids)) {
    duplicate_id <- expert_ids[duplicated(expert_ids)][[1]]
    tempest_research_expert_abort(c(
      "Expert ids in {.arg {arg}} must be unique.",
      x = "Duplicated expert id: {.val {duplicate_id}}."
    ))
  }
  if (isTRUE(active_only)) {
    retired <- vapply(
      experts,
      \(expert) identical(expert@state, "retired"),
      logical(1)
    )
    if (any(retired)) {
      tempest_research_expert_abort(c(
        "Selected experts must be active.",
        x = "Retired expert: {.val {expert_ids[retired][[1]]}}."
      ))
    }
  }
  unname(experts)
}

tempest_expert_runtime_record <- function(expert) {
  tempest_validate_experts(list(expert), "expert", active_only = FALSE)
  list(
    id = expert@expert_id,
    expert_id = expert@expert_id,
    version = expert@version,
    name = expert@name,
    title = expert@title,
    affiliation = expert@metadata$affiliation %||% NA_character_,
    background = expert@metadata$background %||% expert@description,
    focus_areas = expert@focus_areas,
    perspective = expert@description,
    instructions = expert@instructions,
    skill_ids = expert@skill_ids,
    required_capability_ids = expert@required_capability_ids,
    optional_capability_ids = expert@optional_capability_ids,
    model_role = expert@model_role,
    model_policy_ref = expert@model_policy_ref,
    initial_work_items = expert@initial_work_items,
    initial_questions = unique(c(
      expert@initial_questions,
      expert@initial_work_items
    )),
    state = expert@state,
    retired = identical(expert@state, "retired")
  )
}

tempest_expert_update <- function(expert, ...) {
  changes <- list(...)
  data <- tempest_expert_profile_data(expert)
  unknown <- setdiff(names(changes), names(data))
  if (length(unknown) > 0L) {
    tempest_research_expert_abort(
      "Unknown expert field {.field {unknown[[1]]}}."
    )
  }
  data[names(changes)] <- changes
  if (!identical(data$schema_version, 1L)) {
    tempest_research_expert_abort(
      "Expert profiles must use exact current schema version `1`."
    )
  }
  do.call(tempest_expert, data[setdiff(names(data), "schema_version")])
}

tempest_generated_expert_id <- function(value, index = 1L) {
  fingerprint <- tempest_product_record_hash(list(
    index = as.integer(index),
    expert = value
  ))
  paste0("expert.generated-", substr(fingerprint, 1L, 16L))
}

tempest_expert_profile_values <- function(expert) {
  if (!S7::S7_inherits(expert, TempestExpertProfile)) {
    tempest_research_expert_abort(
      "{.arg expert} is not a valid Tempest expert profile."
    )
  }
  stats::setNames(
    lapply(S7::prop_names(expert), function(name) S7::prop(expert, name)),
    S7::prop_names(expert)
  )
}

tempest_expert_profile_data <- function(expert) {
  data <- tempest_expert_profile_values(expert)
  if (!identical(data$schema_version, 1L)) {
    tempest_research_expert_abort(
      "Expert profiles must use exact current schema version `1`."
    )
  }
  validated <- do.call(
    tempest_expert,
    data[setdiff(names(data), "schema_version")]
  )
  data <- tempest_expert_profile_values(validated)
  sensitive <- c(
    tempest_contract_sensitive_names(data, "expert"),
    tempest_contract_sensitive_values(data, "expert")
  )
  if (length(sensitive) > 0L) {
    tempest_research_expert_abort(c(
      "{.arg expert} cannot contain credential or secret values.",
      i = "Store authenticated material in a host connection provider.",
      x = "Sensitive field: {.field {sensitive[[1]]}}."
    ))
  }
  data
}

tempest_expert_profile_record_data <- function(data, fingerprint) {
  array_fields <- c(
    "focus_areas",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "initial_work_items",
    "initial_questions"
  )
  data[array_fields] <- lapply(data[array_fields], function(value) {
    unname(as.list(value))
  })
  for (field in c("selection_metadata", "metadata")) {
    value <- data[[field]]
    if (length(value) == 0L && is.null(names(value))) {
      names(value) <- character()
    }
    value_names <- names(value)
    if (
      is.null(value_names) ||
        anyNA(value_names) ||
        any(!nzchar(value_names)) ||
        anyDuplicated(value_names)
    ) {
      tempest_research_expert_abort(
        "Expert profile record-map fields must be fully and uniquely named."
      )
    }
    data[[field]] <- tryCatch(
      tempest_product_canonical_value(value),
      error = function(error) {
        tempest_research_expert_abort(
          "Expert profile record-map fields must be canonical.",
          parent = error
        )
      }
    )
  }
  data["model_policy_ref"] <- list(NULL)
  data$fingerprint <- fingerprint
  data
}

tempest_expert_profile_fingerprint <- function(expert_or_data) {
  data <- if (S7::S7_inherits(expert_or_data, TempestExpertProfile)) {
    tempest_expert_profile_data(expert_or_data)
  } else {
    expert_or_data
  }
  data <- tempest_expert_profile_record_data(data, NULL)
  data$fingerprint <- NULL
  tempest_product_record_hash(data)
}

tempest_expert_profile_record <- function(expert) {
  tempest_expert_profile_record_data(
    tempest_expert_profile_data(expert),
    tempest_expert_profile_fingerprint(expert)
  )
}

tempest_research_record_string <- function(value, arg) {
  valid <- is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.null(attributes(value)) &&
    nzchar(value) &&
    identical(value, tempest_trim(value))
  if (!valid) {
    tempest_research_expert_abort(
      "Expert profile field {.field {arg}} must be one exact plain string."
    )
  }
  value
}

tempest_research_record_character <- function(value, arg) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !is.null(attributes(value))
  ) {
    tempest_research_expert_abort(
      "Expert profile field {.field {arg}} must be one exact unnamed array."
    )
  }
  strings <- unname(vapply(
    value,
    tempest_research_record_string,
    character(1),
    arg = arg
  ))
  if (anyDuplicated(strings)) {
    tempest_research_expert_abort(
      "Expert profile field {.field {arg}} cannot contain duplicates."
    )
  }
  strings
}

tempest_research_record_list <- function(value, arg) {
  value_names <- names(value)
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(value_names) ||
      anyNA(value_names) ||
      any(!nzchar(value_names)) ||
      anyDuplicated(value_names)
  ) {
    tempest_research_expert_abort(
      "Expert profile field {.field {arg}} must be one exact named map."
    )
  }
  canonical <- tryCatch(
    tempest_product_canonical_value(value),
    error = function(error) {
      tempest_research_expert_abort(
        "Expert profile field {.field {arg}} must be canonical.",
        parent = error
      )
    }
  )
  if (!identical(value, canonical)) {
    tempest_research_expert_abort(
      "Expert profile field {.field {arg}} must be canonical."
    )
  }
  value
}

tempest_expert_profile_from_data <- function(data) {
  if (!is.list(data) || is.data.frame(data)) {
    tempest_research_expert_abort(
      "{.arg data} must be an expert profile record."
    )
  }
  expected_fields <- c(tempest_research_expert_fields(), "fingerprint")
  fields <- names(data)
  if (
    !identical(fields, expected_fields) ||
      !identical(attributes(data), list(names = expected_fields))
  ) {
    tempest_research_expert_abort(
      "Expert profile records must contain the exact current field set."
    )
  }
  fingerprint <- data$fingerprint
  if (
    !is.character(fingerprint) ||
      length(fingerprint) != 1L ||
      is.na(fingerprint) ||
      !is.null(attributes(fingerprint)) ||
      !grepl("^[a-f0-9]{64}$", fingerprint)
  ) {
    tempest_research_expert_abort(
      "Expert profile records must include a valid fingerprint."
    )
  }
  record_data <- data[-length(data)]
  record_fingerprint <- tryCatch(
    tempest_product_record_hash(record_data),
    error = function(error) {
      tempest_research_expert_abort(
        "Expert profile record fingerprint validation failed.",
        parent = error
      )
    }
  )
  if (!identical(record_fingerprint, fingerprint)) {
    tempest_research_expert_abort(
      "Expert profile record fingerprint validation failed."
    )
  }
  scalar_fields <- c(
    "expert_id",
    "version",
    "name",
    "title",
    "description",
    "instructions",
    "model_role",
    "state"
  )
  invisible(lapply(
    scalar_fields,
    function(field) tempest_research_record_string(data[[field]], field)
  ))
  if (!identical(data$schema_version, 1L)) {
    tempest_research_expert_abort(
      "Expert profile field {.field schema_version} must be exact integer `1`."
    )
  }
  if (!is.null(data$model_policy_ref)) {
    tempest_research_expert_abort(
      "Expert profile field {.field model_policy_ref} must be exact `NULL`."
    )
  }
  array_fields <- c(
    "focus_areas",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "initial_work_items",
    "initial_questions"
  )
  arrays <- stats::setNames(
    lapply(
      array_fields,
      function(field) {
        tempest_research_record_character(data[[field]], field)
      }
    ),
    array_fields
  )
  selection_metadata <- tempest_research_record_list(
    data$selection_metadata,
    "selection_metadata"
  )
  metadata <- tempest_research_record_list(data$metadata, "metadata")
  value <- tryCatch(
    tempest_expert(
      expert_id = data$expert_id,
      name = data$name,
      title = data$title,
      description = data$description,
      instructions = data$instructions,
      version = data$version,
      focus_areas = arrays$focus_areas,
      skill_ids = arrays$skill_ids,
      skill_versions = arrays$skill_versions,
      required_capability_ids = arrays$required_capability_ids,
      optional_capability_ids = arrays$optional_capability_ids,
      model_role = data$model_role,
      model_policy_ref = NA_character_,
      selection_metadata = selection_metadata,
      initial_work_items = arrays$initial_work_items,
      initial_questions = arrays$initial_questions,
      state = data$state,
      metadata = metadata
    ),
    error = function(error) {
      tempest_research_expert_abort(
        "Could not restore an expert profile record.",
        parent = error
      )
    }
  )
  value
}


#' @keywords internal
tempest_expert_records <- function(experts) {
  experts <- tempest_validate_experts(experts, active_only = FALSE)
  unname(lapply(experts, tempest_expert_profile_record))
}

#' @keywords internal
tempest_expert_record_fields <- function() {
  c(
    "expert_id",
    "version",
    "name",
    "title",
    "description",
    "instructions",
    "focus_areas",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "model_role",
    "model_policy_ref",
    "selection_metadata",
    "initial_work_items",
    "initial_questions",
    "state",
    "metadata",
    "schema_version",
    "fingerprint"
  )
}

#' @keywords internal
tempest_experts_from_records <- function(
  records,
  what = "expert profiles",
  class = tempest_persistence_error_class()
) {
  if (!is.list(records) || is.data.frame(records)) {
    tempest_abort(
      "Cannot restore {what}; expected a list of expert-profile records.",
      class = class
    )
  }
  records <- tempest_persistence_exact_records(
    records,
    tempest_expert_record_fields(),
    what,
    class
  )
  valid_writer_fields <- vapply(
    records,
    function(record) {
      rlang::is_string(record$version) &&
        !is.na(record$version) &&
        rlang::is_string(record$state) &&
        !is.na(record$state) &&
        identical(record$schema_version, 1L) &&
        is.list(record$focus_areas) &&
        !is.data.frame(record$focus_areas) &&
        is.null(names(record$focus_areas)) &&
        all(vapply(
          record$focus_areas,
          \(value) rlang::is_string(value) && !is.na(value),
          logical(1)
        )) &&
        is.list(record$selection_metadata) &&
        !is.data.frame(record$selection_metadata) &&
        !is.null(names(record$selection_metadata)) &&
        is.list(record$metadata) &&
        !is.data.frame(record$metadata) &&
        !is.null(names(record$metadata))
    },
    logical(1)
  )
  if (!all(valid_writer_fields)) {
    tempest_abort(
      paste0(
        "Cannot restore {what}; expert-profile records must retain exact ",
        "non-null writer fields."
      ),
      class = class
    )
  }
  tryCatch(
    {
      experts <- lapply(records, tempest_expert_profile_from_data)
      tempest_validate_experts(experts, active_only = FALSE)
    },
    error = function(error) {
      tempest_abort(
        "Cannot restore {what}; an expert-profile record is invalid.",
        class = class,
        parent = error
      )
    }
  )
}

#' @keywords internal
tempest_expert_session_record_fields <- function() {
  c(
    "session_id",
    "expert_id",
    "expert_version",
    "expert_fingerprint",
    "model_role",
    "allowed_connection_ref_ids",
    "grants",
    "created_at"
  )
}

#' @keywords internal
tempest_expert_session_snapshot_record <- function(binding) {
  fields <- tempest_expert_session_record_fields()
  invalid <- !is.list(binding) ||
    is.data.frame(binding) ||
    !identical(names(binding), fields) ||
    any(vapply(fields, \(field) is.null(binding[[field]]), logical(1)))
  if (invalid) {
    tempest_abort(
      paste0(
        "A live expert-session binding must retain every exact current ",
        "writer field."
      ),
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  if (
    !is.character(binding$allowed_connection_ref_ids) ||
      !is.null(names(binding$allowed_connection_ref_ids)) ||
      length(binding$allowed_connection_ref_ids) > 0L ||
      !is.list(binding$grants) ||
      is.data.frame(binding$grants) ||
      !is.null(names(binding$grants)) ||
      length(binding$grants) > 0L
  ) {
    tempest_abort(
      "A live product expert session cannot contain generic capabilities.",
      class = tempest_session_persistence_error_class(
        "tempest_session_snapshot_error"
      )
    )
  }
  tempest_product_serializable_list(binding, "expert_session")
}

#' @keywords internal
tempest_expert_sessions_snapshot <- function(session) {
  manager <- tempest_session_expert_manager(session)
  session_ids <- sort(manager$list_sessions())
  lapply(session_ids, function(session_id) {
    tempest_expert_session_snapshot_record(
      manager$session_profile(session_id)
    )
  })
}
