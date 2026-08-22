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

tempest_research_expert_prop_chr_vec <- function() {
  S7::new_property(S7::class_character, default = character())
}

tempest_research_expert_authored_fields <- function() {
  c(
    "name",
    "title",
    "description",
    "instructions",
    "focus_areas",
    "initial_questions"
  )
}

tempest_research_expert_fields <- function() {
  c(
    "expert_id",
    "version",
    tempest_research_expert_authored_fields(),
    "schema_version"
  )
}

TempestExpertProfile <- S7::new_class(
  "tempest_expert",
  properties = list(
    expert_id = tempest_product_prop_chr(),
    version = tempest_product_prop_chr(),
    name = tempest_product_prop_chr(),
    title = tempest_product_prop_chr(),
    description = tempest_product_prop_chr(),
    instructions = tempest_product_prop_chr(),
    focus_areas = tempest_research_expert_prop_chr_vec(),
    initial_questions = tempest_research_expert_prop_chr_vec(),
    schema_version = S7::new_property(S7::class_integer, default = 2L)
  )
)

#' Create a Tempest expert profile
#'
#' `r lifecycle::badge("experimental")`
#'
#' Expert profiles contain only human-authored scientific identity and
#' perspective. Tempest derives the exact profile identity and version from
#' those canonical fields. Runtime chats, the fixed expert model role, tools,
#' clients, roster state, and credentials are resolved separately.
#'
#' @param name Expert display name.
#' @param title Short title or area of expertise.
#' @param description Description of the expert's perspective and scope.
#' @param instructions Instructions the expert should follow.
#' @param focus_areas Character vector of focus areas.
#' @param initial_questions Optional startup research questions.
#' @return A `tempest_expert` S7 object.
#' @examples
#' expert <- tempest_expert(
#'   name = "Dr. Rivera",
#'   title = "Battery policy analyst",
#'   description = "Policy and market incentives",
#'   instructions = "Compare policy mechanisms and preserve uncertainty."
#' )
#' @export
tempest_expert <- function(
  name,
  title,
  description,
  instructions,
  focus_areas = character(),
  initial_questions = character()
) {
  name <- tempest_product_scalar(name, "name")
  title <- tempest_product_scalar(title, "title")
  description <- tempest_product_scalar(description, "description")
  instructions <- tempest_product_scalar(instructions, "instructions")
  focus_areas <- tempest_product_character(focus_areas, "focus_areas")
  initial_questions <- tempest_product_character(
    initial_questions,
    "initial_questions"
  )
  authored <- list(
    name = name,
    title = title,
    description = description,
    instructions = instructions,
    focus_areas = focus_areas,
    initial_questions = initial_questions
  )
  sensitive <- c(
    tempest_contract_sensitive_names(authored, "expert"),
    tempest_contract_sensitive_values(authored, "expert")
  )
  if (length(sensitive) > 0L) {
    tempest_research_expert_abort(c(
      "Expert profile fields cannot contain credential or secret values.",
      x = "Sensitive value: {.field {sensitive[[1]]}}."
    ))
  }
  profile_hash <- tempest_product_record_hash(authored)

  TempestExpertProfile(
    expert_id = paste0("expert::", profile_hash),
    version = paste0("sha256-", profile_hash),
    name = name,
    title = title,
    description = description,
    instructions = instructions,
    focus_areas = focus_areas,
    initial_questions = initial_questions,
    schema_version = 2L
  )
}

tempest_is_exact_expert <- function(expert) {
  identical(S7::S7_class(expert), TempestExpertProfile)
}

tempest_validate_experts <- function(experts, arg = "experts") {
  if (!is.list(experts)) {
    tempest_research_expert_abort(
      "{.arg {arg}} must be a list of {.cls tempest_expert} profiles."
    )
  }
  if (length(experts) == 0L) {
    return(experts)
  }
  valid <- vapply(experts, tempest_is_exact_expert, logical(1))
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
  invisible(lapply(experts, tempest_expert_profile_data))
  unname(experts)
}

tempest_expert_runtime_record <- function(expert) {
  tempest_validate_experts(list(expert), "expert")
  list(
    id = expert@expert_id,
    expert_id = expert@expert_id,
    version = expert@version,
    name = expert@name,
    title = expert@title,
    background = expert@description,
    focus_areas = expert@focus_areas,
    perspective = expert@description,
    instructions = expert@instructions,
    initial_questions = expert@initial_questions
  )
}

tempest_expert_update <- function(expert, ...) {
  changes <- list(...)
  authored <- tempest_research_expert_authored_fields()
  fields <- names(changes)
  if (length(changes) == 0L) {
    tempest_research_expert_abort(
      "Expert updates require at least one authored profile field."
    )
  }
  if (
    is.null(fields) ||
      anyNA(fields) ||
      any(!nzchar(fields)) ||
      anyDuplicated(fields)
  ) {
    tempest_research_expert_abort(
      "Expert update fields must be uniquely and explicitly named."
    )
  }
  derived <- intersect(fields, c("expert_id", "version", "schema_version"))
  if (length(derived) > 0L) {
    tempest_research_expert_abort(
      "Derived expert field {.field {derived[[1]]}} cannot be updated."
    )
  }
  unknown <- setdiff(fields, authored)
  if (length(unknown) > 0L) {
    tempest_research_expert_abort(
      "Unknown expert field {.field {unknown[[1]]}}."
    )
  }
  data <- tempest_expert_profile_data(expert)
  data[fields] <- changes
  do.call(tempest_expert, data[authored])
}

tempest_expert_profile_values <- function(expert) {
  if (!tempest_is_exact_expert(expert)) {
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
  if (!identical(data$schema_version, 2L)) {
    tempest_research_expert_abort(
      "Expert profiles must use exact current schema version `2`."
    )
  }
  authored <- tempest_research_expert_authored_fields()
  validated <- do.call(
    tempest_expert,
    data[authored]
  )
  validated_data <- tempest_expert_profile_values(validated)
  if (!identical(data, validated_data)) {
    tempest_research_expert_abort(
      "Expert profiles must retain exact canonical authored and derived fields."
    )
  }
  data <- validated_data
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
  array_fields <- c("focus_areas", "initial_questions")
  data[array_fields] <- lapply(data[array_fields], function(value) {
    unname(as.list(value))
  })
  data$fingerprint <- fingerprint
  data
}

tempest_expert_profile_fingerprint <- function(expert_or_data) {
  data <- if (tempest_is_exact_expert(expert_or_data)) {
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
    "instructions"
  )
  invisible(lapply(
    scalar_fields,
    function(field) tempest_research_record_string(data[[field]], field)
  ))
  if (!identical(data$schema_version, 2L)) {
    tempest_research_expert_abort(
      "Expert profile field {.field schema_version} must be exact integer `2`."
    )
  }
  array_fields <- c("focus_areas", "initial_questions")
  arrays <- stats::setNames(
    lapply(
      array_fields,
      function(field) {
        tempest_research_record_character(data[[field]], field)
      }
    ),
    array_fields
  )
  value <- tryCatch(
    tempest_expert(
      name = data$name,
      title = data$title,
      description = data$description,
      instructions = data$instructions,
      focus_areas = arrays$focus_areas,
      initial_questions = arrays$initial_questions
    ),
    error = function(error) {
      tempest_research_expert_abort(
        "Could not restore an expert profile record.",
        parent = error
      )
    }
  )
  if (
    !identical(value@expert_id, data$expert_id) ||
      !identical(value@version, data$version)
  ) {
    tempest_research_expert_abort(
      "Expert profile record identity does not match its authored fields."
    )
  }
  value
}


#' @keywords internal
tempest_expert_records <- function(experts) {
  experts <- tempest_validate_experts(experts)
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
    "initial_questions",
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
  writer_fields <- tempest_expert_record_fields()
  valid_writer_fields <- vapply(
    records,
    function(record) {
      all(vapply(
        writer_fields,
        \(field) !is.null(record[[field]]),
        logical(1)
      )) &&
        rlang::is_string(record$version) &&
        !is.na(record$version) &&
        identical(record$schema_version, 2L) &&
        is.list(record$focus_areas) &&
        !is.data.frame(record$focus_areas) &&
        is.null(names(record$focus_areas)) &&
        all(vapply(
          record$focus_areas,
          \(value) rlang::is_string(value) && !is.na(value),
          logical(1)
        )) &&
        is.list(record$initial_questions) &&
        !is.data.frame(record$initial_questions) &&
        is.null(names(record$initial_questions))
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
      tempest_validate_experts(experts)
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
