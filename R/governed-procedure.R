# Inert governed-procedure provenance for retained research inspection

tempest_governed_procedure_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_governed_procedure_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_governed_procedure_fields <- function() {
  c(
    "stage",
    "tempest_governed_procedure_id",
    "record_id",
    "revision_id",
    "program_artifact_id",
    "contract_version",
    "evaluator_id",
    "evaluator_version",
    "store_id",
    "snapshot_id",
    "schema_build_digest",
    "commit_order"
  )
}

tempest_governed_procedure_string <- function(value, path) {
  tryCatch(
    tempest_research_manifest_id(value, path),
    error = function(error) {
      tempest_governed_procedure_abort(
        "{.field {path}} must be a bounded credential-free identifier."
      )
    }
  )
}

tempest_governed_procedure_commit_order <- function(value, path) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 1 ||
      value != trunc(value) ||
      value >= 2^53
  ) {
    tempest_governed_procedure_abort(
      "{.field {path}} must be a positive whole-number commit order."
    )
  }
  as.double(value)
}

tempest_governed_procedure_contract_version <- function(value, path) {
  if (!tempest_exact_integer_scalar_valid(value, 1L, 1L)) {
    tempest_governed_procedure_abort(
      "{.field {path}} must be the exact Tempest contract version `1`."
    )
  }
  value
}

tempest_governed_procedure_record <- function(value, path = "reference") {
  if (S7::S7_inherits(value, TempestGovernedProcedureRef)) {
    value <- stats::setNames(
      lapply(
        tempest_governed_procedure_fields(),
        \(field) S7::prop(value, field)
      ),
      tempest_governed_procedure_fields()
    )
  }
  if (!is.list(value) || is.data.frame(value)) {
    tempest_governed_procedure_abort(
      "{.field {path}} must be a Tempest governed-procedure reference."
    )
  }
  fields <- tempest_governed_procedure_fields()
  value_names <- names(value)
  if (!identical(value_names, fields)) {
    tempest_governed_procedure_abort(
      paste0(
        "{.field {path}} must contain exactly the current ",
        "governed-procedure fields in writer order."
      )
    )
  }
  value$stage <- tempest_governed_procedure_string(
    value$stage,
    paste0(path, "$stage")
  )
  if (!value$stage %in% tempest_program_set_stages()) {
    tempest_governed_procedure_abort(
      "{.field {path}$stage} must identify an exact Tempest stage."
    )
  }
  for (field in c(
    "record_id",
    "revision_id",
    "tempest_governed_procedure_id",
    "evaluator_id",
    "evaluator_version",
    "store_id",
    "snapshot_id",
    "schema_build_digest"
  )) {
    value[[field]] <- tempest_governed_procedure_string(
      value[[field]],
      paste0(path, "$", field)
    )
  }
  value$program_artifact_id <- tryCatch(
    tempest_research_manifest_program_artifact_id(
      value$program_artifact_id,
      paste0(path, "$program_artifact_id")
    ),
    error = function(error) {
      tempest_governed_procedure_abort(
        "{.field {path}$program_artifact_id} must be an exact dsprrr artifact ID."
      )
    }
  )
  value$contract_version <- tempest_governed_procedure_contract_version(
    value$contract_version,
    paste0(path, "$contract_version")
  )
  value$commit_order <- tempest_governed_procedure_commit_order(
    value$commit_order,
    paste0(path, "$commit_order")
  )
  value
}

tempest_governed_procedure_s7_validator <- function(self) {
  result <- tryCatch(
    {
      tempest_governed_procedure_record(self)
      NULL
    },
    error = conditionMessage
  )
  result
}

#' Tempest governed-procedure reference
#'
#' An immutable reference to one governed procedure revision and the pinned
#' Graft view in which it was accepted. The reference also binds the exact
#' dsprrr program artifact and Tempest evaluator contract.
#'
#' @keywords internal
TempestGovernedProcedureRef <- S7::new_class(
  "TempestGovernedProcedureRef",
  properties = list(
    stage = S7::new_property(S7::class_character),
    tempest_governed_procedure_id = S7::new_property(S7::class_character),
    record_id = S7::new_property(S7::class_character),
    revision_id = S7::new_property(S7::class_character),
    program_artifact_id = S7::new_property(S7::class_character),
    contract_version = S7::new_property(S7::class_integer),
    evaluator_id = S7::new_property(S7::class_character),
    evaluator_version = S7::new_property(S7::class_character),
    store_id = S7::new_property(S7::class_character),
    snapshot_id = S7::new_property(S7::class_character),
    schema_build_digest = S7::new_property(S7::class_character),
    commit_order = S7::new_property(S7::class_double)
  ),
  validator = tempest_governed_procedure_s7_validator
)

tempest_governed_procedure_ref_new <- function(
  stage,
  tempest_governed_procedure_id,
  record_id,
  revision_id,
  program_artifact_id,
  contract_version = 1L,
  evaluator_id = paste0("tempest::evaluator/", stage),
  evaluator_version = "1",
  store_id,
  snapshot_id,
  schema_build_digest,
  commit_order
) {
  value <- tempest_governed_procedure_record(list(
    stage = stage,
    tempest_governed_procedure_id = tempest_governed_procedure_id,
    record_id = record_id,
    revision_id = revision_id,
    program_artifact_id = program_artifact_id,
    contract_version = contract_version,
    evaluator_id = evaluator_id,
    evaluator_version = evaluator_version,
    store_id = store_id,
    snapshot_id = snapshot_id,
    schema_build_digest = schema_build_digest,
    commit_order = commit_order
  ))
  do.call(TempestGovernedProcedureRef, value)
}

# Preserve a stored procedure reference as inert inspection data.
tempest_governed_procedure_trace_binding <- function(reference) {
  reference <- tempest_governed_procedure_record(reference)
  c(list(kind = "governed_procedure"), reference)
}
