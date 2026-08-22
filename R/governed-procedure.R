# Governed procedure references -------------------------------------------

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

tempest_governed_procedure_payload_contract_version <- function(value, path) {
  if (!identical(value, "1")) {
    tempest_governed_procedure_abort(
      "{.field {path}} must be the exact governed payload contract version `\"1\"`."
    )
  }
  1L
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

tempest_governed_procedure_references <- function(value) {
  stages <- tempest_program_set_stages()
  if (is.null(value) || length(value) == 0L) {
    return(stats::setNames(rep(list(NULL), length(stages)), stages))
  }
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(names(value)) ||
      anyNA(names(value)) ||
      any(!nzchar(names(value))) ||
      anyDuplicated(names(value)) ||
      any(!names(value) %in% stages)
  ) {
    tempest_governed_procedure_abort(
      paste0(
        "{.arg governed_procedure_refs} must be a uniquely named list ",
        "using only exact Tempest stages."
      )
    )
  }
  result <- stats::setNames(rep(list(NULL), length(stages)), stages)
  for (stage in names(value)) {
    reference <- tempest_governed_procedure_record(
      value[[stage]],
      paste0("governed_procedure_refs$", stage)
    )
    if (!identical(reference$stage, stage)) {
      tempest_governed_procedure_abort(
        "Governed-procedure stage {.val {reference$stage}} must match list stage {.val {stage}}."
      )
    }
    result[[stage]] <- reference
  }
  result
}

tempest_governed_procedure_view_snapshot <- function(knowledge_view) {
  graft::graft_view_snapshot(knowledge_view)
}

tempest_governed_procedure_history <- function(knowledge_view, record_id) {
  graft::graft_history(knowledge_view, record_id, limit = 1L)
}

tempest_governed_procedure_expected_record <- function(reference) {
  list(
    tempest_governed_procedure_id = reference$tempest_governed_procedure_id,
    stage = reference$stage,
    program_artifact_id = reference$program_artifact_id,
    contract_version = as.character(reference$contract_version),
    evaluator_id = reference$evaluator_id,
    evaluator_version = reference$evaluator_version
  )
}

tempest_governed_procedure_history_head <- function(
  knowledge_view,
  record_id,
  expected_class
) {
  history <- tryCatch(
    tempest_governed_procedure_history(knowledge_view, record_id),
    error = function(error) {
      tempest_governed_procedure_abort(
        "Could not resolve the accepted Graft record {.val {record_id}}."
      )
    }
  )
  required <- c("revision_id", "record_id", "class", "record")
  if (
    !is.data.frame(history) ||
      nrow(history) != 1L ||
      !all(required %in% names(history)) ||
      !identical(history$record_id[[1L]], record_id) ||
      !identical(history$class[[1L]], expected_class) ||
      !is.list(history$record[[1L]])
  ) {
    tempest_governed_procedure_abort(
      "The pinned Graft view did not return the exact {.val {expected_class}} record."
    )
  }
  history
}

# Resolve one accepted `GovernedProcedure` and its exact dsprrr
# `ProgramArtifact` through an immutable Graft view. The returned value binds
# the current accepted revision and every snapshot authority dimension.
#
# A reference can only be created from records proven at the supplied pinned
# boundary. Tempest repeats the same verification immediately before provider
# execution so a serialized reference never becomes authority by itself.
tempest_governed_procedure_ref <- function(knowledge_view, record_id) {
  record_id <- tempest_governed_procedure_string(record_id, "record_id")
  snapshot <- tryCatch(
    tempest_governed_procedure_view_snapshot(knowledge_view),
    error = function(error) {
      tempest_governed_procedure_abort(
        "{.arg knowledge_view} must be a valid pinned Graft view."
      )
    }
  )
  snapshot_reference <- tryCatch(
    tempest_snapshot_reference(snapshot),
    error = function(error) {
      tempest_governed_procedure_abort(
        "The pinned Graft view does not expose a valid immutable snapshot."
      )
    }
  )
  procedure <- tempest_governed_procedure_history_head(
    knowledge_view,
    record_id,
    "GovernedProcedure"
  )
  payload <- procedure$record[[1L]]
  required <- c(
    "tempest_governed_procedure_id",
    "stage",
    "program_artifact_id",
    "contract_version",
    "evaluator_id",
    "evaluator_version"
  )
  if (!all(required %in% names(payload))) {
    tempest_governed_procedure_abort(
      "The accepted GovernedProcedure is missing its exact Tempest contract."
    )
  }
  contract_version <- tempest_governed_procedure_payload_contract_version(
    payload$contract_version,
    "GovernedProcedure$contract_version"
  )
  reference <- tempest_governed_procedure_ref_new(
    stage = payload$stage,
    tempest_governed_procedure_id = payload$tempest_governed_procedure_id,
    record_id = record_id,
    revision_id = procedure$revision_id[[1L]],
    program_artifact_id = payload$program_artifact_id,
    contract_version = contract_version,
    evaluator_id = payload$evaluator_id,
    evaluator_version = payload$evaluator_version,
    store_id = snapshot_reference$store_id,
    snapshot_id = snapshot_reference$snapshot_id,
    schema_build_digest = snapshot_reference$schema_build_digest,
    commit_order = snapshot_reference$commit_order
  )
  reference_record <- tempest_governed_procedure_record(reference)
  expected_payload <- tempest_governed_procedure_expected_record(
    reference_record
  )
  if (!identical(payload[names(expected_payload)], expected_payload)) {
    tempest_governed_procedure_abort(
      "The accepted GovernedProcedure does not use the exact Tempest contract."
    )
  }
  artifact <- tempest_governed_procedure_history_head(
    knowledge_view,
    reference_record$program_artifact_id,
    "ProgramArtifact"
  )
  expected_artifact <- list(
    id = reference_record$program_artifact_id,
    artifact_kind = "dsprrr_program"
  )
  if (
    !identical(
      artifact$record[[1L]][names(expected_artifact)],
      expected_artifact
    )
  ) {
    tempest_governed_procedure_abort(
      "The governed procedure does not reference an accepted dsprrr ProgramArtifact."
    )
  }
  reference
}

tempest_governed_procedure_preflight <- function(
  reference,
  knowledge_view,
  stage,
  program_artifact_id,
  contract_version,
  evaluator_id,
  evaluator_version
) {
  reference <- tempest_governed_procedure_record(reference)
  expected_binding <- list(
    stage = stage,
    program_artifact_id = program_artifact_id,
    contract_version = contract_version,
    evaluator_id = evaluator_id,
    evaluator_version = evaluator_version
  )
  actual_binding <- reference[names(expected_binding)]
  if (!identical(actual_binding, expected_binding)) {
    tempest_governed_procedure_abort(
      "The governed procedure does not match the exact ProgramSet execution."
    )
  }
  snapshot <- tryCatch(
    tempest_governed_procedure_view_snapshot(knowledge_view),
    error = function(error) {
      tempest_governed_procedure_abort(
        "{.arg knowledge_view} must be a valid pinned Graft view."
      )
    }
  )
  snapshot_reference <- tryCatch(
    tempest_snapshot_reference(snapshot),
    error = function(error) {
      tempest_governed_procedure_abort(
        "The pinned Graft view does not expose a valid immutable snapshot."
      )
    }
  )
  expected_snapshot <- reference[c(
    "store_id",
    "snapshot_id",
    "schema_build_digest",
    "commit_order"
  )]
  actual_snapshot <- snapshot_reference[names(expected_snapshot)]
  if (!identical(actual_snapshot, expected_snapshot)) {
    tempest_governed_procedure_abort(
      "The governed procedure does not belong to the supplied pinned Graft view."
    )
  }

  procedure <- tempest_governed_procedure_history_head(
    knowledge_view,
    reference$record_id,
    "GovernedProcedure"
  )
  if (!identical(procedure$revision_id[[1L]], reference$revision_id)) {
    tempest_governed_procedure_abort(
      "The governed procedure revision is not current at the pinned boundary."
    )
  }
  expected_record <- tempest_governed_procedure_expected_record(reference)
  actual_record <- procedure$record[[1L]]
  if (!identical(actual_record[names(expected_record)], expected_record)) {
    tempest_governed_procedure_abort(
      "The accepted GovernedProcedure payload does not match its typed reference."
    )
  }

  artifact <- tempest_governed_procedure_history_head(
    knowledge_view,
    reference$program_artifact_id,
    "ProgramArtifact"
  )
  expected_artifact <- list(
    id = reference$program_artifact_id,
    artifact_kind = "dsprrr_program"
  )
  if (
    !identical(
      artifact$record[[1L]][names(expected_artifact)],
      expected_artifact
    )
  ) {
    tempest_governed_procedure_abort(
      "The governed procedure does not reference an accepted dsprrr ProgramArtifact."
    )
  }
  reference
}

tempest_governed_procedure_trace_binding <- function(reference) {
  reference <- tempest_governed_procedure_record(reference)
  c(list(kind = "governed_procedure"), reference)
}
