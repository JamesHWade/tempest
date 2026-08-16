# Cross-repository identity and correlation boundaries

tempest_ecosystem_contract_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_ecosystem_contract_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_dsprrr_execution_program <- function(program) {
  if (inherits(program, "tempest_dsprrr_execution")) {
    return(program$program)
  }
  program
}

tempest_dsprrr_execution <- function(
  program,
  program_artifact_id,
  trace_context,
  stage,
  contract_version = 1L,
  evaluator_id,
  evaluator_version,
  governed_procedure_ref = NULL
) {
  stage <- tempest_program_set_string(stage, "stage")
  if (!stage %in% tempest_program_set_stages()) {
    tempest_ecosystem_contract_abort(
      "{.arg stage} must identify an exact Tempest ProgramSet stage."
    )
  }
  expected_evaluator <- tempest_program_set_default_evaluators()[[stage]]
  contract_version <- tempest_governed_procedure_contract_version(
    contract_version,
    "contract_version"
  )
  evaluator_id <- tempest_program_set_string(evaluator_id, "evaluator_id")
  evaluator_version <- tempest_program_set_string(
    evaluator_version,
    "evaluator_version"
  )
  if (
    !identical(evaluator_id, expected_evaluator$evaluator_id) ||
      !identical(evaluator_version, expected_evaluator$evaluator_version)
  ) {
    tempest_ecosystem_contract_abort(
      "The stage execution references an unknown builtin evaluator contract."
    )
  }
  governed_procedure_ref <- if (is.null(governed_procedure_ref)) {
    NULL
  } else {
    reference <- tempest_governed_procedure_record(
      governed_procedure_ref,
      "governed_procedure_ref"
    )
    expected_binding <- list(
      stage = stage,
      program_artifact_id = program_artifact_id,
      contract_version = contract_version,
      evaluator_id = evaluator_id,
      evaluator_version = evaluator_version
    )
    if (!identical(reference[names(expected_binding)], expected_binding)) {
      tempest_ecosystem_contract_abort(
        "The governed procedure does not match the dsprrr execution contract."
      )
    }
    reference
  }
  structure(
    list(
      program = tempest_dsprrr_execution_program(program),
      program_artifact_id = tempest_research_manifest_program_artifact_id(
        program_artifact_id,
        "program_artifact_id"
      ),
      trace_context = tempest_research_manifest_canonical_value(
        trace_context,
        "trace_context"
      ),
      stage = stage,
      contract_version = contract_version,
      evaluator_id = evaluator_id,
      evaluator_version = evaluator_version,
      governed_procedure_ref = governed_procedure_ref
    ),
    class = c("tempest_dsprrr_execution", "list")
  )
}

tempest_program_reference <- function(program) {
  program <- tempest_dsprrr_execution_program(program)
  program_artifact_id <- tryCatch(
    dsprrr::program_artifact_id(program),
    error = function(error) {
      tempest_ecosystem_contract_abort(
        "Could not identify the dsprrr program artifact.",
        parent = error
      )
    }
  )
  list(
    program_artifact_id = tempest_research_manifest_program_artifact_id(
      program_artifact_id,
      "program_artifact_id"
    )
  )
}

tempest_snapshot_reference <- function(snapshot) {
  if (
    !inherits(snapshot, "graft::GraftSnapshot") ||
      !inherits(snapshot, "S7_object")
  ) {
    tempest_ecosystem_contract_abort(
      "{.arg snapshot} must be a real {.cls graft::GraftSnapshot}."
    )
  }
  fields <- c(
    "schema_version",
    "snapshot_id",
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "commit_order",
    "batch_id",
    "committed_at",
    "history_complete"
  )
  if (!setequal(S7::prop_names(snapshot), fields)) {
    tempest_ecosystem_contract_abort(
      "The Graft snapshot does not expose the complete public boundary."
    )
  }
  tryCatch(
    S7::validate(snapshot),
    error = function(error) {
      tempest_ecosystem_contract_abort(
        "The Graft snapshot failed its public validation contract.",
        parent = error
      )
    }
  )
  reference <- stats::setNames(
    lapply(fields, \(field) S7::prop(snapshot, field)),
    fields
  )
  missing <- vapply(
    reference,
    \(value) length(value) == 1L && is.atomic(value) && is.na(value),
    logical(1)
  )
  reference[missing] <- rep(list(NULL), sum(missing))
  tempest_research_manifest_knowledge_snapshot(reference)
}

tempest_deputy_run_context <- function(
  manifest,
  stage,
  role,
  program_artifact_id = NULL,
  expert_id = NULL
) {
  if (!S7::S7_inherits(manifest, TempestResearchManifest)) {
    tempest_ecosystem_contract_abort(
      "{.arg manifest} must be created by {.fn tempest_research_manifest}."
    )
  }
  stage <- tempest_research_manifest_string(stage, "stage")
  role <- tempest_research_manifest_string(role, "role")
  if (!is.null(program_artifact_id)) {
    program_artifact_id <- tempest_research_manifest_id(
      program_artifact_id,
      "program_artifact_id"
    )
    matches_manifest <- vapply(
      manifest@programs,
      \(reference) {
        identical(
          reference$program_artifact_id,
          program_artifact_id
        )
      },
      logical(1)
    )
    if (!any(matches_manifest)) {
      tempest_ecosystem_contract_abort(
        paste0(
          "The Deputy run-context program artifact is not recorded in ",
          "the research manifest."
        )
      )
    }
  }
  if (!is.null(expert_id)) {
    expert_id <- tempest_research_manifest_id(expert_id, "expert_id")
  }
  context <- list(
    product = "tempest",
    research_run_id = manifest@research_run_id,
    mode = manifest@mode,
    stage = stage,
    role = role
  )
  snapshot_id <- manifest@knowledge_snapshot$snapshot_id %||% NULL
  if (!is.null(snapshot_id)) {
    context$knowledge_snapshot_id <- snapshot_id
  }
  if (!is.null(program_artifact_id)) {
    context$program_artifact_id <- program_artifact_id
  }
  if (!is.null(expert_id)) {
    context$expert_id <- expert_id
  }
  tempest_research_manifest_canonical_value(context, "run_context")
}

tempest_dsprrr_trace_context <- function(
  manifest,
  stage,
  program_artifact_id
) {
  context <- tempest_deputy_run_context(
    manifest,
    stage = stage,
    role = "program",
    program_artifact_id = program_artifact_id
  )
  # dsprrr reserves this field and injects its verified value independently.
  context$program_artifact_id <- NULL
  context
}
