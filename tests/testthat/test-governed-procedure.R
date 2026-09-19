test_that("governed procedure references bind every authority dimension", {
  program_id <- paste0("sha256:", strrep("a", 64L))
  reference <- tempest:::tempest_governed_procedure_ref_new(
    stage = "extract_claims",
    tempest_governed_procedure_id = "tempest-procedure:claims",
    record_id = "graft-procedure:claims",
    revision_id = "revision:claims-7",
    program_artifact_id = program_id,
    store_id = "store:accepted",
    snapshot_id = "snapshot:accepted",
    schema_build_digest = "schema:accepted",
    commit_order = 7
  )
  record <- tempest:::tempest_governed_procedure_record(reference)

  expect_s7_class(reference, tempest:::TempestGovernedProcedureRef)
  expect_named(record, tempest:::tempest_governed_procedure_fields())
  expect_identical(record$program_artifact_id, program_id)
  expect_identical(record$commit_order, 7)
  expect_identical(
    tempest:::tempest_governed_procedure_trace_binding(reference),
    c(list(kind = "governed_procedure"), record)
  )
  expect_error(
    tempest:::tempest_governed_procedure_record(record[rev(names(record))]),
    class = "tempest_governed_procedure_error"
  )
  whole_double_contract <- record
  whole_double_contract$contract_version <- 1
  expect_error(
    tempest:::tempest_governed_procedure_record(whole_double_contract),
    class = "tempest_governed_procedure_error"
  )
})

test_that("ProgramSet rejects the old schema exactly", {
  current <- tempest_program_set()

  expect_error(
    tempest:::TempestProgramSet(
      schema_version = 1L,
      bundle_root = current@bundle_root,
      entries = current@entries,
      programs = current@programs
    ),
    regexp = "schema_version must be the supported version 2"
  )
})


test_that("stored procedure provenance cannot bind a live ProgramSet", {
  programs <- tempest_program_set()
  entries <- tempest_program_set_entries(programs)
  reference <- test_governed_procedure_ref(
    "personas",
    entries$personas$program_artifact_id
  )
  entries$personas$governed_procedure_ref <-
    tempest_governed_procedure_record(reference)
  manifest <- tempest_research_manifest(
    research_run_id = "inert-provenance",
    mode = "storm",
    config = tempest_config(),
    programs = entries
  )
  expect_identical(
    manifest@programs$personas$governed_procedure_ref,
    entries$personas$governed_procedure_ref
  )
  expect_error(
    tempest_program_set_new(entries, programs@programs),
    "provenance only",
    class = "tempest_program_set_error"
  )

  attr(programs, "entries") <- entries
  expect_error(
    tempest_program_set_execution(programs, "personas"),
    class = "tempest_program_set_verification_error"
  )
})

test_that("stage execution rejects stored authority before provider calls", {
  programs <- tempest_program_set()
  module <- tempest_program_set_execution(programs, "personas")
  calls <- 0L
  local_mocked_bindings(tempest_dsprrr_run = function(...) {
    calls <<- calls + 1L
    stop("provider must not run")
  })
  for (field in c("knowledge_view", "governed_procedure_ref")) {
    candidate <- module
    candidate[[field]] <- list(stored = "authority")
    expect_error(
      tempest_execute_stage(
        candidate,
        fake_chat(),
        inputs = list(topic = "Topic", n_experts = 1L, requirements = ""),
        context = list(n_experts = 1L)
      ),
      "cannot bind executable",
      class = "tempest_ecosystem_contract_error"
    )
  }
  expect_identical(calls, 0L)
  expect_identical(tempest_dsprrr_execution_verify(module, "personas"), module)
})
