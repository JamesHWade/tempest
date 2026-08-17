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

test_that("governed procedure references reject legacy and ambiguous shapes", {
  program_id <- paste0("sha256:", strrep("b", 64L))

  expect_error(
    tempest_program_set(
      governed_procedure_refs = list(extract_claims = "revision:legacy")
    ),
    class = "tempest_program_set_error"
  )
  expect_error(
    tempest:::tempest_governed_procedure_ref_new(
      stage = "not-a-stage",
      tempest_governed_procedure_id = "tempest-procedure:claims",
      record_id = "graft-procedure:claims",
      revision_id = "revision:claims-7",
      program_artifact_id = program_id,
      store_id = "store:accepted",
      snapshot_id = "snapshot:accepted",
      schema_build_digest = "schema:accepted",
      commit_order = 7
    ),
    class = "tempest_governed_procedure_error"
  )
  old_shape <- list(
    stage = "extract_claims",
    governed_procedure_revision_id = "revision:legacy"
  )
  expect_error(
    tempest:::tempest_governed_procedure_record(old_shape),
    class = "tempest_governed_procedure_error"
  )
})

test_that("ProgramSet construction rejects mismatched governed bindings", {
  reference <- tempest:::tempest_governed_procedure_ref_new(
    stage = "personas",
    tempest_governed_procedure_id = "tempest-procedure:personas",
    record_id = "graft-procedure:personas",
    revision_id = "revision:personas",
    program_artifact_id = paste0("sha256:", strrep("f", 64L)),
    store_id = "store:accepted",
    snapshot_id = "snapshot:accepted",
    schema_build_digest = "schema:accepted",
    commit_order = 1
  )

  expect_error(
    tempest_program_set(
      governed_procedure_refs = list(personas = reference)
    ),
    class = "tempest_research_manifest_error",
    regexp = "must match its exact ProgramSet"
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

test_that("governed preflight verifies the pinned procedure and program", {
  program_id <- paste0("sha256:", strrep("c", 64L))
  reference <- test_governed_procedure_ref(
    "extract_claims",
    program_id,
    revision_id = "revision:claims-7",
    snapshot_id = "snapshot:accepted"
  )
  reference_record <- tempest:::tempest_governed_procedure_record(reference)
  snapshot_reference <- list(
    batch_id = "batch:7",
    commit_order = 1,
    committed_at = "2026-08-16T12:00:00.000000Z",
    history_complete = TRUE,
    schema_build_digest = "schema:test",
    schema_version = 1L,
    snapshot_id = "snapshot:accepted",
    store_format_version = "1",
    store_id = "store:test"
  )
  procedure_record <- tempest:::tempest_governed_procedure_expected_record(
    reference_record
  )
  histories <- list(
    `procedure:extract_claims` = data.frame(
      revision_id = "revision:claims-7",
      record_id = "procedure:extract_claims",
      class = "GovernedProcedure",
      record = I(list(procedure_record))
    )
  )
  histories[[program_id]] <- data.frame(
    revision_id = "revision:program",
    record_id = program_id,
    class = "ProgramArtifact",
    record = I(list(list(
      id = program_id,
      artifact_kind = "dsprrr_program"
    )))
  )
  local_mocked_bindings(
    tempest_governed_procedure_view_snapshot = \(knowledge_view) list(),
    tempest_snapshot_reference = \(snapshot) snapshot_reference,
    tempest_governed_procedure_history = function(knowledge_view, record_id) {
      histories[[record_id]]
    }
  )

  resolved <- tempest_governed_procedure_ref(
    structure(list(), class = "pinned-view-fixture"),
    "procedure:extract_claims"
  )
  expect_identical(
    tempest:::tempest_governed_procedure_record(resolved),
    reference_record
  )

  verified <- tempest:::tempest_governed_procedure_preflight(
    reference,
    knowledge_view = structure(list(), class = "pinned-view-fixture"),
    stage = "extract_claims",
    program_artifact_id = program_id,
    contract_version = 1L,
    evaluator_id = "tempest::evaluator/extract_claims",
    evaluator_version = "1"
  )
  expect_identical(verified, reference_record)

  expect_error(
    tempest:::tempest_governed_procedure_preflight(
      reference,
      knowledge_view = structure(list(), class = "pinned-view-fixture"),
      stage = "extract_claims",
      program_artifact_id = paste0("sha256:", strrep("d", 64L)),
      contract_version = 1L,
      evaluator_id = "tempest::evaluator/extract_claims",
      evaluator_version = "1"
    ),
    class = "tempest_governed_procedure_error",
    regexp = "exact ProgramSet execution"
  )
})

test_that("governed preflight rejects stale accepted revisions", {
  program_id <- paste0("sha256:", strrep("e", 64L))
  reference <- test_governed_procedure_ref(
    "extract_claims",
    program_id,
    revision_id = "revision:expected"
  )
  reference_record <- tempest:::tempest_governed_procedure_record(reference)
  local_mocked_bindings(
    tempest_governed_procedure_view_snapshot = \(knowledge_view) list(),
    tempest_snapshot_reference = \(snapshot) {
      list(
        store_id = "store:test",
        snapshot_id = "snapshot:test",
        schema_build_digest = "schema:test",
        commit_order = 1
      )
    },
    tempest_governed_procedure_history = function(knowledge_view, record_id) {
      data.frame(
        revision_id = "revision:later",
        record_id = record_id,
        class = "GovernedProcedure",
        record = I(list(
          tempest:::tempest_governed_procedure_expected_record(
            reference_record
          )
        ))
      )
    }
  )

  expect_error(
    tempest:::tempest_governed_procedure_preflight(
      reference,
      knowledge_view = list(),
      stage = "extract_claims",
      program_artifact_id = program_id,
      contract_version = 1L,
      evaluator_id = "tempest::evaluator/extract_claims",
      evaluator_version = "1"
    ),
    class = "tempest_governed_procedure_error",
    regexp = "not current"
  )
})
