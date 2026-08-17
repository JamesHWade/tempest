test_governed_program_set <- function(
  stage = "personas",
  snapshot_reference = NULL
) {
  base <- tempest_program_set()
  entry <- tempest:::tempest_program_set_entry(base, stage)
  snapshot_reference <- snapshot_reference %||%
    list(
      store_id = "store:governed",
      snapshot_id = "snapshot:governed",
      schema_build_digest = "schema:governed",
      commit_order = 1
    )
  snapshot_reference <- snapshot_reference[c(
    "store_id",
    "snapshot_id",
    "schema_build_digest",
    "commit_order"
  )]
  reference <- do.call(
    tempest:::tempest_governed_procedure_ref_new,
    c(
      list(
        stage = stage,
        tempest_governed_procedure_id = paste0("tempest-procedure:", stage),
        record_id = paste0("graft-procedure:", stage),
        revision_id = paste0("revision:", stage),
        program_artifact_id = entry$program_artifact_id,
        contract_version = entry$contract_version,
        evaluator_id = entry$evaluator_id,
        evaluator_version = entry$evaluator_version
      ),
      snapshot_reference
    )
  )
  tempest_program_set(
    governed_procedure_refs = stats::setNames(list(reference), stage)
  )
}

test_knowledge_view <- function(.local_envir = parent.frame()) {
  path <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    "knowledge-view.duckdb"
  )
  schema <- graft::graft_schema(system.file(
    "extdata",
    "team-directory.data-dict.json",
    package = "graft",
    mustWork = TRUE
  ))
  store <- graft::graft_open(schema, path, okf = "disabled")
  withr::defer(graft::graft_close(store), envir = .local_envir)
  plan <- graft::graft_plan(
    store,
    list(
      organization = data.frame(
        id = "org:knowledge-view-fixture",
        name = "Knowledge View Fixture"
      )
    ),
    graft::graft_provenance(
      "tempest-knowledge-view-test",
      idempotency_key = "knowledge-view-fixture"
    )
  )
  graft::graft_commit(store, plan)
  snapshot <- graft::graft_snapshot(store)
  list(
    view = graft::graft_at(store, snapshot),
    snapshot = snapshot
  )
}
