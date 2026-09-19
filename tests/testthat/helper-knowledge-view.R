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
