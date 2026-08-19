test_that("Graft snapshot sidecars fail closed on integrity mismatch", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir()
  schema <- graft::graft_schema(system.file(
    "extdata",
    "team-directory.data-dict.json",
    package = "graft",
    mustWork = TRUE
  ))
  store <- graft::graft_open(
    schema,
    file.path(root, "knowledge.duckdb"),
    okf = "disabled"
  )
  withr::defer(graft::graft_close(store))
  graft::graft_ingest(
    store,
    list(organization = data.frame(id = "org:tempest", name = "Tempest")),
    graft::graft_provenance(
      "tempest-integrity",
      idempotency_key = "tempest-integrity-v1"
    )
  )
  snapshot <- graft::graft_snapshot(store)
  reference <- tempest:::tempest_snapshot_reference(snapshot)
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace(graft_snapshot = snapshot)
  session <- tempest_session(
    "Graft sidecar integrity",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.graft-integrity",
      name = "Graft Integrity Expert",
      title = "Persistence reviewer",
      description = "Checks sidecar integrity.",
      instructions = "Reject ambiguous accepted-knowledge boundaries."
    )),
    retriever = tempest_retriever(config = cfg, workspace = workspace)
  )
  session_dir <- file.path(root, "session")
  tempest_session_save(session, session_dir)
  storm_dir <- file.path(root, "storm")
  tempest:::tempest_storm_save_artifacts(
    storm_dir,
    tempest_research_workspace(graft_snapshot = snapshot),
    tempest:::tempest_storm_state("Graft sidecar integrity"),
    tempest_research_manifest(
      "graft-sidecar-integrity",
      config = cfg,
      programs = program_references,
      knowledge_snapshot = reference
    ),
    program_set = program_set,
    config = cfg,
    steps = "research"
  )

  sidecar <- "knowledge/graft-snapshot.rds"
  session_sidecar <- file.path(session_dir, sidecar)
  storm_sidecar <- file.path(storm_dir, sidecar)
  session_bytes <- readBin(
    session_sidecar,
    what = "raw",
    n = file.info(session_sidecar)$size
  )
  storm_bytes <- readBin(
    storm_sidecar,
    what = "raw",
    n = file.info(storm_sidecar)$size
  )

  unlink(session_sidecar)
  expect_error(
    tempest_session_resume(session_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  writeBin(session_bytes, session_sidecar)
  writeBin(charToRaw("corrupt snapshot"), session_sidecar)
  expect_error(
    tempest_session_resume(session_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  writeBin(session_bytes, session_sidecar)

  unlink(storm_sidecar)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      storm_dir,
      config = cfg,
      program_set = program_set
    ),
    class = "tempest_run_restore_error"
  )
  writeBin(storm_bytes, storm_sidecar)
  writeBin(charToRaw("corrupt snapshot"), storm_sidecar)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      storm_dir,
      config = cfg,
      program_set = program_set
    ),
    class = "tempest_run_restore_error"
  )
  writeBin(storm_bytes, storm_sidecar)

  session_manifest_path <- file.path(session_dir, "session.json")
  session_manifest <- tempest:::tempest_product_read_json(
    session_manifest_path
  )
  mismatches <- list(
    schema_version = 2L,
    snapshot_id = paste0("sha256:", strrep("0", 64L)),
    store_id = "graft-store-foreign",
    store_format_version = "999.0.0",
    schema_build_digest = paste0("sha256:", strrep("1", 64L)),
    commit_order = reference$commit_order + 1,
    batch_id = "graft:foreign",
    committed_at = "2099-01-01T00:00:00.000000Z",
    history_complete = !reference$history_complete
  )
  for (field in names(mismatches)) {
    tampered <- session_manifest
    tampered$research_manifest$knowledge_snapshot[[field]] <-
      mismatches[[field]]
    tempest:::tempest_product_write_json(session_manifest_path, tampered)
    expect_error(
      tempest_session_resume(session_dir, config = cfg),
      class = "tempest_session_restore_error",
      info = field
    )
  }
  tempest:::tempest_product_write_json(session_manifest_path, session_manifest)

  foreign_store <- graft::graft_open(
    schema,
    file.path(root, "foreign.duckdb"),
    okf = "disabled"
  )
  withr::defer(graft::graft_close(foreign_store))
  graft::graft_ingest(
    foreign_store,
    list(organization = data.frame(id = "org:foreign", name = "Foreign")),
    graft::graft_provenance(
      "tempest-integrity",
      idempotency_key = "tempest-integrity-foreign"
    )
  )
  saveRDS(graft::graft_snapshot(foreign_store), session_sidecar, version = 3L)
  foreign_manifest <- session_manifest
  foreign_manifest$checksums[[sidecar]] <-
    tempest:::tempest_product_bundle_checksum(session_dir, sidecar)
  tempest:::tempest_product_write_json(session_manifest_path, foreign_manifest)
  expect_error(
    tempest_session_resume(session_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})
