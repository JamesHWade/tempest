test_that("Tempest restores real Graft snapshots for historical reads", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir()
  store_path <- file.path(root, "knowledge.duckdb")
  schema <- graft::graft_schema(system.file(
    "extdata",
    "team-directory.data-dict.json",
    package = "graft",
    mustWork = TRUE
  ))
  store <- graft::graft_open(schema, store_path, okf = "disabled")
  graft::graft_ingest(
    store,
    list(organization = data.frame(id = "org:tempest", name = "Tempest v1")),
    graft::graft_provenance(
      "tempest-persistence",
      idempotency_key = "tempest-persistence-v1"
    )
  )
  graft_snapshot <- graft::graft_snapshot(store)
  reference <- tempest:::tempest_snapshot_reference(graft_snapshot)
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)

  session_workspace <- tempest_research_workspace(
    graft_snapshot = graft_snapshot
  )
  session <- tempest_session(
    "Graft session persistence",
    config = cfg,
    experts = list(tempest_expert(
      name = "Graft Persistence Expert",
      title = "Knowledge historian",
      description = "Checks immutable accepted-knowledge reads.",
      instructions = "Keep historical reads pinned to their snapshot."
    )),
    retriever = tempest_retriever(
      config = cfg,
      workspace = session_workspace
    ),
    session_id = "graft-session-persistence"
  )
  in_memory <- tempest_session_snapshot(session)
  session_dir <- file.path(root, "session")
  tempest_session_save(session, session_dir)

  storm_workspace <- tempest_research_workspace(
    graft_snapshot = graft_snapshot
  )
  storm_dir <- file.path(root, "storm")
  tempest:::tempest_storm_save_artifacts(
    storm_dir,
    storm_workspace,
    tempest:::tempest_storm_state("Graft STORM persistence"),
    tempest_research_manifest(
      "graft-storm-persistence",
      config = cfg,
      programs = program_references,
      knowledge_snapshot = reference
    ),
    program_set = program_set,
    config = cfg,
    steps = "research"
  )

  session_manifest <- tempest:::tempest_product_read_json(
    file.path(session_dir, "session.json")
  )
  storm_manifest <- tempest:::tempest_product_read_json(
    file.path(storm_dir, "run_config.json")
  )
  sidecar <- "knowledge/graft-snapshot.rds"
  expect_contains(unlist(session_manifest$files, use.names = FALSE), sidecar)
  expect_contains(unlist(storm_manifest$files, use.names = FALSE), sidecar)
  expect_identical(
    session_manifest$checksums[[sidecar]],
    tempest:::tempest_product_bundle_checksum(session_dir, sidecar)
  )
  expect_identical(
    storm_manifest$checksums[[sidecar]],
    tempest:::tempest_product_bundle_checksum(storm_dir, sidecar)
  )

  rm(session, session_workspace, storm_workspace, graft_snapshot)
  graft::graft_close(store)
  store <- graft::graft_open(schema, store_path, okf = "disabled")
  withr::defer(graft::graft_close(store))
  graft::graft_ingest(
    store,
    list(organization = data.frame(id = "org:tempest", name = "Tempest v2")),
    graft::graft_provenance(
      "tempest-persistence",
      idempotency_key = "tempest-persistence-v2"
    )
  )

  restored_memory <- tempest_session_restore(in_memory, config = cfg)
  restored_session <- tempest_session_resume(session_dir, config = cfg)
  restored_storm <- tempest:::tempest_storm_load_artifacts(
    storm_dir,
    config = cfg,
    program_set = program_set,
    run_id = "graft-storm-persistence"
  )
  restored_snapshots <- list(
    tempest:::tempest_session_workspace(restored_memory)$graft_snapshot,
    tempest:::tempest_session_workspace(restored_session)$graft_snapshot,
    restored_storm$workspace$graft_snapshot
  )

  for (restored_snapshot in restored_snapshots) {
    expect_s3_class(restored_snapshot, "graft::GraftSnapshot")
    expect_identical(
      tempest:::tempest_snapshot_reference(restored_snapshot),
      reference
    )
    historical <- graft::graft_at(store, restored_snapshot)
    expect_identical(
      graft::graft_get(historical, "org:tempest")$record$name,
      "Tempest v1"
    )
  }
  expect_identical(
    graft::graft_get(store, "org:tempest")$record$name,
    "Tempest v2"
  )
})
