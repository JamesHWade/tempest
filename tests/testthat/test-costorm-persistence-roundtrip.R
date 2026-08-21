test_that("Tempest session bundles save and resume durable state", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        return(fake_chat(
          structured = list(list(
            facts = list(list(
              claim = "Bundles preserve claims.",
              sources = list(list(
                source_id = source@resource_id,
                quote = "Bundles preserve claims."
              )),
              confidence = "high"
            ))
          ))
        ))
      }
      if (identical(role, "coordinator")) {
        return(fake_chat(
          text = list(paste0(
            "Bundles preserve claims [",
            source@resource_id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )
  store <- tempest_research_workspace()
  source <- fake_source(
    url = "https://example.com/session-bundle",
    title = "Session Bundle Source",
    content_text = "Bundles preserve claims."
  )
  store$upsert_retrieved_resource(source)
  expert <- tempest_expert(
    name = "Dr. Bundle",
    title = "Persistence expert",
    description = "Bundle state",
    instructions = "Preserve durable state and evidence lineage."
  )
  session <- tempest_session(
    "Session bundle",
    config = cfg,
    experts = list(expert),
    retriever = tempest_retriever(config = cfg, workspace = store)
  )
  completion_id <- tempest:::tempest_costorm_await(
    session$request_completion_async("Record the bundle evidence.")
  )
  withCallingHandlers(
    tempest:::tempest_costorm_await(tempest_session_process_turn_async(
      session,
      completion_id,
      suggest = FALSE,
      n_suggestions = 4L,
      is_current = function() TRUE
    )),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  claim_id <- store$list_proposed_claims()[[1L]]@claim_id
  tempest_verify_claims(
    session,
    verifier = fake_chat(
      structured = list(list(
        status = "supported",
        score = 0.95,
        rationale = "The exact span supports the claim."
      ))
    ),
    verifier_model = "judge.bundle"
  )
  session_id <- session$session_id
  session$add_turn("User", "user", "Save this session.")
  report_md <- tempest_report_md(
    title = session$title,
    body = paste0("Bundle report [", source@resource_id, "]."),
    workspace = store,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  report_md <- test_persistence_commit_existing_costorm_report(
    session,
    report_md
  )
  tempest:::tempest_session_set_suggestions(
    session,
    c("What next?", "And then?")
  )
  session$emit_progress(
    "stage",
    "started",
    stage = "dialogue",
    step = "turn"
  )

  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  saved <- tempest_session_save(session, bundle_dir)
  manifest <- tempest:::tempest_product_read_json(
    file.path(bundle_dir, "session.json")
  )
  expect_equal(
    saved,
    normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  )
  expect_null(manifest$status)
  expect_equal(manifest$bundle_type, "costorm")
  expect_equal(manifest$bundle_status, "complete")
  expect_equal(manifest$schema_version, 10L)
  expect_identical(
    manifest$research_manifest$research_run_id,
    session_id
  )
  expect_identical(manifest$research_manifest$mode, "costorm")
  expect_identical(manifest$research_manifest$status, "succeeded")
  expect_identical(
    manifest$workspace$base_snapshot_id,
    session$workspace$base_snapshot_id
  )
  expect_identical(manifest$workspace$schema_version, 5L)
  expect_setequal(names(manifest$checksums), manifest$files)
  expect_contains(
    manifest$files,
    c(
      "experts.json",
      "retired_expert_ids.json",
      "expert_sessions.json",
      "progress_events.json",
      "stage_records.json",
      "workspace/retrieved_resources.json",
      "workspace/proposed_claims.json",
      "workspace/claim_supports.json",
      "report.md",
      "artifacts/suggested_questions.json"
    )
  )
  expect_false("artifacts/report_body.md" %in% manifest$files)
  expect_false("artifacts/mindmap.md" %in% manifest$files)
  expect_false("workflow_run.json" %in% manifest$files)
  expect_false(any(startsWith(
    unlist(manifest$files, use.names = FALSE),
    "artifacts/typed/"
  )))
  expect_false("config.json" %in% manifest$files)

  legacy_manifest <- manifest
  legacy_manifest$schema_version <- 9L
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    legacy_manifest
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_unsupported_format_error"
  )

  reordered_manifest <- manifest[rev(names(manifest))]
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    reordered_manifest
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  reordered_workspace <- manifest
  reordered_workspace$workspace <- reordered_workspace$workspace[
    rev(names(reordered_workspace$workspace))
  ]
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    reordered_workspace
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    manifest
  )

  downgraded_manifest <- manifest
  downgraded_manifest$workspace$schema_version <- 4L
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    downgraded_manifest
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    manifest
  )

  nested_files <- manifest
  nested_files$files[[1]] <- list(nested_files$files[[1]])
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    nested_files
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  nested_checksums <- manifest
  first_checksum <- names(nested_checksums$checksums)[[1]]
  nested_checksums$checksums[[first_checksum]] <- list(
    nested_checksums$checksums[[first_checksum]]
  )
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    nested_checksums
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    manifest
  )

  restore_collector <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest_session_resume(
    bundle_dir,
    config = cfg,
    progress = restore_collector$record
  )

  expect_r6_class(restored, "TempestSession")
  expect_equal(restored$session_id, session_id)
  expect_equal(tail(restored$transcript, 1L)[[1L]]$text, "Save this session.")
  expect_identical(tempest_session_report_md(restored), report_md)
  expect_equal(
    tempest:::tempest_session_suggestions(restored),
    c("What next?", "And then?")
  )
  expect_equal(
    restored$workspace$get_proposed_claim(claim_id)@claim_text,
    "Bundles preserve claims."
  )
  expect_identical(restored$retriever$workspace, restored$workspace)
  expect_equal(nrow(tempest_claim_supports(restored$workspace)), 1)
  expect_identical(restored$manifest@research_run_id, session_id)
  expect_identical(
    restored$manifest@config_digest,
    session$manifest@config_digest
  )
  expect_length(restore_collector$events(), 0)
  expect_equal(
    tempest_progress_state(tempest_execution_events(restored))$run_id,
    session_id
  )

  expect_error(
    tempest_session_save(session, bundle_dir),
    class = "tempest_session_save_error"
  )
  expect_error(
    tempest_session_save(list(), bundle_dir, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  expect_error(
    tempest_session_snapshot(list()),
    class = "tempest_session_snapshot_error"
  )
  expect_no_error(tempest_session_save(session, bundle_dir, overwrite = TRUE))
})

test_that("schema 9 session bundles are rejected", {
  bundle_dir <- withr::local_tempdir()
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "session.json"),
    list(schema_version = 9L)
  )
  expect_error(
    tempest_session_resume(bundle_dir),
    class = "tempest_unsupported_format_error"
  )
})
