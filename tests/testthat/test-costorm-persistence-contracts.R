test_that("Co-STORM restore and resume require the recorded custom ProgramSet", {
  skip_if_not_installed("ellmer")
  forward <- function(text, ...) list(answer = text)
  program_set <- test_program_set_from_program(
    dsprrr::module_fn("text -> answer", forward),
    registry = list(forward = forward)
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Custom Co-STORM programs",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.program-set")),
    program_set = program_set
  )
  snapshot <- tempest_session_snapshot(session)
  bundle <- file.path(withr::local_tempdir(), "custom-program-session")
  tempest_session_save(session, bundle)

  expect_error(
    tempest_session_restore(snapshot, config = cfg),
    class = "tempest_session_restore_error"
  )
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )
  expect_r6_class(
    tempest_session_restore(snapshot, config = cfg, program_set = program_set),
    "TempestSession"
  )
  expect_r6_class(
    tempest_session_resume(bundle, config = cfg, program_set = program_set),
    "TempestSession"
  )
})

test_that("TempestSession restores progress history without replaying it", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  collector <- tempest_progress_collector(include_payload = TRUE)
  expert <- tempest_expert(
    name = "Dr. History",
    title = "Progress expert",
    description = "Event replay",
    instructions = "Track workflow progress without replaying old events."
  )
  session <- tempest:::TempestSession$new(
    "Progress history",
    config = cfg,
    experts = list(expert),
    progress = collector$record
  )
  session$emit_progress(
    "stage",
    "started",
    stage = "dialogue",
    step = "turn"
  )
  history <- tempest_execution_events(session)
  snapshot <- tempest:::tempest_session_snapshot(session)

  expect_equal(length(history), 2)
  expect_equal(tempest_progress_state(history)$run_id, session$session_id)
  expect_equal(length(snapshot$progress_events), length(history))
  expect_identical("artifacts" %in% names(snapshot), FALSE)

  restore_collector <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest:::tempest_session_restore_internal(
    snapshot,
    config = cfg,
    progress = restore_collector$record
  )

  expect_length(restore_collector$events(), 0)
  restored_history <- tempest_execution_events(restored)
  expect_equal(length(restored_history), length(history))
  expect_equal(
    tempest_progress_state(restored_history)$run_id,
    session$session_id
  )

  restored$emit_progress(
    "stage",
    "succeeded",
    stage = "dialogue",
    step = "turn"
  )
  expect_length(restore_collector$events(), 1)
  expect_equal(
    length(tempest_execution_events(restored)),
    length(history) + 1
  )

  legacy_snapshot <- snapshot
  legacy_snapshot$schema_version <- 9L
  expect_error(
    tempest:::tempest_session_restore(legacy_snapshot, config = cfg),
    class = "tempest_unsupported_format_error"
  )
})

test_that("Co-STORM snapshots require terminal stage attempts", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Running stage snapshot",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.running-stage")),
    session_id = "running-stage-snapshot"
  )
  reference <- session$manifest@programs$personas
  running <- tempest:::tempest_stage_record_start(
    "personas",
    reference$program_artifact_id,
    reference$governed_procedure_ref$revision_id,
    trace_references = list(
      research_run_id = session$session_id,
      mode = "costorm",
      role = "program"
    ),
    attempt_id = "attempt-running-session",
    started_at = "2026-08-16T00:00:00Z"
  )
  tempest:::tempest_session_set_stage_records(session, list(running))
  live_data <- tempest:::tempest_stage_record_data(running)

  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  expect_identical(
    tempest:::tempest_session_stage_records(session)[[1]]@status,
    "running"
  )
  expect_identical(
    tempest:::tempest_stage_record_data(
      tempest:::tempest_session_stage_records(session)[[1]]
    ),
    live_data
  )
  cancelled <- tempest:::tempest_stage_record_cancel(
    running,
    completed_at = "2026-08-16T00:00:30Z"
  )
  tempest:::tempest_session_set_stage_records(session, list(cancelled))
  snapshot <- tempest_session_snapshot(session)
  expect_identical(snapshot$stage_records[[1]]$status, "cancelled")
  restored <- tempest_session_restore(snapshot, config = cfg)
  expect_identical(
    tempest:::tempest_session_stage_records(restored)[[1]]@status,
    "cancelled"
  )

  snapshot$stage_records <- tempest:::tempest_stage_records_data(list(running))
  expect_error(
    tempest_session_restore(snapshot, config = cfg),
    class = "tempest_session_restore_error"
  )

  expect_error(
    tempest:::tempest_stage_record_succeed(
      running,
      tempest:::tempest_stage_output_reference(
        "state_field",
        "perspectives",
        content_digest = paste0("sha256:", strrep("1", 64L))
      ),
      support_status = "unknown",
      completed_at = "2026-08-16T00:01:00Z"
    ),
    class = "tempest_stage_record_error"
  )

  valid_output <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = tempest:::tempest_stage_state_output_digest(
        "personas",
        session$experts
      )
    ),
    support_status = "unknown",
    completed_at = "2026-08-16T00:01:00Z"
  )
  tempest:::tempest_session_set_stage_records(session, list(valid_output))
  valid_snapshot <- tempest_session_snapshot(session)
  expect_identical(valid_snapshot$stage_records[[1]]$status, "succeeded")

  reordered_stage_record <- valid_snapshot
  reordered_stage_record$stage_records[[1]] <-
    reordered_stage_record$stage_records[[1]][
      rev(names(reordered_stage_record$stage_records[[1]]))
    ]
  expect_error(
    tempest_session_restore(reordered_stage_record, config = cfg),
    class = "tempest_session_restore_error"
  )

  renamed <- valid_snapshot
  renamed$experts[[1]]$expert_id <- "expert.forged-custom-id"
  renamed$experts[[1]]$fingerprint <-
    tempest:::tempest_expert_profile_fingerprint(renamed$experts[[1]])
  expect_error(
    tempest_session_restore(renamed, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle <- file.path(withr::local_tempdir(), "persona-binding")
  tempest_session_save(session, bundle)
  experts_path <- file.path(bundle, "experts.json")
  experts <- tempest:::tempest_product_read_json(experts_path)
  experts[[1]]$name <- "Forged but internally refingerprinted expert"
  experts[[1]]$fingerprint <-
    tempest:::tempest_expert_profile_fingerprint(experts[[1]])
  tempest:::tempest_product_write_json(experts_path, experts)
  manifest_path <- file.path(bundle, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_product_bundle_checksum(bundle, "experts.json")
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("current Co-STORM snapshots require the expert field", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Required expert snapshot",
    config = cfg,
    experts = list(tempest_expert(
      name = "Snapshot Expert",
      title = "Persistence analyst",
      description = "Checks required current fields.",
      instructions = "Reject missing expert records."
    ))
  )
  snapshot <- tempest_session_snapshot(session)
  snapshot$experts <- NULL

  expect_error(
    tempest_session_restore(snapshot, config = cfg),
    class = "tempest_unsupported_format_error"
  )
})
