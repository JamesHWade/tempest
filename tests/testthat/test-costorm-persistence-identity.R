test_that("schema 10 session restore protects research identity", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace()
  session <- tempest_session(
    "Protected session",
    config = cfg,
    experts = list(tempest_expert(
      name = "Protected Session Expert",
      title = "Persistence analyst",
      description = "Checks durable research identity.",
      instructions = "Reject mismatched restore inputs."
    )),
    retriever = tempest_retriever(config = cfg, workspace = workspace),
    session_id = "protected-session"
  )
  snapshot <- tempest_session_snapshot(session)
  expect_no_error(tempest_session_restore(snapshot, config = cfg))

  double_schema <- snapshot
  double_schema$schema_version <- 10.0
  expect_error(
    tempest_session_restore(double_schema, config = cfg),
    class = "tempest_session_restore_error"
  )

  null_transcript <- snapshot
  null_transcript["transcript"] <- list(NULL)
  expect_error(
    tempest_session_restore(null_transcript, config = cfg),
    class = "tempest_session_restore_error"
  )

  null_expert_sessions <- snapshot
  null_expert_sessions["expert_sessions"] <- list(NULL)
  expect_error(
    tempest_session_restore(null_expert_sessions, config = cfg),
    class = "tempest_session_restore_error"
  )

  for (field in tempest:::tempest_expert_record_fields()) {
    null_expert_field <- snapshot
    null_expert_field$experts[[1]][field] <- list(NULL)
    expect_error(
      tempest_session_restore(null_expert_field, config = cfg),
      class = "tempest_session_restore_error",
      info = field
    )
  }

  double_expert_schema <- snapshot
  double_expert_schema$experts[[1]]$schema_version <- 2.0
  expect_error(
    tempest_session_restore(double_expert_schema, config = cfg),
    class = "tempest_session_restore_error"
  )

  extra_expert_field <- snapshot
  extra_expert_field$experts[[1]]$state <- "active"
  expect_error(
    tempest_session_restore(extra_expert_field, config = cfg),
    class = "tempest_session_restore_error"
  )

  mismatched_id <- snapshot
  mismatched_id$session_id <- "replacement-session"
  expect_error(
    tempest_session_restore(mismatched_id, config = cfg),
    class = "tempest_session_restore_error"
  )

  wrong_mode <- snapshot
  wrong_mode$research_manifest$mode <- "storm"
  expect_error(
    tempest_session_restore(wrong_mode, config = cfg),
    class = "tempest_session_restore_error"
  )

  terminal <- snapshot
  terminal$research_manifest$status <- "succeeded"
  expect_error(
    tempest_session_restore(terminal, config = cfg),
    class = "tempest_session_restore_error"
  )

  changed_cfg <- tempest_config(
    max_sources = cfg@max_sources + 1L,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expect_error(
    tempest_session_restore(snapshot, config = changed_cfg),
    class = "tempest_session_restore_error"
  )

  mismatched_snapshot <- snapshot
  mismatched_snapshot$workspace$base_snapshot_id <- "snapshot-b"
  expect_error(
    tempest_session_restore(mismatched_snapshot, config = cfg),
    class = "tempest_session_restore_error"
  )

  downgraded_workspace <- snapshot
  downgraded_workspace$workspace$schema_version <- 4L
  expect_error(
    tempest_session_restore(downgraded_workspace, config = cfg),
    class = "tempest_session_restore_error"
  )

  arbitrary <- snapshot
  arbitrary$artifacts <- list(client = new.env(parent = emptyenv()))
  expect_error(
    tempest_session_restore(arbitrary, config = cfg),
    class = "tempest_session_restore_error"
  )

  missing_transcript <- snapshot
  missing_transcript$transcript <- NULL
  expect_error(
    tempest_session_restore(missing_transcript, config = cfg),
    class = "tempest_session_restore_error"
  )

  generic_catalog <- snapshot
  generic_catalog$artifact_catalog <- list()
  expect_error(
    tempest_session_restore(generic_catalog, config = cfg),
    class = "tempest_session_restore_error"
  )

  generic_workflow <- snapshot
  generic_workflow$workflow_run <- list()
  expect_error(
    tempest_session_restore(generic_workflow, config = cfg),
    class = "tempest_session_restore_error"
  )

  invalid_title <- snapshot
  invalid_title$title <- new.env(parent = emptyenv())
  expect_error(
    tempest_session_restore(invalid_title, config = cfg),
    class = "tempest_session_restore_error"
  )

  expect_false("config" %in% names(snapshot))

  invalid_transcript <- snapshot
  invalid_transcript$transcript <- list(list(
    speaker = "User",
    role = "user",
    text = new.env(parent = emptyenv()),
    at = "2026-08-15T00:00:00.000000Z"
  ))
  expect_error(
    tempest_session_restore(invalid_transcript, config = cfg),
    class = "tempest_session_restore_error"
  )

  invalid_mindmap <- snapshot
  invalid_mindmap$mindmap$nodes[[1]]$notes <- new.env(parent = emptyenv())
  expect_error(
    tempest_session_restore(invalid_mindmap, config = cfg),
    class = "tempest_session_restore_error"
  )

  missing_source_ids <- snapshot
  missing_source_ids$mindmap$nodes[[1]]$source_ids <- NULL
  expect_error(
    tempest_session_restore(missing_source_ids, config = cfg),
    class = "tempest_session_restore_error"
  )

  null_source_ids <- snapshot
  null_source_ids$mindmap$nodes[[1]]["source_ids"] <- list(NULL)
  expect_error(
    tempest_session_restore(null_source_ids, config = cfg),
    class = "tempest_session_restore_error"
  )

  null_questions <- snapshot
  null_questions["suggested_questions"] <- list(NULL)
  expect_error(
    tempest_session_restore(null_questions, config = cfg),
    class = "tempest_session_restore_error"
  )

  scalar_questions <- snapshot
  scalar_questions$suggested_questions <- "What next?"
  expect_error(
    tempest_session_restore(scalar_questions, config = cfg),
    class = "tempest_session_restore_error"
  )

  cyclic_mindmap <- snapshot
  cyclic_mindmap$mindmap$nodes <- c(
    cyclic_mindmap$mindmap$nodes,
    list(
      list(
        id = "cycle-a",
        label = "Cycle A",
        parent = "cycle-b",
        notes = "",
        source_ids = character()
      ),
      list(
        id = "cycle-b",
        label = "Cycle B",
        parent = "cycle-a",
        notes = "",
        source_ids = character()
      )
    )
  )
  expect_error(
    tempest_session_restore(cyclic_mindmap, config = cfg),
    class = "tempest_session_restore_error"
  )

  unknown_source <- snapshot
  unknown_source$mindmap$nodes[[1]]$source_ids <- "source.unknown"
  expect_error(
    tempest_session_restore(unknown_source, config = cfg),
    class = "tempest_session_restore_error"
  )

  unknown_edge <- snapshot
  unknown_edge$mindmap$edges <- list(list(
    from = "root",
    to = "missing",
    relation = "subtopic"
  ))
  expect_error(
    tempest_session_restore(unknown_edge, config = cfg),
    class = "tempest_session_restore_error"
  )

  invalid_title <- snapshot
  invalid_title$title <- 1L
  expect_error(
    tempest_session_restore(invalid_title, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("schema 10 progress history is exact and session-bound", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Bound progress",
    config = cfg,
    experts = list(tempest_expert(
      name = "Bound Progress Expert",
      title = "Persistence analyst",
      description = "Checks progress correlation.",
      instructions = "Reject ambiguous progress history."
    )),
    session_id = "bound-progress"
  )
  snapshot <- tempest_session_snapshot(session)
  event <- snapshot$progress_events[[1]]
  expect_identical(
    names(event),
    tempest:::tempest_session_progress_event_fields()
  )
  expect_r6_class(
    tempest_session_restore(snapshot, config = cfg),
    "TempestSession"
  )
  expect_invalid <- function(events) {
    candidate <- snapshot
    candidate$progress_events <- events
    expect_error(
      tempest_session_restore(candidate, config = cfg),
      class = "tempest_session_restore_error"
    )
  }

  reordered <- event[rev(names(event))]
  expect_invalid(list(reordered))

  missing_field <- event
  missing_field$message <- NULL
  expect_invalid(list(missing_field))

  extra_field <- event
  extra_field$legacy <- "value"
  expect_invalid(list(extra_field))

  wrong_sequence <- event
  wrong_sequence$sequence <- 2L
  expect_invalid(list(wrong_sequence))

  double_sequence <- event
  double_sequence$sequence <- 1.0
  expect_invalid(list(double_sequence))

  wrong_run <- event
  wrong_run$run_id <- "other-session"
  expect_invalid(list(wrong_run))

  wrong_workflow <- event
  wrong_workflow$workflow <- "storm"
  expect_invalid(list(wrong_workflow))

  runtime_payload <- event
  runtime_payload$payload <- list(client = new.env(parent = emptyenv()))
  expect_invalid(list(runtime_payload))

  duplicate <- event
  duplicate$sequence <- 2L
  expect_invalid(list(event, duplicate))

  bundle_dir <- file.path(withr::local_tempdir(), "progress-bundle")
  tempest_session_save(session, bundle_dir)
  events_path <- file.path(bundle_dir, "progress_events.json")
  events <- tempest:::tempest_product_read_json(events_path)
  events[[1]]$run_id <- "other-session"
  tempest:::tempest_product_write_json(events_path, events)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["progress_events.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      bundle_dir,
      "progress_events.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg
    ),
    class = "tempest_session_restore_error"
  )
})
