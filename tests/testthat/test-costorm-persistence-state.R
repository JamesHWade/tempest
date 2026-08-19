test_that("TempestSession snapshots restore durable session state", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- tempest_research_workspace()
  expert <- tempest_expert(
    expert_id = "expert.snapshot",
    name = "Dr. Snapshot",
    title = "Persistence expert",
    description = "Durable session state",
    instructions = "Identify the state needed for a faithful restart.",
    initial_questions = "What should be persisted?"
  )
  session <- tempest:::TempestSession$new(
    "Session persistence",
    config = cfg,
    experts = list(expert),
    retriever = tempest_retriever(config = cfg, workspace = store),
    session_id = "session_snapshot"
  )
  programs <- tempest:::tempest_program_set_manifest_programs(
    tempest:::tempest_session_program_set(session)
  )
  fixture <- test_add_verifiable_claim(
    store,
    key = "session-snapshot",
    claim_text = "Session snapshots preserve claims.",
    quote = "Session snapshots preserve claims.",
    extracted_by = programs$extract_claims$program_artifact_id
  )
  claim_id <- fixture$claim@claim_id
  store$verify_proposed_claims_batch(
    list(test_claim_support(fixture$claim, fixture$span)),
    verified_at = "2026-08-16T00:00:00Z",
    verifier = programs$verify_claim_support$program_artifact_id,
    .verification_owner_token = tempest:::tempest_session_verification_owner_token(
      session
    )
  )
  session$add_turn("User", "user", "What is durable?")
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Session persistence",
      notes = "Durable state",
      source_ids = list()
    )),
    edges = list()
  )
  tempest:::tempest_session_restore_product_state(
    session,
    title = "Session persistence report",
    transcript = session$transcript,
    mindmap = mindmap,
    events = session$events,
    progress = NULL
  )
  report_md <- tempest_report_md(
    title = session$title,
    body = "Restored report",
    workspace = store,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  report_md <- test_persistence_commit_costorm_report(session, report_md)
  tempest:::tempest_session_set_suggestions(session, c("Q1", "Q2"))
  expert_session <- tempest:::tempest_session_expert_manager(
    session
  )$get_or_create(
    expert@expert_id
  )
  expert_session_id <- expert_session$session_id

  snapshot <- tempest:::tempest_session_snapshot(session)
  reordered_snapshot <- snapshot[rev(names(snapshot))]
  expect_error(
    tempest_session_restore(reordered_snapshot, config = cfg),
    class = "tempest_session_restore_error"
  )
  reordered_manifest <- snapshot
  reordered_manifest$research_manifest <-
    reordered_manifest$research_manifest[
      rev(names(reordered_manifest$research_manifest))
    ]
  expect_error(
    tempest_session_restore(reordered_manifest, config = cfg),
    class = "tempest_session_restore_error"
  )
  restore_snapshot <- snapshot
  collector <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest:::tempest_session_restore_internal(
    restore_snapshot,
    config = cfg,
    progress = collector$record
  )

  expect_r6_class(restored, "TempestSession")
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(restored$workspace),
    "sealed"
  )
  expect_error(
    restored$workspace$upsert_retrieved_resource(tempest:::tempest_source(
      "https://example.com/injected-after-session-restore"
    )),
    class = "tempest_research_workspace_error"
  )
  expect_equal(snapshot$schema_version, 9L)
  expect_identical(
    snapshot$research_manifest$research_run_id,
    snapshot$session_id
  )
  expect_identical(snapshot$research_manifest$mode, "costorm")
  expect_identical(snapshot$research_manifest$status, "succeeded")
  expect_type(snapshot$workspace, "list")
  expect_named(snapshot, tempest:::tempest_session_snapshot_fields())
  expect_length(snapshot$stage_records, 3L)
  expect_identical(snapshot$report_md, report_md)
  expect_equal(snapshot$experts[[1]]$expert_id, "expert.snapshot")
  expect_match(snapshot$experts[[1]]$fingerprint, "^[a-f0-9]{64}$")
  expect_equal(
    S7::S7_inherits(snapshot$experts[[1]], TempestExpertProfile),
    FALSE
  )
  expect_equal(restored$session_id, "session_snapshot")
  expect_identical(
    tempest:::tempest_stage_records_data(
      tempest:::tempest_session_stage_records(restored)
    ),
    snapshot$stage_records
  )
  expect_equal(restored$title, "Session persistence report")
  expect_equal(restored$transcript[[1]]$text, "What is durable?")
  expect_equal(restored$mindmap$nodes[[1]]$notes, "Durable state")
  expect_identical(snapshot$mindmap$nodes[[1]]$source_ids, list())
  expect_identical(snapshot$suggested_questions, list("Q1", "Q2"))
  expect_identical(tempest_session_report_md(restored), report_md)
  expect_equal(
    tempest:::tempest_session_suggestions(restored),
    c("Q1", "Q2")
  )
  expect_equal(
    restored$workspace$get_proposed_claim(claim_id)@claim_text,
    "Session snapshots preserve claims."
  )
  expect_equal(
    tempest:::tempest_session_expert_manager(restored)$list_sessions(),
    expert_session_id
  )
  expert <- tempest:::tempest_session_expert_manager(restored)$get_or_create(
    restored$experts[[1]]@expert_id,
    session_id = expert_session_id
  )
  expect_equal(expert$is_new, FALSE)

  expect_length(collector$events(), 0)
  restored$emit_progress(
    "workflow",
    "succeeded",
    stage = "session",
    step = "test"
  )
  expect_equal(collector$data()[[1]]$run_id, "session_snapshot")
})
