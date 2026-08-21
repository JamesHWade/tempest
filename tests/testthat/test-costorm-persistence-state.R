test_that("TempestSession snapshots restore durable session state", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- tempest_research_workspace()
  expert <- tempest_expert(
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
    restored$workspace$upsert_retrieved_resource(fake_source(
      url = "https://example.com/injected-after-session-restore"
    )),
    class = "tempest_research_workspace_error"
  )
  expect_equal(snapshot$schema_version, 10L)
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
  expect_equal(snapshot$experts[[1]]$expert_id, expert@expert_id)
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

test_that("retired experts persist separately from immutable profiles", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  experts <- list(
    tempest_expert(
      name = "Retired Alpha",
      title = "Retirement analyst",
      description = "Checks the first durable retirement entry.",
      instructions = "Preserve the authored profile."
    ),
    tempest_expert(
      name = "Retired Beta",
      title = "Roster analyst",
      description = "Checks the second durable retirement entry.",
      instructions = "Keep retirement outside the profile."
    ),
    tempest_expert(
      name = "Active Gamma",
      title = "Active roster analyst",
      description = "Remains available after restore.",
      instructions = "Remain active."
    )
  )
  session <- tempest_session(
    "Retired roster persistence",
    config = cfg,
    experts = experts
  )
  manager <- tempest:::tempest_session_expert_manager(session)
  retired_ids <- sort(vapply(experts[1:2], \(x) x@expert_id, character(1)))
  for (expert_id in rev(retired_ids)) {
    expect_identical(manager$retire_expert(expert_id), TRUE)
  }

  snapshot <- tempest_session_snapshot(session)
  expect_identical(snapshot$retired_expert_ids, as.list(retired_ids))
  expect_false(any(vapply(
    snapshot$experts,
    \(record) any(c("state", "metadata") %in% names(record)),
    logical(1)
  )))

  expect_retirement_error <- function(value) {
    candidate <- snapshot
    candidate["retired_expert_ids"] <- list(value)
    expect_error(
      tempest_session_restore(candidate, config = cfg),
      class = "tempest_session_restore_error"
    )
  }
  expect_retirement_error(NULL)
  expect_retirement_error(retired_ids[[1]])
  expect_retirement_error(stats::setNames(as.list(retired_ids), retired_ids))
  expect_retirement_error(list("malformed id"))
  expect_retirement_error(as.list(c(retired_ids, retired_ids[[1]])))
  expect_retirement_error(as.list(rev(retired_ids)))
  expect_retirement_error(list("expert.unknown"))

  restored <- tempest_session_restore(snapshot, config = cfg)
  restored_manager <- tempest:::tempest_session_expert_manager(restored)
  expect_identical(restored_manager$list_retired_expert_ids(), retired_ids)
  expect_identical(
    sort(vapply(restored$get_active_experts(), \(x) x@expert_id, character(1))),
    setdiff(
      sort(vapply(experts, \(x) x@expert_id, character(1))),
      retired_ids
    )
  )

  revived_id <- retired_ids[[1]]
  profile_before <- restored_manager$profile(
    revived_id,
    active_only = FALSE
  )
  profile_record <- tempest:::tempest_expert_profile_record(profile_before)
  expect_identical(
    restored_manager$add_expert(profile_before, replace = TRUE),
    revived_id
  )
  expect_identical(
    restored_manager$list_retired_expert_ids(),
    setdiff(retired_ids, revived_id)
  )
  expect_identical(
    tempest:::tempest_expert_profile_record(restored_manager$profile(
      revived_id
    )),
    profile_record
  )

  bundle_dir <- file.path(withr::local_tempdir(), "retired-roster")
  tempest_session_save(session, bundle_dir)
  manifest <- tempest:::tempest_product_read_json(
    file.path(bundle_dir, "session.json")
  )
  manifest_files <- unlist(manifest$files, use.names = FALSE)
  expect_contains(manifest_files, "retired_expert_ids.json")
  expect_named(manifest$checksums, manifest_files, ignore.order = TRUE)
  expect_identical(
    tempest:::tempest_product_read_json(
      file.path(bundle_dir, "retired_expert_ids.json")
    ),
    as.list(retired_ids)
  )
  resumed <- tempest_session_resume(bundle_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_expert_manager(
      resumed
    )$list_retired_expert_ids(),
    retired_ids
  )
})
