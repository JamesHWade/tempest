test_that("tempest_prepare_run_dir creates topic slug directory", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(
    root,
    "A Topic: With Punctuation!"
  )

  expect_equal(basename(run_dir), "a-topic-with-punctuation")
  expect_equal(dir.exists(run_dir), TRUE)
})

test_that("ResearchWorkspace snapshots restore artifact-free product state", {
  skip_if_not_installed("jsonlite")
  workspace <- tempest_research_workspace(
    base_snapshot_id = "snapshot-a",
    max_sources = 4L,
    accepted_graft_references = list(
      list(record_id = "record-z", revision_id = "revision-2"),
      list(record_id = "record-a", revision_id = "revision-1")
    )
  )
  source <- tempest:::tempest_source(
    "https://example.com/workspace",
    title = "Workspace source",
    snippet = "Workspace snippet"
  )
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "protocols/workspace",
    title = "Workspace protocol",
    media_type = "text/plain",
    content = "Preserve the provisional evidence ledger.",
    retrieved_at = "2026-08-15T12:00:00Z",
    metadata = list(revision = "reviewed")
  )
  workspace$upsert_retrieved_resource(source)
  workspace$upsert_retrieved_resource(resource)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-a",
    source_id = source$id,
    quote = "workspace evidence"
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-a",
    claim_text = "Workspaces preserve provisional evidence.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    confidence = "high",
    verification_status = "supported",
    support_score = 0.95
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-a",
    topic = "workspace persistence",
    claim_ids = claim_id,
    evidence_balance = "agreement"
  ))
  workspace$set_citation_audit(tibble::tibble(
    claim_id = claim_id,
    claim_text = "Workspaces preserve provisional evidence.",
    verification_status = "supported",
    support_score = 0.95,
    rationale = "Direct support"
  ))

  now_calls <- 0L
  local_mocked_bindings(
    tempest_now_utc = function() {
      now_calls <<- now_calls + 1L
      "2099-01-01T00:00:00Z"
    }
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  snapshot_again <- tempest:::tempest_research_workspace_snapshot(workspace)

  expect_identical(snapshot, snapshot_again)
  expect_identical(now_calls, 0L)
  expect_identical(snapshot$schema_version, 4L)
  expect_identical(snapshot$max_sources, 4L)
  expect_named(
    snapshot,
    c(
      "schema_version",
      "base_snapshot_id",
      "max_sources",
      "accepted_graft_references",
      "retrieved_resources",
      "proposed_claims",
      "evidence_spans",
      "disputes",
      "citation_audit"
    )
  )
  expect_equal("artifacts" %in% names(snapshot), FALSE)
  expect_length(snapshot$retrieved_resources, 2L)
  resource_ids <- vapply(
    snapshot$retrieved_resources,
    `[[`,
    character(1),
    "resource_id"
  )
  expect_contains(resource_ids, c(resource@resource_id, source$id))
  expect_equal(
    vapply(
      snapshot$accepted_graft_references,
      `[[`,
      character(1),
      "record_id"
    ),
    c("record-a", "record-z")
  )

  path <- withr::local_tempfile(fileext = ".json")
  tempest:::tempest_write_json(path, snapshot)
  restored <- tempest:::tempest_research_workspace_restore(
    tempest:::tempest_read_json_strict(path)
  )
  restored_snapshot <- tempest:::tempest_research_workspace_snapshot(restored)

  expect_identical(restored_snapshot, snapshot)
  expect_r6_class(restored, "ResearchWorkspace")
  expect_identical(restored$base_snapshot_id, "snapshot-a")
  expect_equal(restored$max_sources, 4L)
  expect_equal(
    restored$list_accepted_graft_references(),
    workspace$list_accepted_graft_references()
  )
  expect_equal(
    restored$get_retrieved_source(source$id)$title,
    "Workspace source"
  )
  expect_equal(
    restored$get_proposed_claim(claim_id)@claim_text,
    "Workspaces preserve provisional evidence."
  )
  expect_equal(
    restored$get_evidence_for_proposed_claim(claim_id)[[1]]@quote,
    "workspace evidence"
  )
  expect_equal(restored$list_disputes()[[1]]@dispute_id, "dispute-a")
  expect_equal(restored$citation_audit$support_score, 0.95)
  expect_equal("artifacts" %in% names(restored), FALSE)

  workspace$record_accepted_graft_reference(list(record_id = "record-new"))
  workspace$upsert_retrieved_resource(tempest:::tempest_source(
    "https://example.com/workspace",
    title = "Changed after snapshot"
  ))
  current_audit <- workspace$citation_audit
  current_audit$support_score <- 0.25
  expect_equal(length(snapshot$accepted_graft_references), 2L)
  titles <- vapply(
    snapshot$retrieved_resources,
    `[[`,
    character(1),
    "title"
  )
  expect_contains(titles, c("Workspace source", "Workspace protocol"))
  expect_equal(snapshot$citation_audit[[1]]$support_score, 0.95)

  snapshot$accepted_graft_references[[1]]$record_id <- "tampered"
  snapshot$retrieved_resources[[1]]$title <- "Tampered snapshot"
  expect_equal(
    restored$accepted_graft_references[[1]]$record_id,
    "record-a"
  )
  expect_equal(
    restored$get_retrieved_source(source$id)$title,
    "Workspace source"
  )
})

test_that("ResearchWorkspace rejects pre-hard-cut snapshots", {
  expect_error(
    tempest:::tempest_research_workspace_restore(list(schema_version = 3L)),
    class = "tempest_unsupported_format_error"
  )
})

test_that("ResearchWorkspace snapshots encode unbounded source limits", {
  skip_if_not_installed("jsonlite")
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    tempest_research_workspace()
  )

  expect_identical(snapshot$max_sources, "unbounded")
  expect_no_error(jsonlite::toJSON(
    snapshot,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  expect_identical(restored$max_sources, Inf)
})

test_that("ResearchWorkspace restore validates schema and pinned state", {
  snapshot <- tempest:::tempest_research_workspace_snapshot(
    tempest_research_workspace(
      base_snapshot_id = "snapshot-a",
      accepted_graft_references = list(list(record_id = "record-a"))
    )
  )
  foreign <- tempest_research_workspace(
    base_snapshot_id = "snapshot-b",
    accepted_graft_references = list(list(record_id = "record-a"))
  )

  expect_error(
    tempest:::tempest_research_workspace_restore(
      snapshot,
      workspace = foreign
    ),
    class = "tempest_research_workspace_restore_error"
  )
  foreign <- tempest_research_workspace(
    base_snapshot_id = "snapshot-a",
    accepted_graft_references = list(list(record_id = "record-b"))
  )
  expect_error(
    tempest:::tempest_research_workspace_restore(
      snapshot,
      workspace = foreign
    ),
    class = "tempest_research_workspace_restore_error"
  )

  malformed <- snapshot
  malformed$retrieved_resources <- list(list(resource_id = "foreign-record"))
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )

  snapshot$artifacts <- list(report_md = "not workspace state")
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$artifacts <- NULL
  snapshot$runtime <- list(client_id = "not workspace state")
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$runtime <- NULL
  snapshot$max_sources <- Inf
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$max_sources <- "unbounded"
  snapshot$schema_version <- 5L
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_research_workspace_restore_error"
  )
})

test_that("ResearchWorkspace restore rejects orphan evidence records", {
  workspace <- tempest_research_workspace(max_sources = 8L)
  source <- tempest:::tempest_source(
    "https://example.com/orphan-integrity",
    title = "Integrity source"
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-integrity",
    source_id = source$id,
    quote = "Evidence remains linked."
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-integrity",
    claim_text = "Evidence records remain linked.",
    source_ids = source$id,
    evidence_span_ids = span_id
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-integrity",
    topic = "Evidence integrity",
    claim_ids = claim_id,
    evidence_balance = "mixed"
  ))
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  orphan_span <- snapshot
  orphan_span$evidence_spans[[1L]]$source_id <- "resource.unknown"
  expect_error(
    tempest:::tempest_research_workspace_restore(orphan_span),
    class = "tempest_research_workspace_restore_error"
  )

  orphan_claim <- snapshot
  orphan_claim$proposed_claims[[1L]]$source_ids <- list("resource.unknown")
  expect_error(
    tempest:::tempest_research_workspace_restore(orphan_claim),
    class = "tempest_research_workspace_restore_error"
  )

  orphan_dispute <- snapshot
  orphan_dispute$disputes[[1L]]$claim_ids <- list("claim.unknown")
  expect_error(
    tempest:::tempest_research_workspace_restore(orphan_dispute),
    class = "tempest_research_workspace_restore_error"
  )
})

test_that("ResearchWorkspace record schemas and identities are exact", {
  workspace <- tempest_research_workspace(max_sources = 8L)
  source <- tempest:::tempest_source(
    "https://example.com/exact-records",
    title = "Exact record source"
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-exact",
    source_id = source$id,
    quote = "Exact evidence"
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-exact",
    claim_text = "Workspace records have exact schemas.",
    source_ids = source$id,
    evidence_span_ids = span_id
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-exact",
    topic = "Exact schemas",
    claim_ids = claim_id
  ))
  workspace$set_citation_audit(tibble::tibble(
    claim_id = claim_id,
    claim_text = "Workspace records have exact schemas.",
    verification_status = "unverified",
    support_score = NA_real_,
    rationale = "Not yet reviewed"
  ))
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  for (field in c(
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "disputes"
  )) {
    duplicated <- snapshot
    duplicated[[field]] <- c(duplicated[[field]], duplicated[[field]][1])
    expect_error(
      tempest:::tempest_research_workspace_restore(duplicated),
      class = "tempest_research_workspace_restore_error"
    )

    missing <- snapshot
    record <- missing[[field]][[1]]
    missing[[field]][[1]] <- record[-1]
    expect_error(
      tempest:::tempest_research_workspace_restore(missing),
      class = "tempest_research_workspace_restore_error"
    )

    extra <- snapshot
    extra[[field]][[1]]$runtime <- "unsupported"
    expect_error(
      tempest:::tempest_research_workspace_restore(extra),
      class = "tempest_research_workspace_restore_error"
    )
  }

  missing_audit <- snapshot
  missing_audit$citation_audit[[1]]$rationale <- NULL
  expect_error(
    tempest:::tempest_research_workspace_restore(missing_audit),
    class = "tempest_research_workspace_restore_error"
  )
  extra_audit <- snapshot
  extra_audit$citation_audit[[1]]$runtime <- "unsupported"
  expect_error(
    tempest:::tempest_research_workspace_restore(extra_audit),
    class = "tempest_research_workspace_restore_error"
  )

  malformed <- snapshot
  malformed$proposed_claims[[1]]$source_ids <- list(list(source$id))
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$proposed_claims[[1]]["created_at"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$evidence_spans[[1]]$start_offset <- 1.5
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$evidence_spans[[1]]["extracted_by"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$disputes[[1]]$claim_ids <- list(list(claim_id))
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
  malformed <- snapshot
  malformed$disputes[[1]]["evidence_balance"] <- list(NULL)
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
    class = "tempest_research_workspace_restore_error"
  )
})

test_that("ResearchWorkspace caller restore is transactional", {
  persisted <- tempest_research_workspace(max_sources = 8L)
  source <- tempest:::tempest_source(
    "https://example.com/transaction-persisted",
    title = "Persisted source"
  )
  persisted$upsert_retrieved_resource(source)
  claim_id <- persisted$add_proposed_claim(tempest_claim(
    claim_id = "claim-transaction",
    claim_text = "Late failures do not mutate caller state.",
    source_ids = source$id
  ))
  persisted$add_dispute(tempest_dispute(
    dispute_id = "dispute-transaction",
    topic = "Transactional restore",
    claim_ids = claim_id,
    evidence_balance = "mixed"
  ))
  malformed <- tempest:::tempest_research_workspace_snapshot(persisted)
  malformed$disputes[[1L]]$claim_ids <- list("claim.unknown")

  caller <- tempest_research_workspace(max_sources = 2L)
  caller$upsert_retrieved_resource(tempest:::tempest_source(
    "https://example.com/transaction-caller",
    title = "Caller source"
  ))
  before <- serialize(
    tempest:::tempest_research_workspace_snapshot(caller),
    NULL,
    version = 3L
  )

  expect_error(
    tempest:::tempest_research_workspace_restore(
      malformed,
      workspace = caller
    ),
    class = "tempest_research_workspace_restore_error"
  )
  after <- serialize(
    tempest:::tempest_research_workspace_snapshot(caller),
    NULL,
    version = 3L
  )

  expect_identical(after, before)
  expect_identical(caller$max_sources, 2L)
})

test_that("TempestSession snapshots restore durable session state", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/session-snapshot",
    title = "Session Snapshot Source"
  )
  store$upsert_retrieved_resource(source)
  claim_id <- store$add_proposed_claim(tempest_claim(
    claim_text = "Session snapshots preserve claims.",
    source_ids = source$id,
    session_id = "session_snapshot"
  ))
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
  session$title <- "Session persistence report"
  session$add_turn("User", "user", "What is durable?")
  session$mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Session persistence",
      notes = "Durable state"
    )),
    edges = list()
  )
  tempest:::tempest_session_set_report_value(session, "# Restored report")
  session$artifacts[["report"]] <- "Legacy report body"
  session$artifacts[["report_md"]] <- "# Restored report"
  session$artifacts[["mindmap_md"]] <- "Legacy mind map"
  session$artifacts[["suggested_questions"]] <- c("Q1", "Q2")
  expert_session <- session$expert_session_manager$get_or_create(
    expert@expert_id
  )
  expert_session_id <- expert_session$session_id

  snapshot <- tempest:::tempest_session_snapshot(session)
  restore_snapshot <- snapshot
  collector <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest:::tempest_session_restore_internal(
    restore_snapshot,
    config = cfg,
    progress = collector$record
  )

  expect_r6_class(restored, "TempestSession")
  expect_equal(snapshot$schema_version, 5L)
  expect_identical(
    snapshot$research_manifest$research_run_id,
    snapshot$session_id
  )
  expect_identical(snapshot$research_manifest$mode, "costorm")
  expect_identical(snapshot$research_manifest$status, "running")
  expect_type(snapshot$workspace, "list")
  expect_equal("artifacts" %in% names(snapshot), FALSE)
  expect_equal("store" %in% names(snapshot), FALSE)
  expect_named(snapshot, tempest:::tempest_session_snapshot_fields())
  expect_false("artifact_catalog" %in% names(snapshot))
  expect_false("workflow_run" %in% names(snapshot))
  expect_identical(snapshot$report_md, "# Restored report")
  expect_equal(snapshot$experts[[1]]$expert_id, "expert.snapshot")
  expect_match(snapshot$experts[[1]]$fingerprint, "^[a-f0-9]{64}$")
  expect_equal(
    S7::S7_inherits(snapshot$experts[[1]], TempestExpertProfile),
    FALSE
  )
  expect_equal(restored$session_id, "session_snapshot")
  expect_equal(restored$title, "Session persistence report")
  expect_equal(restored$transcript[[1]]$text, "What is durable?")
  expect_equal(restored$mindmap$nodes[[1]]$notes, "Durable state")
  expect_null(snapshot$artifacts$report)
  expect_null(snapshot$artifacts$report_md)
  expect_null(snapshot$artifacts$mindmap_md)
  expect_null(restored$artifacts[["report"]])
  expect_null(restored$artifacts[["report_md"]])
  expect_null(restored$artifacts[["mindmap_md"]])
  expect_identical(tempest_session_report_md(restored), "# Restored report")
  expect_equal(restored$artifacts[["suggested_questions"]], c("Q1", "Q2"))
  expect_equal(
    restored$workspace$get_proposed_claim(claim_id)@claim_text,
    "Session snapshots preserve claims."
  )
  expect_equal(
    restored$expert_session_manager$list_sessions(),
    expert_session_id
  )
  expert <- restored$expert_session_manager$get_or_create(
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

test_that("TempestSession restores progress history without replaying it", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  collector <- tempest_progress_collector(include_payload = TRUE)
  expert <- tempest_expert(
    expert_id = "expert.history",
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
  expect_null(snapshot$artifacts$progress_events)
  expect_null(session$artifacts[["progress_events"]])

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
  expect_null(restored$artifacts[["progress_events"]])

  legacy_snapshot <- snapshot
  legacy_snapshot$schema_version <- 4L
  expect_error(
    tempest:::tempest_session_restore(legacy_snapshot, config = cfg),
    class = "tempest_unsupported_format_error"
  )
})

test_that("schema 5 session restore protects research identity", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace()
  session <- tempest_session(
    "Protected session",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.protected-session",
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
  downgraded_workspace$workspace$schema_version <- 3L
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

  session$title <- 1L
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
})

test_that("schema 5 progress history is exact and session-bound", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Bound progress",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.bound-progress",
      name = "Bound Progress Expert",
      title = "Persistence analyst",
      description = "Checks progress correlation.",
      instructions = "Reject ambiguous progress history."
    )),
    session_id = "bound-progress"
  )
  snapshot <- tempest_session_snapshot(session)
  event <- snapshot$progress_events[[1]]
  expect_invalid <- function(events) {
    candidate <- snapshot
    candidate$progress_events <- events
    expect_error(
      tempest_session_restore(candidate, config = cfg),
      class = "tempest_session_restore_error"
    )
  }

  missing_field <- event
  missing_field$message <- NULL
  expect_invalid(list(missing_field))

  extra_field <- event
  extra_field$legacy <- "value"
  expect_invalid(list(extra_field))

  wrong_sequence <- event
  wrong_sequence$sequence <- 2L
  expect_invalid(list(wrong_sequence))

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
  events <- tempest:::tempest_read_json_strict(events_path)
  events[[1]]$run_id <- "other-session"
  tempest:::tempest_write_json(events_path, events)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["progress_events.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "progress_events.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("Tempest session bundles save and resume durable state", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/session-bundle",
    title = "Session Bundle Source"
  )
  store$upsert_retrieved_resource(source)
  claim_id <- store$add_proposed_claim(tempest_claim(
    claim_text = "Bundles preserve claims.",
    source_ids = source$id,
    verification_status = "supported",
    support_score = 0.95
  ))
  store$set_citation_audit(tibble::tibble(
    claim_id = claim_id,
    claim_text = "Bundles preserve claims.",
    verification_status = "supported",
    support_score = 0.95,
    rationale = "test"
  ))
  expert <- tempest_expert(
    expert_id = "expert.bundle",
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
  session_id <- session$session_id
  session$add_turn("User", "user", "Save this session.")
  tempest:::tempest_session_set_report_value(session, "# Bundle report")
  session$artifacts[["suggested_questions"]] <- c("What next?", "And then?")
  session$emit_progress(
    "stage",
    "started",
    stage = "dialogue",
    step = "turn"
  )

  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  saved <- tempest_session_save(session, bundle_dir)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  expect_equal(
    saved,
    normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  )
  expect_null(manifest$status)
  expect_equal(manifest$bundle_type, "costorm")
  expect_equal(manifest$bundle_status, "complete")
  expect_equal(manifest$schema_version, 5L)
  expect_identical(
    manifest$research_manifest$research_run_id,
    session_id
  )
  expect_identical(manifest$research_manifest$mode, "costorm")
  expect_identical(manifest$research_manifest$status, "running")
  expect_identical(
    manifest$workspace$base_snapshot_id,
    session$workspace$base_snapshot_id
  )
  expect_identical(manifest$workspace$schema_version, 4L)
  expect_setequal(names(manifest$checksums), manifest$files)
  expect_contains(
    manifest$files,
    c(
      "experts.json",
      "progress_events.json",
      "workspace/retrieved_resources.json",
      "workspace/proposed_claims.json",
      "workspace/citation_audit.json",
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

  downgraded_manifest <- manifest
  downgraded_manifest$workspace$schema_version <- 3L
  tempest:::tempest_write_json(
    file.path(bundle_dir, "session.json"),
    downgraded_manifest
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  tempest:::tempest_write_json(file.path(bundle_dir, "session.json"), manifest)

  nested_files <- manifest
  nested_files$files[[1]] <- list(nested_files$files[[1]])
  tempest:::tempest_write_json(
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
  tempest:::tempest_write_json(
    file.path(bundle_dir, "session.json"),
    nested_checksums
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  tempest:::tempest_write_json(file.path(bundle_dir, "session.json"), manifest)

  restore_collector <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest_session_resume(
    bundle_dir,
    config = cfg,
    progress = restore_collector$record
  )

  expect_r6_class(restored, "TempestSession")
  expect_equal(restored$session_id, session_id)
  expect_equal(restored$transcript[[1]]$text, "Save this session.")
  expect_null(restored$artifacts[["report"]])
  expect_null(restored$artifacts[["report_md"]])
  expect_null(restored$artifacts[["mindmap_md"]])
  expect_identical(tempest_session_report_md(restored), "# Bundle report")
  expect_length(test_session_artifact_catalog(restored)$list(), 0L)
  expect_equal(
    restored$artifacts[["suggested_questions"]],
    c("What next?", "And then?")
  )
  expect_equal(
    restored$workspace$get_proposed_claim(claim_id)@claim_text,
    "Bundles preserve claims."
  )
  expect_identical(restored$retriever$workspace, restored$workspace)
  expect_equal(nrow(restored$workspace$citation_audit), 1)
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

test_that("schema 4 session bundles fail closed", {
  bundle_dir <- withr::local_tempdir()
  tempest:::tempest_write_json(
    file.path(bundle_dir, "session.json"),
    list(schema_version = 4L)
  )
  expect_error(
    tempest_session_resume(bundle_dir),
    class = "tempest_unsupported_format_error"
  )
})

test_that("session bundles do not persist generic artifact catalogs", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  spec <- tempest_deliverable_spec(
    "generic-output",
    title = "Generic output",
    purpose = "Exercise the removed persistence surface",
    instructions = "Do not persist this generic artifact.",
    generator_id = "host.generate",
    renderer_ids = "host.render",
    media_types = "text/plain"
  )
  artifact <- tempest_artifact(
    spec,
    content = "Ephemeral body",
    artifact_id = "generic-artifact",
    media_type = "text/plain"
  )
  session <- tempest_session(
    "Narrow product persistence",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.narrow-persistence",
      name = "Narrow Persistence Expert",
      title = "Artifact specialist",
      description = "Checks product persistence boundaries.",
      instructions = "Persist only the scientific report product."
    ))
  )
  test_session_artifact_catalog(session)$register(spec)
  test_session_artifact_catalog(session)$add(artifact)
  tempest:::tempest_session_set_report_value(session, "# Durable report")
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  tempest_session_save(session, bundle_dir)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  restored <- tempest_session_resume(bundle_dir, config = cfg)

  expect_false(any(startsWith(
    unlist(manifest$files, use.names = FALSE),
    "artifacts/typed/"
  )))
  expect_false("artifact_files" %in% names(manifest))
  expect_false("artifact_index" %in% names(manifest))
  expect_false("deliverable_index" %in% names(manifest))
  expect_identical(tempest_session_report_md(restored), "# Durable report")
  expect_length(test_session_artifact_catalog(restored)$list(), 0L)
})

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

  session_workspace <- tempest_research_workspace(
    graft_snapshot = graft_snapshot
  )
  session <- tempest_session(
    "Graft session persistence",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.graft-persistence",
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
  tempest:::tempest_save_run_artifacts(
    storm_dir,
    storm_workspace,
    tempest:::tempest_storm_state("Graft STORM persistence"),
    tempest_research_manifest(
      "graft-storm-persistence",
      config = cfg,
      knowledge_snapshot = reference
    ),
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  session_manifest <- tempest:::tempest_read_json_strict(
    file.path(session_dir, "session.json")
  )
  storm_manifest <- tempest:::tempest_read_json_strict(
    file.path(storm_dir, "run_config.json")
  )
  sidecar <- "knowledge/graft-snapshot.rds"
  expect_contains(unlist(session_manifest$files, use.names = FALSE), sidecar)
  expect_contains(unlist(storm_manifest$files, use.names = FALSE), sidecar)
  expect_identical(
    session_manifest$checksums[[sidecar]],
    tempest:::tempest_session_bundle_checksum(session_dir, sidecar)
  )
  expect_identical(
    storm_manifest$checksums[[sidecar]],
    tempest:::tempest_session_bundle_checksum(storm_dir, sidecar)
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
  restored_storm <- tempest:::tempest_load_run_artifacts(
    storm_dir,
    config = cfg,
    run_id = "graft-storm-persistence"
  )
  restored_snapshots <- list(
    restored_memory$workspace$graft_snapshot,
    restored_session$workspace$graft_snapshot,
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
  tempest:::tempest_save_run_artifacts(
    storm_dir,
    tempest_research_workspace(graft_snapshot = snapshot),
    tempest:::tempest_storm_state("Graft sidecar integrity"),
    tempest_research_manifest(
      "graft-sidecar-integrity",
      config = cfg,
      knowledge_snapshot = reference
    ),
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
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
    tempest:::tempest_load_run_artifacts(storm_dir, config = cfg),
    class = "tempest_run_restore_error"
  )
  writeBin(storm_bytes, storm_sidecar)
  writeBin(charToRaw("corrupt snapshot"), storm_sidecar)
  expect_error(
    tempest:::tempest_load_run_artifacts(storm_dir, config = cfg),
    class = "tempest_run_restore_error"
  )
  writeBin(storm_bytes, storm_sidecar)

  session_manifest_path <- file.path(session_dir, "session.json")
  session_manifest <- tempest:::tempest_read_json_strict(
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
    tempest:::tempest_write_json(session_manifest_path, tampered)
    expect_error(
      tempest_session_resume(session_dir, config = cfg),
      class = "tempest_session_restore_error",
      info = field
    )
  }
  tempest:::tempest_write_json(session_manifest_path, session_manifest)

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
    tempest:::tempest_session_bundle_checksum(session_dir, sidecar)
  tempest:::tempest_write_json(session_manifest_path, foreign_manifest)
  expect_error(
    tempest_session_resume(session_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("session bundles exclude process-local runtime registries", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  secret <- "runtime-secret-must-not-be-persisted"
  runtime <- tempest_runtime(
    skill_specs = list(tempest_skill(
      "skill.customer-context",
      purpose = "Interpret customer context",
      instructions = "Apply the customer's terminology."
    )),
    connection_refs = list(tempest_connection_ref(
      "connection.customer-records",
      provider_id = "host.connections",
      connection_type = "customer-records",
      title = "Customer records",
      description = "Host-owned customer context."
    )),
    connection_bindings = list(
      "connection.customer-records" = list(api_key = secret)
    )
  )
  session <- tempest:::TempestSession$new(
    "Customer objective",
    config = cfg,
    runtime = runtime,
    experts = list(tempest_expert(
      expert_id = "expert.customer-context",
      name = "Customer Context Expert",
      title = "Customer context analyst",
      description = "Interprets customer objectives and constraints.",
      instructions = "Use the selected customer-context procedure."
    ))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  snapshot <- tempest_session_snapshot(session)
  tempest_session_save(session, bundle_dir)
  bundle_files <- list.files(bundle_dir, recursive = TRUE)
  bundle_text <- paste(
    vapply(
      file.path(bundle_dir, bundle_files),
      \(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )

  expect_setequal(
    intersect(
      names(snapshot),
      c(
        "skills",
        "connection_refs",
        "connection_permissions",
        "capability_grants"
      )
    ),
    character()
  )
  expect_setequal(
    intersect(
      bundle_files,
      c(
        "skills.json",
        "connection_refs.json",
        "connection_permissions.json",
        "capability_grants.json"
      )
    ),
    character()
  )
  expect_no_match(bundle_text, secret, fixed = TRUE)
  expect_no_match(bundle_text, "api_key", fixed = TRUE)
})

test_that("session restore rejects contract and expert-binding tampering", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  skill <- tempest_skill(
    "skill.tamper-check",
    purpose = "Test contract integrity",
    instructions = "Preserve the exact saved procedure."
  )
  runtime <- tempest_runtime(skill_specs = list(skill))
  expert <- tempest_expert(
    expert_id = "expert.tamper-check",
    name = "Integrity Expert",
    title = "Integrity analyst",
    description = "Checks persisted profile bindings.",
    instructions = "Reject changed profile definitions.",
    skill_ids = skill@skill_id
  )
  session <- tempest:::TempestSession$new(
    "Integrity check",
    config = cfg,
    runtime = runtime,
    experts = list(expert)
  )
  session$expert_session_manager$get_or_create(expert@expert_id)
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)

  experts_path <- file.path(bundle_dir, "experts.json")
  experts <- tempest:::tempest_read_json_strict(experts_path)
  experts[[1]]$fingerprint <- strrep("0", 64)
  tempest:::tempest_write_json(experts_path, experts)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "experts.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest:::tempest_session_resume_internal(
      bundle_dir,
      config = cfg,
      runtime = runtime
    ),
    class = "tempest_session_restore_error"
  )

  runtime_object <- tempest_session_snapshot(session)
  runtime_object$expert_sessions[[1]]$runtime <- new.env(parent = emptyenv())
  expect_error(
    tempest:::tempest_session_restore_internal(
      runtime_object,
      config = cfg,
      runtime = runtime
    ),
    class = "tempest_session_restore_error"
  )

  tamper_expert_sessions <- function(mutate) {
    tempest_session_save(session, bundle_dir, overwrite = TRUE)
    sessions_path <- file.path(bundle_dir, "expert_sessions.json")
    sessions <- tempest:::tempest_read_json_strict(sessions_path)
    sessions[[1]] <- mutate(sessions[[1]])
    tempest:::tempest_write_json(sessions_path, sessions)
    manifest_path <- file.path(bundle_dir, "session.json")
    manifest <- tempest:::tempest_read_json_strict(manifest_path)
    manifest$checksums[["expert_sessions.json"]] <-
      tempest:::tempest_session_bundle_checksum(
        bundle_dir,
        "expert_sessions.json"
      )
    tempest:::tempest_write_json(manifest_path, manifest)
    expect_error(
      tempest:::tempest_session_resume_internal(
        bundle_dir,
        config = cfg,
        runtime = runtime
      ),
      class = "tempest_session_restore_error"
    )
  }

  tamper_expert_sessions(function(binding) {
    binding$model_role <- NULL
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$unexpected <- "runtime"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$model_role <- "writer"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$allowed_connection_ref_ids <- "connection.foreign"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$allowed_connection_ref_ids <- list(list("connection.nested"))
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$created_at <- "not-a-timestamp"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$grants <- list(
      "capability.invalid" = list(status = "granted")
    )
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$grants <- list(
      "capability.nested" = list(
        capability_id = "capability.nested",
        capability_version = "1",
        operation_id = NULL,
        operation_version = NULL,
        required = TRUE,
        status = "denied",
        connection_ref_ids = list(list("connection.nested")),
        reason_code = NULL,
        reason = NULL,
        metadata = list()
      )
    )
    binding
  })

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  sessions_path <- file.path(bundle_dir, "expert_sessions.json")
  sessions <- tempest:::tempest_read_json_strict(sessions_path)
  duplicate <- sessions[[1]]
  duplicate$session_id <- "session.duplicate"
  sessions[[2]] <- duplicate
  tempest:::tempest_write_json(sessions_path, sessions)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["expert_sessions.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "expert_sessions.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_session_resume_internal(
      bundle_dir,
      config = cfg,
      runtime = runtime
    ),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  sessions_path <- file.path(bundle_dir, "expert_sessions.json")
  sessions <- tempest:::tempest_read_json_strict(sessions_path)
  sessions[[1]]$expert_fingerprint <- strrep("f", 64)
  tempest:::tempest_write_json(sessions_path, sessions)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["expert_sessions.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "expert_sessions.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest:::tempest_session_resume_internal(
      bundle_dir,
      config = cfg,
      runtime = runtime
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("session save refuses to overwrite a non-bundle directory", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Guarded save",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.guard",
      name = "Guard Expert",
      title = "Persistence guard",
      description = "Protect bundle replacement.",
      instructions = "Refuse unsafe replacement paths."
    ))
  )

  not_a_bundle <- file.path(withr::local_tempdir(), "important")
  dir.create(not_a_bundle)
  keep <- file.path(not_a_bundle, "keep.txt")
  writeLines("do not delete", keep)

  expect_error(
    tempest_session_save(session, not_a_bundle, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  expect_true(file.exists(keep))
})

test_that("Tempest session bundle resume reports classed file errors", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expert <- tempest_expert(
    expert_id = "expert.broken",
    name = "Dr. Broken",
    title = "Persistence expert",
    description = "Failure handling",
    instructions = "Exercise classed persistence failures."
  )
  session <- tempest_session(
    "Broken bundle",
    config = cfg,
    experts = list(expert)
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)

  unlink(file.path(bundle_dir, "experts.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  writeLines("{", file.path(bundle_dir, "workspace/proposed_claims.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  unlink(file.path(bundle_dir, "transcript.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$schema_version <- 999L
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("failed session replacement preserves the previous bundle", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Original bundle",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.atomic",
      name = "Dr. Atomic",
      title = "Atomic persistence expert",
      description = "Atomic bundle replacement.",
      instructions = "Keep the last complete bundle intact."
    ))
  )
  root <- withr::local_tempdir()
  bundle_dir <- file.path(root, "bundle")
  tempest_session_save(session, bundle_dir)
  session$title <- "Replacement bundle"
  withr::local_options(
    tempest.session_write_hook = function(file) {
      if (identical(file, "workspace/proposed_claims.json")) {
        stop("injected write failure")
      }
    }
  )

  expect_error(
    tempest_session_save(session, bundle_dir, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  restored <- tempest_session_resume(bundle_dir, config = cfg)

  expect_equal(restored$topic, "Original bundle")
  expect_equal(
    list.files(root, pattern = "staging", all.files = TRUE),
    character()
  )
})

test_that("partial session recovery is explicit and skips corrupt optional data", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Partial recovery",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.recovery",
      name = "Dr. Recovery",
      title = "Recovery expert",
      description = "Partial bundle recovery.",
      instructions = "Recover only explicitly optional state."
    ))
  )
  session$artifacts[["suggested_questions"]] <- "What remains?"
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  writeLines("{", questions_path)

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  warnings <- character()
  restored <- withCallingHandlers(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )

  expect_r6_class(restored, "TempestSession")
  expect_identical(
    restored$artifacts[["suggested_questions"]],
    character()
  )
  expect_match(paste(warnings, collapse = "\n"), "incomplete", fixed = TRUE)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  declared <- suppressWarnings(
    tempest:::tempest_session_bundle_validate_manifest(
      bundle_dir,
      manifest,
      partial_recovery = TRUE
    )
  )
  expect_false("artifacts/suggested_questions.json" %in% declared)
})

test_that("partial recovery filters missing and unsafe suggestion files", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Suggestion recovery",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.suggestion-recovery",
      name = "Suggestion Recovery Expert",
      title = "Recovery analyst",
      description = "Tests optional suggestion recovery.",
      instructions = "Keep durable state strict."
    ))
  )
  session$artifacts[["suggested_questions"]] <- "What remains?"
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  unlink(questions_path)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  declared <- suppressWarnings(
    tempest:::tempest_session_bundle_validate_manifest(
      bundle_dir,
      manifest,
      partial_recovery = TRUE
    )
  )
  expect_false("artifacts/suggested_questions.json" %in% declared)

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  external_path <- tempfile("tempest-external-suggestions-")
  writeLines('["Outside bundle"]', external_path)
  unlink(questions_path)
  linked <- file.symlink(external_path, questions_path)
  if (isTRUE(linked)) {
    manifest <- tempest:::tempest_read_json_strict(
      file.path(bundle_dir, "session.json")
    )
    declared <- suppressWarnings(
      tempest:::tempest_session_bundle_validate_manifest(
        bundle_dir,
        manifest,
        partial_recovery = TRUE
      )
    )
    expect_false("artifacts/suggested_questions.json" %in% declared)
  }
})

test_that("partial recovery rejects every non-presentation integrity failure", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Strict durable recovery",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.strict-recovery",
      name = "Strict Recovery Expert",
      title = "Integrity analyst",
      description = "Rejects damage to durable session state.",
      instructions = "Never recover corrupted durable state."
    ))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  critical_files <- c(
    "experts.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json"
  )

  for (critical_file in critical_files) {
    tempest_session_save(
      session,
      bundle_dir,
      overwrite = dir.exists(bundle_dir)
    )
    writeLines("tampered durable state", file.path(bundle_dir, critical_file))
    expect_error(
      tempest_session_resume(
        bundle_dir,
        config = cfg,
        partial_recovery = TRUE
      ),
      class = "tempest_session_restore_error"
    )
  }

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  manifest_path <- file.path(bundle_dir, "session.json")
  claims_path <- file.path(bundle_dir, "workspace/proposed_claims.json")
  writeLines("{", claims_path)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["workspace/proposed_claims.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "workspace/proposed_claims.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  workflow_path <- file.path(bundle_dir, "workflow_run.json")
  tempest:::tempest_write_json(workflow_path, list(schema_version = 2L))
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$files <- sort(c(
    unlist(manifest$files, use.names = FALSE),
    "workflow_run.json"
  ))
  manifest$checksums[["workflow_run.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "workflow_run.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  writeLines("tampered workflow state", workflow_path)
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("session resume rejects files that its manifest does not declare", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Declared inventory",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.inventory",
      name = "Dr. Inventory",
      title = "Inventory expert",
      description = "Manifest-scoped bundle loading.",
      instructions = "Load only files declared by the manifest."
    ))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)
  tempest:::tempest_write_json(
    file.path(bundle_dir, "artifacts/suggested_questions.json"),
    "undeclared"
  )

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("schema 4 run bundles restore workspace, state, and manifest", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Lithium Batteries")
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/source",
    title = "Example Source",
    snippet = "Snippet"
  )
  workspace$upsert_retrieved_resource(source)
  workspace$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Lithium batteries store energy.",
    source_ids = source$id,
    confidence = "high",
    verification_status = "supported",
    support_score = 0.9
  ))
  expert <- tempest_expert(
    expert_id = "expert.technical",
    name = "Dr. Tech",
    title = "Engineer",
    description = "Battery technology and manufacturing.",
    instructions = "Explain technical tradeoffs with source-backed claims."
  )
  state <- tempest:::tempest_storm_state(
    topic = "Lithium Batteries",
    title = "Lithium Batteries",
    perspectives = list(list(
      name = "Technical",
      description = "Technology",
      key_questions = c("How do they work?")
    )),
    experts = list(expert),
    draft_outline = list(
      title = "Lithium Batteries",
      sections = list(list(
        title = "Overview",
        summary = "Summary",
        subsections = list()
      ))
    ),
    outline = list(
      title = "Lithium Batteries",
      sections = list(list(
        title = "Overview",
        summary = "Summary",
        subsections = list()
      ))
    ),
    draft_md = "Draft body",
    report_md = "Polished body",
    completed_stages = c(
      "perspectives",
      "research",
      "outline",
      "write",
      "polish"
    )
  )
  workspace$set_citation_audit(tibble::tibble(
    claim_id = workspace$list_proposed_claims()[[1]]@claim_id,
    claim_text = "Lithium batteries store energy.",
    verification_status = "supported",
    support_score = 0.9,
    rationale = "matches source"
  ))

  cfg <- tempest_config()
  research_manifest <- tempest_research_manifest(
    research_run_id = "lithium-run",
    mode = "storm",
    config = cfg,
    programs = list(
      extract_claims = list(program_artifact_id = "sha256:program")
    ),
    status = "succeeded"
  )
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    research_manifest,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions",
    parallel_writing = TRUE,
    remove_duplicate = TRUE
  )

  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    run_id = "lithium-run"
  )

  expect_equal(
    loaded$completed_stages,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_equal(length(loaded$workspace$list_retrieved_sources()), 1)
  expect_equal(length(loaded$workspace$list_proposed_claims()), 1)
  expect_equal(loaded$state$title, "Lithium Batteries")
  expect_equal(loaded$state$outline$title, "Lithium Batteries")
  expect_equal(loaded$state$draft_md, "Draft body")
  expect_equal(loaded$state$report_md, "Polished body")
  expect_equal(loaded$state$experts[[1]]@expert_id, "expert.technical")
  expect_s7_class(loaded$state$experts[[1]], TempestExpertProfile)
  expect_false("artifact_catalog" %in% names(loaded))
  expect_s3_class(loaded$workspace$citation_audit, "tbl_df")
  expect_equal(nrow(loaded$workspace$citation_audit), 1)
  expect_s7_class(loaded$research_manifest, TempestResearchManifest)
  expect_identical(loaded$research_manifest@research_run_id, "lithium-run")
  expect_identical(
    loaded$research_manifest@programs,
    research_manifest@programs
  )
  expect_null(loaded$workspace$base_snapshot_id)
  expect_equal("artifacts" %in% names(loaded$workspace), FALSE)
  expect_false("parallel_writing" %in% names(loaded$metadata))
  expect_false("remove_duplicate" %in% names(loaded$metadata))
  expect_false(any(startsWith(
    unlist(loaded$metadata$files, use.names = FALSE),
    "artifacts/typed/"
  )))
  expect_equal(loaded$metadata$schema_version, 4L)
  expect_identical(loaded$metadata$bundle_type, "storm")
  expect_identical(loaded$metadata$bundle_status, "complete")
  expect_type(loaded$metadata$research_manifest, "list")
  expect_equal(
    file.exists(file.path(run_dir, "research_manifest.json")),
    FALSE
  )
  expect_equal(file.exists(file.path(run_dir, "experts.json")), TRUE)
  expect_equal(file.exists(file.path(run_dir, "personas.json")), FALSE)
})

test_that("run restore rejects tampered expert-profile records", {
  skip_if_not_installed("jsonlite")
  run_dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  state <- tempest:::tempest_storm_state(
    "Run integrity",
    perspectives = list(list(
      name = "Integrity",
      description = "Persistence integrity",
      key_questions = "Are expert bindings intact?"
    )),
    experts = list(tempest_expert(
      expert_id = "expert.run-integrity",
      name = "Run Integrity Expert",
      title = "Persistence integrity analyst",
      description = "Checks STORM expert records.",
      instructions = "Require exact profile fingerprints."
    )),
    completed_stages = "perspectives"
  )
  manifest <- tempest_research_manifest("run-integrity", config = cfg)
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    config = cfg,
    steps = "perspectives",
    research_strategy = "key_questions"
  )
  experts_path <- file.path(run_dir, "experts.json")
  records <- tempest:::tempest_read_json_strict(experts_path)
  records[[1]]$fingerprint <- strrep("0", 64)
  tempest:::tempest_write_json(experts_path, records)
  manifest_path <- file.path(run_dir, "run_config.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      run_dir,
      "experts.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      run_id = "run-integrity"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("completed stage metadata controls resume state", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Partial Run")
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  state <- tempest:::tempest_storm_state(
    "Partial Run",
    perspectives = list(list(
      name = "Overview",
      description = "General overview",
      key_questions = "What is already known?"
    )),
    experts = list(tempest_expert(
      expert_id = "expert.partial",
      name = "Partial Expert",
      title = "Partial run expert",
      description = "A persisted expert for a partial run.",
      instructions = "Cover the saved perspective."
    )),
    completed_stages = "perspectives"
  )
  manifest <- tempest_research_manifest("partial-run", config = cfg)

  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions"
  )

  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    run_id = "partial-run"
  )

  expect_equal(
    tempest:::tempest_stage_complete(
      loaded$completed_stages,
      "perspectives"
    ),
    TRUE
  )
  expect_equal(
    tempest:::tempest_stage_complete(
      loaded$completed_stages,
      "research"
    ),
    FALSE
  )
})

test_that("completed STORM product state fails closed when artifacts drift", {
  make_bundle <- function() {
    dir <- tempfile("tempest-completed-state-")
    dir.create(dir)
    cfg <- tempest_config()
    outline <- list(
      title = "Durable state",
      sections = list(list(
        title = "Overview",
        summary = "Summary",
        subsections = list()
      ))
    )
    state <- tempest:::tempest_storm_state(
      "Durable state",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "What is authoritative?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.durable-state",
        name = "Durable State Expert",
        title = "Persistence analyst",
        description = "Checks completed product state.",
        instructions = "Reject incomplete persisted stages."
      )),
      draft_outline = outline,
      outline = outline,
      draft_md = "# Draft\n\nDurable draft.",
      report_md = "# Report\n\nDurable report.",
      completed_stages = c(
        "perspectives",
        "research",
        "outline",
        "write",
        "polish"
      )
    )
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      state,
      tempest_research_manifest("completed-state", config = cfg),
      config = cfg,
      steps = state$completed_stages,
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }
  rewrite_checked <- function(bundle, file, value, json = TRUE) {
    path <- file.path(bundle$dir, file)
    if (json) {
      tempest:::tempest_write_json(path, value)
    } else {
      writeLines(value, path)
    }
    manifest_path <- file.path(bundle$dir, "run_config.json")
    manifest <- tempest:::tempest_read_json_strict(manifest_path)
    manifest$checksums[[file]] <-
      tempest:::tempest_session_bundle_checksum(bundle$dir, file)
    tempest:::tempest_write_json(manifest_path, manifest)
  }
  expect_rejected <- function(bundle) {
    expect_error(
      tempest:::tempest_load_run_artifacts(
        bundle$dir,
        config = bundle$config,
        run_id = "completed-state"
      ),
      class = "tempest_run_restore_error"
    )
  }

  bundle <- make_bundle()
  rewrite_checked(bundle, "perspectives.json", list())
  expect_rejected(bundle)

  bundle <- make_bundle()
  perspectives <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "perspectives.json")
  )
  perspectives[[1]]$key_questions <- list(list("What is authoritative?"))
  rewrite_checked(bundle, "perspectives.json", perspectives)
  expect_rejected(bundle)

  bundle <- make_bundle()
  perspectives <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "perspectives.json")
  )
  second <- perspectives[[1]]
  second$name <- "Second"
  rewrite_checked(bundle, "perspectives.json", c(perspectives, list(second)))
  expect_rejected(bundle)

  for (file in c("direct_gen_outline.json", "storm_gen_outline.json")) {
    bundle <- make_bundle()
    rewrite_checked(bundle, file, list())
    expect_rejected(bundle)

    bundle <- make_bundle()
    outline <- tempest:::tempest_read_json_strict(
      file.path(bundle$dir, file)
    )
    outline$sections[[1]]$subsections <- list(list(
      title = "Nested list",
      bullets = list(list("Nested bullet")),
      needed = list("Flat question")
    ))
    rewrite_checked(bundle, file, outline)
    expect_rejected(bundle)
  }

  for (file in c(
    "storm_gen_article.md",
    "storm_gen_article_polished.md"
  )) {
    bundle <- make_bundle()
    rewrite_checked(bundle, file, "", json = FALSE)
    expect_rejected(bundle)
  }
})

test_that("run restore rejects undeclared product files", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  state <- tempest:::tempest_storm_state("t")
  manifest <- tempest_research_manifest("undeclared-run", config = cfg)
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    config = cfg,
    steps = "polish",
    research_strategy = "key_questions"
  )
  writeLines("UNDECLARED", file.path(dir, "storm_gen_article_polished.md"))

  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      run_id = "undeclared-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("run save refuses pre-existing unowned files", {
  dir <- withr::local_tempdir()
  credentials <- file.path(dir, "credentials.json")
  writeLines("user-owned", credentials)
  cfg <- tempest_config()

  expect_error(
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Unowned files"),
      tempest_research_manifest("unowned-files", config = cfg),
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
  expect_identical(readLines(credentials), "user-owned")
  expect_false(file.exists(file.path(dir, "run_config.json")))
})

test_that("run manifest failures are classed and reject escaping symlinks", {
  make_bundle <- function() {
    dir <- tempfile("tempest-run-")
    dir.create(dir)
    cfg <- tempest_config()
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("t"),
      tempest_research_manifest("manifest-run", config = cfg),
      config = cfg,
      steps = "polish",
      research_strategy = "key_questions"
    )
    dir
  }

  missing_checksum_dir <- make_bundle()
  manifest_path <- file.path(missing_checksum_dir, "run_config.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[[manifest$files[[1]]]] <- NULL
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      missing_checksum_dir,
      config = tempest_config(),
      run_id = "manifest-run"
    ),
    class = "tempest_run_restore_error"
  )

  skip_on_os("windows")
  symlink_dir <- make_bundle()
  source_path <- file.path(symlink_dir, "workspace.json")
  outside_path <- tempfile("outside-workspace-", tmpdir = dirname(symlink_dir))
  expect_true(file.copy(source_path, outside_path))
  unlink(source_path)
  expect_true(file.symlink(outside_path, source_path))
  expect_error(
    tempest:::tempest_load_run_artifacts(
      symlink_dir,
      config = tempest_config(),
      run_id = "manifest-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("tempest_atomic_write_lines writes content and leaves no temp files", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "out.txt")

  tempest:::tempest_atomic_write_lines(c("a", "b"), path)
  expect_equal(readLines(path), c("a", "b"))

  tempest:::tempest_atomic_write_lines("c", path)
  expect_equal(readLines(path), "c")

  expect_length(list.files(dir, pattern = "\\.tmp$"), 0L)
})

test_that("the run manifest is written after the artifacts it certifies", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  paths <- tempest:::tempest_run_artifact_paths(dir)
  cfg <- tempest_config()
  state <- tempest:::tempest_storm_state(
    "t",
    outline = list(
      title = "Draft",
      sections = list(list(title = "Draft", summary = "Draft"))
    ),
    draft_md = "# Draft",
    completed_stages = c("research", "write")
  )

  tempest:::tempest_save_run_artifacts(
    dir,
    tempest_research_workspace(),
    state,
    tempest_research_manifest("write-order", config = cfg),
    config = cfg,
    steps = c("research", "write"),
    research_strategy = "key_questions"
  )

  # If the manifest claims "write" is complete, its artifact must exist.
  expect_equal(
    file.exists(c(paths$run_config, paths$draft_md)),
    c(TRUE, TRUE)
  )
})

test_that("references.json holds only the cited sources and reloads", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  s1 <- tempest:::tempest_source(url = "https://example.com/a", title = "A")
  s2 <- tempest:::tempest_source(url = "https://example.com/b", title = "B")
  workspace$upsert_retrieved_resource(s1)
  workspace$upsert_retrieved_resource(s2)
  state <- tempest:::tempest_storm_state(
    "t",
    draft_md = "# Draft",
    report_md = paste0("A cited claim [", s1$id, "]."),
    completed_stages = "polish"
  )

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    tempest_research_manifest("references-run", config = cfg),
    config = cfg,
    steps = "polish",
    research_strategy = "key_questions"
  )

  refs <- tempest:::tempest_read_json_strict(
    file.path(dir, "references.json")
  )
  expect_setequal(vapply(refs, function(r) r$id, character(1)), s1$id)

  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "references-run"
  )
  expect_length(loaded$state$references, 1L)

  refs[[1]]$title <- "Forged title"
  references_path <- file.path(dir, "references.json")
  tempest:::tempest_write_json(references_path, refs)
  manifest_path <- file.path(dir, "run_config.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["references.json"]] <-
    tempest:::tempest_session_bundle_checksum(dir, "references.json")
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      run_id = "references-run"
    ),
    class = "tempest_run_restore_error"
  )

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    tempest_research_manifest("references-run", config = cfg),
    config = cfg,
    steps = "polish",
    research_strategy = "key_questions"
  )
  report_path <- file.path(dir, "storm_gen_article_polished.md")
  writeLines(paste0("Unknown citation [", s2$id, "]."), report_path)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["storm_gen_article_polished.md"]] <-
    tempest:::tempest_session_bundle_checksum(
      dir,
      "storm_gen_article_polished.md"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      run_id = "references-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("schema 3 STORM bundles fail closed", {
  dir <- withr::local_tempdir()
  tempest:::tempest_write_json(
    file.path(dir, "run_config.json"),
    list(schema_version = 3L)
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(dir),
    class = "tempest_unsupported_format_error"
  )
})

test_that("schema 4 resume protects run and config identity", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace()
  manifest <- tempest_research_manifest(
    "protected-run",
    config = cfg,
    programs = list(extract_claims = list(program_artifact_id = "program-a")),
    status = "failed"
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    tempest:::tempest_storm_state("Protected run"),
    manifest,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      run_id = "different-run"
    ),
    class = "tempest_run_restore_error"
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = tempest_config(max_search_results = 4L),
      run_id = "protected-run"
    ),
    class = "tempest_run_restore_error"
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      workspace = tempest_research_workspace(
        base_snapshot_id = "snapshot-b"
      ),
      config = cfg,
      run_id = "protected-run"
    ),
    class = "tempest_run_restore_error"
  )

  restored <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "protected-run"
  )
  expect_identical(restored$research_manifest@status, "failed")
  expect_identical(restored$research_manifest@programs, manifest@programs)
})

test_that("schema 4 STORM bundles round-trip the complete workspace", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace(
    max_sources = 4L,
    accepted_graft_references = list(
      list(record_id = "claim.accepted", revision_id = "revision-7")
    )
  )
  source <- tempest:::tempest_source(
    "https://example.com/current-study",
    title = "Current study",
    snippet = "The study reports a reproducible result."
  )
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "protocols/reviewed-assay",
    title = "Reviewed assay protocol",
    media_type = "text/plain",
    resource_id = "resource.reviewed-assay",
    content = "The assay was reviewed before use.",
    retrieved_at = "2026-08-15T13:00:00Z",
    metadata = list(revision_id = "protocol-9")
  )
  workspace$upsert_retrieved_resource(source)
  workspace$upsert_retrieved_resource(resource)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-reviewed-assay",
    source_id = resource@resource_id,
    quote = "The assay was reviewed before use."
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-complete-workspace",
    claim_text = "The result used a reviewed assay.",
    source_ids = c(resource@resource_id, source$id),
    evidence_span_ids = span_id,
    confidence = "high",
    verification_status = "supported",
    support_score = 0.92
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-complete-workspace",
    topic = "Assay review",
    claim_ids = claim_id,
    evidence_balance = "agreement"
  ))
  workspace$set_citation_audit(tibble::tibble(
    claim_id = claim_id,
    claim_text = "The result used a reviewed assay.",
    verification_status = "supported",
    support_score = 0.92,
    rationale = "The protocol directly supports the claim."
  ))
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    tempest:::tempest_storm_state(
      "Complete workspace",
      completed_stages = "research"
    ),
    tempest_research_manifest(
      "complete-workspace",
      config = cfg
    ),
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  metadata <- tempest:::tempest_read_json_strict(
    file.path(dir, "run_config.json")
  )
  declared <- unlist(metadata$files, use.names = FALSE)
  expect_contains(declared, "workspace.json")
  expect_equal(
    any(c("sources.json", "claims.json", "citation_audit.json") %in% declared),
    FALSE
  )
  expect_equal(file.exists(file.path(dir, "workspace.json")), TRUE)
  expect_equal(file.exists(file.path(dir, "sources.json")), FALSE)
  expect_equal(file.exists(file.path(dir, "claims.json")), FALSE)
  expect_equal(file.exists(file.path(dir, "citation_audit.json")), FALSE)

  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "complete-workspace"
  )

  expect_identical(
    tempest:::tempest_research_workspace_snapshot(loaded$workspace),
    snapshot
  )

  workspace_path <- file.path(dir, "workspace.json")
  downgraded <- tempest:::tempest_read_json_strict(workspace_path)
  downgraded$schema_version <- 3L
  tempest:::tempest_write_json(workspace_path, downgraded)
  metadata$checksums[["workspace.json"]] <-
    tempest:::tempest_session_bundle_checksum(dir, "workspace.json")
  tempest:::tempest_write_json(file.path(dir, "run_config.json"), metadata)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      run_id = "complete-workspace"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("STORM workspace files match the exact manifest identity", {
  make_bundle <- function() {
    dir <- tempfile("tempest-workspace-identity-")
    dir.create(dir)
    cfg <- tempest_config()
    workspace <- tempest_research_workspace(
      max_sources = 4L,
      accepted_graft_references = list(list(
        record_id = "accepted.identity",
        revision_id = "revision-1"
      ))
    )
    tempest:::tempest_save_run_artifacts(
      dir,
      workspace,
      tempest:::tempest_storm_state("Workspace identity"),
      tempest_research_manifest("workspace-identity", config = cfg),
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }
  mutate_workspace <- function(bundle, mutate) {
    workspace_path <- file.path(bundle$dir, "workspace.json")
    workspace <- tempest:::tempest_read_json_strict(workspace_path)
    workspace <- mutate(workspace)
    tempest:::tempest_write_json(workspace_path, workspace)
    manifest_path <- file.path(bundle$dir, "run_config.json")
    manifest <- tempest:::tempest_read_json_strict(manifest_path)
    manifest$checksums[["workspace.json"]] <-
      tempest:::tempest_session_bundle_checksum(
        bundle$dir,
        "workspace.json"
      )
    tempest:::tempest_write_json(manifest_path, manifest)
  }
  expect_rejected <- function(bundle) {
    expect_error(
      tempest:::tempest_load_run_artifacts(
        bundle$dir,
        config = bundle$config,
        run_id = "workspace-identity"
      ),
      class = "tempest_run_restore_error"
    )
  }

  bundle <- make_bundle()
  mutate_workspace(bundle, function(workspace) {
    workspace$max_sources <- 5L
    workspace
  })
  expect_rejected(bundle)

  bundle <- make_bundle()
  mutate_workspace(bundle, function(workspace) {
    workspace$accepted_graft_references[[2]] <- list(
      record_id = "accepted.extra",
      revision_id = "revision-2"
    )
    workspace
  })
  expect_rejected(bundle)

  bundle <- make_bundle()
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "run_config.json")
  )
  names(manifest$workspace) <- c(
    "base_snapshot_id",
    "max_sources",
    "max_sources"
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      manifest,
      bundle$config
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("persistence schema dispatch rejects fractional versions", {
  cfg <- tempest_config()
  session_class <- "tempest_session_restore_error"
  run_class <- "tempest_run_restore_error"

  expect_error(
    tempest_session_restore(list(schema_version = 5.5)),
    class = session_class
  )
  expect_error(
    tempest:::tempest_session_bundle_validate_manifest(
      withr::local_tempdir(),
      list(schema_version = 5.5)
    ),
    class = session_class
  )
  expect_error(
    tempest:::tempest_run_bundle_validate_manifest(
      withr::local_tempdir(),
      list(schema_version = 4.5)
    ),
    class = run_class
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      list(schema_version = 4.5),
      cfg
    ),
    class = run_class
  )
  expect_error(
    tempest:::tempest_storm_restore_manifest(
      list(schema_version = 4.5),
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Fractional schema"),
      cfg,
      withr::local_tempdir()
    ),
    class = run_class
  )
})

test_that("schema 4 STORM manifests have an exact product envelope", {
  make_bundle <- function() {
    dir <- tempfile("tempest-exact-storm-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Exact STORM",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "What is the durable state?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.exact-storm",
        name = "Exact STORM Expert",
        title = "Persistence analyst",
        description = "Checks the exact STORM envelope.",
        instructions = "Reject ambiguous bundle metadata."
      )),
      completed_stages = c("perspectives", "research")
    )
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      state,
      tempest_research_manifest("exact-storm", config = cfg),
      config = cfg,
      steps = c("perspectives", "research"),
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }

  bundle <- make_bundle()
  manifest_path <- file.path(bundle$dir, "run_config.json")
  original <- tempest:::tempest_read_json_strict(manifest_path)
  expect_setequal(
    names(original),
    tempest:::tempest_run_bundle_manifest_fields()
  )

  for (field in c("topic", "title")) {
    invalid <- original
    invalid[[field]] <- NULL
    tempest:::tempest_write_json(manifest_path, invalid)
    expect_error(
      tempest:::tempest_load_run_artifacts(
        bundle$dir,
        config = bundle$config,
        run_id = "exact-storm"
      ),
      class = "tempest_run_restore_error"
    )
  }

  invalid <- original
  invalid$workflow_run <- list()
  tempest:::tempest_write_json(manifest_path, invalid)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      bundle$dir,
      config = bundle$config,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  nested_files <- original
  nested_files$files[[1]] <- list(nested_files$files[[1]])
  tempest:::tempest_write_json(manifest_path, nested_files)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      bundle$dir,
      config = bundle$config,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  nested_checksums <- original
  first_checksum <- names(nested_checksums$checksums)[[1]]
  nested_checksums$checksums[[first_checksum]] <- list(
    nested_checksums$checksums[[first_checksum]]
  )
  tempest:::tempest_write_json(manifest_path, nested_checksums)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      bundle$dir,
      config = bundle$config,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  explicit_empty <- original
  explicit_empty$completed_stages <- character()
  tempest:::tempest_write_json(manifest_path, explicit_empty)
  restored <- tempest:::tempest_load_run_artifacts(
    bundle$dir,
    config = bundle$config,
    run_id = "exact-storm"
  )
  expect_identical(restored$completed_stages, character())
})

test_that("schema 4 STORM declared JSON fails closed", {
  make_bundle <- function() {
    dir <- tempfile("tempest-strict-storm-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Strict STORM",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "Is every artifact valid JSON?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.strict-storm",
        name = "Strict STORM Expert",
        title = "Persistence analyst",
        description = "Checks strict STORM product JSON.",
        instructions = "Reject malformed declared artifacts."
      )),
      completed_stages = "perspectives"
    )
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      state,
      tempest_research_manifest("strict-storm", config = cfg),
      config = cfg,
      steps = "perspectives",
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }

  for (file in c("perspectives.json", "references.json")) {
    bundle <- make_bundle()
    writeLines("{", file.path(bundle$dir, file))
    manifest_path <- file.path(bundle$dir, "run_config.json")
    manifest <- tempest:::tempest_read_json_strict(manifest_path)
    manifest$checksums[[file]] <-
      tempest:::tempest_session_bundle_checksum(bundle$dir, file)
    tempest:::tempest_write_json(manifest_path, manifest)

    expect_error(
      tempest:::tempest_load_run_artifacts(
        bundle$dir,
        config = bundle$config,
        run_id = "strict-storm"
      ),
      class = "tempest_run_restore_error"
    )
  }
})

test_that("schema 4 manifests require files implied by completed stages", {
  make_bundle <- function() {
    dir <- tempfile("tempest-stage-files-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Stage files",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "Which files prove completion?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.stage-files",
        name = "Stage File Expert",
        title = "Persistence reviewer",
        description = "Checks stage-specific persisted product files.",
        instructions = "Require the files certified by completed stages."
      )),
      completed_stages = "perspectives"
    )
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      state,
      tempest_research_manifest("stage-files", config = cfg),
      config = cfg,
      steps = "perspectives",
      research_strategy = "key_questions"
    )
    dir
  }
  remove_perspectives <- function(dir) {
    manifest_path <- file.path(dir, "run_config.json")
    manifest <- tempest:::tempest_read_json_strict(manifest_path)
    manifest$files <- setdiff(
      unlist(manifest$files, use.names = FALSE),
      "perspectives.json"
    )
    manifest$checksums[["perspectives.json"]] <- NULL
    unlink(file.path(dir, "perspectives.json"))
    tempest:::tempest_write_json(manifest_path, manifest)
    dir
  }

  expect_setequal(
    tempest:::tempest_storm_stage_required_files(
      c("perspectives", "research", "outline", "write", "polish")
    ),
    c(
      "perspectives.json",
      "experts.json",
      "workspace.json",
      "direct_gen_outline.json",
      "storm_gen_outline.json",
      "storm_gen_article.md",
      "storm_gen_article_polished.md",
      "references.json"
    )
  )

  current_dir <- remove_perspectives(make_bundle())
  expect_error(
    tempest:::tempest_load_run_artifacts(
      current_dir,
      config = tempest_config(),
      run_id = "stage-files"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("STORM resume accepts only an equivalent supplied workspace", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace(
    max_sources = 8L,
    accepted_graft_references = list(
      list(record_id = "accepted-a", revision_id = "revision-a")
    )
  )
  source <- tempest:::tempest_source(
    "https://example.com/authoritative",
    title = "Authoritative source",
    snippet = "Persisted source"
  )
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "protocols/authoritative",
    title = "Authoritative protocol",
    media_type = "text/plain",
    resource_id = "resource.authoritative",
    content = "The persisted protocol is authoritative.",
    retrieved_at = "2026-08-15T14:00:00Z",
    metadata = list(revision_id = "protocol-a")
  )
  workspace$upsert_retrieved_resource(source)
  workspace$upsert_retrieved_resource(resource)
  claim <- tempest_claim(
    claim_id = "claim-authoritative",
    claim_text = "Persisted state is authoritative.",
    source_ids = source$id,
    confidence = "high",
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  workspace$set_citation_audit(tibble::tibble(
    claim_id = claim@claim_id,
    claim_text = claim@claim_text,
    verification_status = "supported",
    support_score = 0.9,
    rationale = "The persisted source directly supports the claim."
  ))
  manifest <- tempest_research_manifest(
    "authoritative-workspace",
    config = cfg
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    tempest:::tempest_storm_state(
      "Authoritative workspace",
      completed_stages = "research"
    ),
    manifest,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  persisted_record <-
    tempest:::tempest_storm_workspace_equivalence_record(workspace)
  expect_identical(
    tempest:::tempest_storm_workspace_is_empty(workspace),
    FALSE
  )
  equivalent <- tempest:::tempest_research_workspace_restore(snapshot)
  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    workspace = equivalent,
    config = cfg,
    run_id = "authoritative-workspace"
  )
  expect_identical(loaded$workspace, equivalent)
  expect_identical(
    tempest:::tempest_storm_workspace_equivalence_record(loaded$workspace),
    persisted_record
  )

  empty <- tempest_research_workspace(
    max_sources = 2L
  )
  loaded_empty <- tempest:::tempest_load_run_artifacts(
    dir,
    workspace = empty,
    config = cfg,
    run_id = "authoritative-workspace"
  )
  expect_identical(loaded_empty$workspace, empty)
  expect_identical(
    tempest:::tempest_storm_workspace_equivalence_record(empty),
    persisted_record
  )

  fresh_workspace <- function() {
    tempest:::tempest_research_workspace_restore(snapshot)
  }
  extra_source <- fresh_workspace()
  extra_source$upsert_retrieved_resource(tempest:::tempest_source(
    "https://example.com/extra",
    title = "Extra source"
  ))
  changed_source <- fresh_workspace()
  source_record <- changed_source$get_retrieved_resource(source$id)
  source_record <- S7::set_props(source_record, title = "Changed source")
  changed_source$upsert_retrieved_resource(source_record)
  changed_source$set_citation_audit(workspace$citation_audit)
  changed_source_metadata <- fresh_workspace()
  source_record <- changed_source_metadata$get_retrieved_resource(source$id)
  source_record <- S7::set_props(
    source_record,
    metadata = list(revision_id = "changed")
  )
  changed_source_metadata$upsert_retrieved_resource(source_record)
  changed_source_metadata$set_citation_audit(workspace$citation_audit)
  changed_source_timestamp <- fresh_workspace()
  source_record <- changed_source_timestamp$get_retrieved_resource(source$id)
  source_record <- S7::set_props(
    source_record,
    retrieved_at = "2026-08-15T15:00:00Z"
  )
  changed_source_timestamp$upsert_retrieved_resource(source_record)
  changed_source_timestamp$set_citation_audit(workspace$citation_audit)
  changed_resource_metadata <- fresh_workspace()
  resource_record <- changed_resource_metadata$get_retrieved_resource(
    resource@resource_id
  )
  resource_record <- S7::set_props(
    resource_record,
    metadata = list(revision_id = "protocol-b")
  )
  changed_resource_metadata$upsert_retrieved_resource(resource_record)
  changed_resource_metadata$set_citation_audit(workspace$citation_audit)
  changed_resource_timestamp <- fresh_workspace()
  resource_record <- changed_resource_timestamp$get_retrieved_resource(
    resource@resource_id
  )
  resource_record <- S7::set_props(
    resource_record,
    retrieved_at = "2026-08-15T16:00:00Z"
  )
  changed_resource_timestamp$upsert_retrieved_resource(resource_record)
  changed_resource_timestamp$set_citation_audit(workspace$citation_audit)
  extra_claim <- fresh_workspace()
  extra_claim$add_proposed_claim(tempest_claim(
    claim_id = "claim-extra",
    claim_text = "This claim was not persisted.",
    source_ids = source$id
  ))
  extra_span <- fresh_workspace()
  extra_span$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-extra",
    source_id = source$id,
    quote = "This span was not persisted."
  ))
  extra_dispute <- fresh_workspace()
  extra_dispute$add_dispute(tempest_dispute(
    dispute_id = "dispute-extra",
    topic = "Unpersisted dispute",
    claim_ids = claim@claim_id
  ))
  extra_reference <- fresh_workspace()
  extra_reference$record_accepted_graft_reference(list(
    record_id = "accepted-extra",
    revision_id = "revision-extra"
  ))
  changed_audit <- fresh_workspace()
  changed_audit$set_citation_audit(NULL)
  divergent <- list(
    extra_source,
    changed_source,
    changed_source_metadata,
    changed_source_timestamp,
    changed_resource_metadata,
    changed_resource_timestamp,
    extra_claim,
    extra_span,
    extra_dispute,
    extra_reference,
    changed_audit
  )
  for (candidate in divergent) {
    expect_error(
      tempest:::tempest_load_run_artifacts(
        dir,
        workspace = candidate,
        config = cfg,
        run_id = "authoritative-workspace"
      ),
      class = "tempest_run_restore_error"
    )
  }
})
