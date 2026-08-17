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

test_that("durable integer validation accepts the exact non-NA range", {
  for (value in c(
    -.Machine$integer.max,
    0L,
    .Machine$integer.max
  )) {
    expect_identical(
      tempest:::tempest_exact_integer_scalar(value, "test integer"),
      value
    )
  }
  expect_error(
    tempest:::tempest_exact_integer_scalar(0.0, "test integer"),
    class = "tempest_error"
  )
  expect_error(
    tempest:::tempest_exact_integer_scalar(NA_integer_, "test integer"),
    class = "tempest_error"
  )
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
    snippet = "Workspace snippet",
    content_text = "workspace evidence"
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
    supporting_quotes = list("workspace evidence"),
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
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source$id,
      verification_status = "supported",
      support_score = 0.95,
      rationale = "Direct support"
    )),
    verified_at = "2026-08-16T12:03:00Z"
  )

  now_calls <- 0L
  local_mocked_bindings(
    tempest_now_utc = function() {
      now_calls <<- now_calls + 1L
      "2099-01-01T00:00:00Z"
    }
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  for (field in c(
    "source_ids",
    "evidence_span_ids",
    "supporting_quotes",
    "contradicting_source_ids"
  )) {
    expect_type(snapshot$proposed_claims[[1]][[field]], "list")
  }
  expect_type(snapshot$disputes[[1]]$claim_ids, "list")
  expect_type(snapshot$disputes[[1]]$unresolved_questions, "list")
  snapshot_again <- tempest:::tempest_research_workspace_snapshot(workspace)

  expect_identical(snapshot, snapshot_again)
  expect_identical(now_calls, 0L)
  expect_identical(snapshot$schema_version, 5L)
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
      "claim_supports",
      "disputes"
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
  sealed_before <- tempest:::tempest_research_workspace_snapshot(workspace)
  expect_error(
    workspace$upsert_retrieved_resource(tempest:::tempest_source(
      "https://example.com/workspace",
      title = "Changed after snapshot",
      content_text = "workspace evidence"
    )),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(workspace),
    sealed_before
  )
  expect_equal(length(snapshot$accepted_graft_references), 2L)
  titles <- vapply(
    snapshot$retrieved_resources,
    `[[`,
    character(1),
    "title"
  )
  expect_contains(titles, c("Workspace source", "Workspace protocol"))
  expect_equal(snapshot$claim_supports[[1]]$support_score, 0.95)

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
    tempest:::tempest_research_workspace_restore(list(schema_version = 4L)),
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
  double_schema <- snapshot
  double_schema$schema_version <- 5.0
  expect_error(
    tempest:::tempest_research_workspace_restore(double_schema),
    class = "tempest_research_workspace_restore_error"
  )
  double_max_sources <- tempest:::tempest_research_workspace_snapshot(
    tempest_research_workspace(max_sources = 8L)
  )
  double_max_sources$max_sources <- 8.0
  expect_error(
    tempest:::tempest_research_workspace_restore(double_max_sources),
    class = "tempest_research_workspace_restore_error"
  )
  snapshot$schema_version <- 4L
  expect_error(
    tempest:::tempest_research_workspace_restore(snapshot),
    class = "tempest_unsupported_format_error"
  )
})

test_that("ResearchWorkspace restore rejects orphan evidence records", {
  workspace <- tempest_research_workspace(max_sources = 8L)
  source <- tempest:::tempest_source(
    "https://example.com/orphan-integrity",
    title = "Integrity source",
    content_text = "Evidence remains linked."
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
    evidence_span_ids = span_id,
    supporting_quotes = list("Evidence remains linked.")
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
    title = "Exact record source",
    content_text = "Exact evidence"
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
    evidence_span_ids = span_id,
    supporting_quotes = list("Exact evidence")
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-exact",
    topic = "Exact schemas",
    claim_ids = claim_id
  ))
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source$id,
      verification_status = "unverifiable",
      support_score = NA_real_,
      rationale = "Not yet reviewed"
    )),
    verified_at = "2026-08-16T12:03:00Z"
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  for (field in c(
    "retrieved_resources",
    "proposed_claims",
    "evidence_spans",
    "claim_supports",
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

  resource_schema_values <- list(
    null = NULL,
    string = "1",
    unknown = 999L
  )
  for (name in names(resource_schema_values)) {
    malformed <- snapshot
    malformed$retrieved_resources[[1]]["schema_version"] <- list(
      resource_schema_values[[name]]
    )
    malformed$retrieved_resources[[1]]$fingerprint <-
      tempest:::tempest_resource_fingerprint(
        malformed$retrieved_resources[[1]]
      )
    expect_error(
      tempest:::tempest_research_workspace_restore(malformed),
      class = "tempest_research_workspace_restore_error",
      info = name
    )
  }

  missing_support <- snapshot
  missing_support$claim_supports[[1]]$rationale <- NULL
  expect_error(
    tempest:::tempest_research_workspace_restore(missing_support),
    class = "tempest_research_workspace_restore_error"
  )
  extra_support <- snapshot
  extra_support$claim_supports[[1]]$runtime <- "unsupported"
  expect_error(
    tempest:::tempest_research_workspace_restore(extra_support),
    class = "tempest_research_workspace_restore_error"
  )

  malformed <- snapshot
  malformed$proposed_claims[[1]]$source_ids <- source$id
  expect_error(
    tempest:::tempest_research_workspace_restore(malformed),
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
  malformed$evidence_spans[[1]]$start_offset <- 1.0
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
  malformed$disputes[[1]]$claim_ids <- claim_id
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
    source_ids = source$id
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
  report_md <- tempest_report_md(
    title = session$title,
    body = "Restored report",
    workspace = store,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  tempest:::tempest_session_set_report_value(session, report_md)
  session$artifacts[["report"]] <- "Legacy report body"
  session$artifacts[["report_md"]] <- report_md
  session$artifacts[["mindmap_md"]] <- "Legacy mind map"
  session$artifacts[["suggested_questions"]] <- c("Q1", "Q2")
  expert_session <- session$expert_session_manager$get_or_create(
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
  expect_equal(snapshot$schema_version, 9L)
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
  expect_identical(snapshot$stage_records, list())
  expect_false("artifact_catalog" %in% names(snapshot))
  expect_false("workflow_run" %in% names(snapshot))
  expect_identical(snapshot$report_md, report_md)
  expect_equal(snapshot$experts[[1]]$expert_id, "expert.snapshot")
  expect_match(snapshot$experts[[1]]$fingerprint, "^[a-f0-9]{64}$")
  expect_equal(
    S7::S7_inherits(snapshot$experts[[1]], TempestExpertProfile),
    FALSE
  )
  expect_equal(restored$session_id, "session_snapshot")
  expect_identical(tempest:::tempest_session_stage_records(restored), list())
  expect_equal(restored$title, "Session persistence report")
  expect_equal(restored$transcript[[1]]$text, "What is durable?")
  expect_equal(restored$mindmap$nodes[[1]]$notes, "Durable state")
  expect_identical(snapshot$mindmap$nodes[[1]]$source_ids, list())
  expect_identical(snapshot$suggested_questions, list("Q1", "Q2"))
  expect_null(snapshot$artifacts$report)
  expect_null(snapshot$artifacts$report_md)
  expect_null(snapshot$artifacts$mindmap_md)
  expect_null(restored$artifacts[["report"]])
  expect_null(restored$artifacts[["report_md"]])
  expect_null(restored$artifacts[["mindmap_md"]])
  expect_identical(tempest_session_report_md(restored), report_md)
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

test_that("schema 9 persists exact Deputy execution authority", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")

  moderator_source <- fake_source(
    "https://example.org/schema-9-moderator",
    content_text = "Moderator evidence is durable."
  )
  expert_source <- fake_source(
    "https://example.org/schema-9-expert",
    content_text = "Expert evidence is durable."
  )
  extractions <- list(
    list(
      facts = list(list(
        claim = "Moderator evidence is durable.",
        sources = list(list(source_id = moderator_source$id)),
        confidence = "high"
      ))
    ),
    list(
      facts = list(list(
        claim = "Expert evidence is durable.",
        sources = list(list(source_id = expert_source$id)),
        confidence = "high"
      ))
    )
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        return(fake_chat(structured = extractions))
      }
      fake_chat()
    }
  )
  expert <- test_expert(expert_id = "expert.schema-9-a")
  other_expert <- test_expert(expert_id = "expert.schema-9-b")
  session <- tempest_session(
    "Schema 9 Deputy authority",
    config = cfg,
    experts = list(expert, other_expert),
    session_id = "schema-9-deputy-authority"
  )
  session$workspace$upsert_retrieved_resource(moderator_source)
  session$workspace$upsert_retrieved_resource(expert_source)
  expert_session <- session$expert_session_manager$get_or_create(
    expert@expert_id
  )
  other_expert_session <- session$expert_session_manager$get_or_create(
    other_expert@expert_id
  )
  make_trace <- function(
    target,
    run_id,
    deputy_session_id,
    role,
    correlation_id,
    expert_id = NULL,
    status = "complete",
    stage = "dialogue"
  ) {
    context <- tempest:::tempest_deputy_run_context(
      target$manifest,
      stage = "dialogue",
      role = role,
      expert_id = expert_id
    )
    trace <- list(
      agent_id = tempest:::tempest_deputy_adapter_agent_id(context),
      correlation_id = correlation_id,
      deputy_run_id = run_id,
      deputy_session_id = deputy_session_id
    )
    if (!is.null(expert_id)) {
      trace$expert_id <- expert_id
    }
    trace$role <- role
    trace$stage <- stage
    trace$status <- status
    trace$trace_id <- run_id
    trace$trace_type <- "deputy_run"
    trace
  }
  moderator_trace <- make_trace(
    session,
    "deputy-run-b-moderator",
    tempest:::tempest_costorm_deputy_session_id(
      session$session_id,
      "moderator"
    ),
    "moderator",
    "turn-schema-9-moderator"
  )
  expert_trace <- make_trace(
    session,
    "deputy-run-d-expert",
    expert_session$session_id,
    "expert",
    "turn-schema-9-expert",
    expert_id = expert@expert_id
  )
  tempest:::tempest_session_record_deputy_trace(session, moderator_trace)
  tempest:::tempest_session_record_deputy_trace(session, expert_trace)
  withCallingHandlers(
    session$extract_facts(
      paste0(
        "Moderator evidence is durable [",
        moderator_source$id,
        "]."
      ),
      source_ids = moderator_source$id,
      expert_id = "moderator",
      correlation_id = moderator_trace$correlation_id,
      deputy_execution = moderator_trace
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  withCallingHandlers(
    session$extract_facts(
      paste0("Expert evidence is durable [", expert_source$id, "]."),
      source_ids = expert_source$id,
      expert_id = expert@expert_id,
      correlation_id = expert_trace$correlation_id,
      deputy_execution = expert_trace
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )

  snapshot <- tempest_session_snapshot(session)
  deputy_traces <- tempest:::tempest_session_deputy_traces(session)
  trace_types <- vapply(
    snapshot$research_manifest$traces,
    `[[`,
    character(1),
    "trace_type"
  )
  expect_identical(snapshot$schema_version, 9L)
  expect_identical(
    trace_types,
    c("stage_attempt", "stage_attempt", "deputy_run", "deputy_run")
  )
  expect_identical(
    tail(snapshot$research_manifest$traces, 2L),
    deputy_traces
  )
  expect_identical(
    snapshot$research_manifest$runtime,
    list(
      deputy_run_ids = as.list(c(
        "deputy-run-b-moderator",
        "deputy-run-d-expert"
      )),
      deputy_session_ids = as.list(sort(c(
        moderator_trace$deputy_session_id,
        expert_trace$deputy_session_id
      )))
    )
  )
  expect_identical(
    snapshot$research_manifest$traces[[1L]]$expert_id,
    "moderator"
  )
  expect_null(moderator_trace$expert_id)

  contains_runtime_object <- function(value) {
    if (
      inherits(
        value,
        c(
          "Agent",
          "TempestDeputyChatAdapter",
          "TempestRuntime",
          "R6"
        )
      )
    ) {
      return(TRUE)
    }
    if (!is.list(value)) {
      return(FALSE)
    }
    any(vapply(value, contains_runtime_object, logical(1)))
  }
  expect_identical(contains_runtime_object(snapshot), FALSE)

  restored <- tempest_session_restore(snapshot, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(restored),
    deputy_traces
  )
  bundle_dir <- file.path(withr::local_tempdir(), "schema-9-deputy")
  tempest_session_save(session, bundle_dir)
  resumed <- tempest_session_resume(bundle_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(resumed),
    deputy_traces
  )
  bundle_files <- list.files(
    bundle_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  bundle_text <- paste(
    vapply(
      bundle_files,
      function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )
  expect_no_match(
    bundle_text,
    "Agent|TempestDeputyChatAdapter|TempestRuntime|R6"
  )

  continued_moderator <- make_trace(
    resumed,
    "deputy-run-a-continued-moderator",
    moderator_trace$deputy_session_id,
    "moderator",
    "turn-schema-9-continued-moderator"
  )
  continued_expert <- make_trace(
    resumed,
    "deputy-run-e-continued-expert",
    expert_trace$deputy_session_id,
    "expert",
    "turn-schema-9-continued-expert",
    expert_id = expert@expert_id
  )
  tempest:::tempest_session_record_deputy_trace(
    resumed,
    continued_moderator
  )
  tempest:::tempest_session_record_deputy_trace(resumed, continued_expert)
  continued_snapshot <- tempest_session_snapshot(resumed)
  expect_length(
    Filter(
      \(trace) identical(trace$trace_type, "deputy_run"),
      continued_snapshot$research_manifest$traces
    ),
    4L
  )
  continued_dir <- file.path(dirname(bundle_dir), "schema-9-continued")
  tempest_session_save(resumed, continued_dir)
  continued <- tempest_session_resume(continued_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(continued),
    tempest:::tempest_session_deputy_traces(resumed)
  )

  historical_session <- tempest_session(
    "Retired Deputy history",
    config = cfg,
    experts = list(expert),
    session_id = "schema-9-retired-history"
  )
  retired_binding <- historical_session$expert_session_manager$get_or_create(
    expert@expert_id
  )
  retired_trace <- make_trace(
    historical_session,
    "deputy-run-retired-expert",
    retired_binding$session_id,
    "expert",
    "turn-schema-9-retired-expert",
    expert_id = expert@expert_id,
    status = "interrupted",
    stage = "warmup"
  )
  tempest:::tempest_session_record_deputy_trace(
    historical_session,
    retired_trace
  )
  historical_session$expert_session_manager$retire_session(
    retired_binding$session_id
  )
  expect_no_error(tempest_session_snapshot(historical_session))
  historical_dir <- file.path(dirname(bundle_dir), "schema-9-historical")
  tempest_session_save(historical_session, historical_dir)
  historical <- tempest_session_resume(historical_dir, config = cfg)
  replacement_binding <- historical$expert_session_manager$get_or_create(
    expert@expert_id
  )
  expect_identical(
    identical(
      replacement_binding$session_id,
      retired_binding$session_id
    ),
    FALSE
  )
  replacement_trace <- make_trace(
    historical,
    "deputy-run-replacement-expert",
    replacement_binding$session_id,
    "expert",
    "turn-schema-9-replacement-expert",
    expert_id = expert@expert_id
  )
  tempest:::tempest_session_record_deputy_trace(
    historical,
    replacement_trace
  )
  expect_no_error(tempest_session_snapshot(historical))
  historical_continued_dir <- file.path(
    dirname(bundle_dir),
    "schema-9-historical-continued"
  )
  tempest_session_save(historical, historical_continued_dir)
  historical_continued <- tempest_session_resume(
    historical_continued_dir,
    config = cfg
  )
  expect_identical(
    tempest:::tempest_session_deputy_traces(historical_continued),
    tempest:::tempest_session_deputy_traces(historical)
  )

  expect_rejected <- function(candidate) {
    expect_error(
      tempest_session_restore(candidate, config = cfg),
      class = "tempest_session_restore_error"
    )
  }
  stage_indexes <- which(trace_types == "stage_attempt")
  moderator_index <- which(vapply(
    snapshot$research_manifest$traces,
    \(trace) {
      identical(trace$trace_type, "deputy_run") &&
        identical(trace$role, "moderator")
    },
    logical(1)
  ))
  expert_index <- which(vapply(
    snapshot$research_manifest$traces,
    \(trace) {
      identical(trace$trace_type, "deputy_run") &&
        identical(trace$role, "expert")
    },
    logical(1)
  ))

  unknown_trace <- rlang::duplicate(snapshot, shallow = FALSE)
  unknown_trace$research_manifest$traces[[expert_index]]$trace_type <-
    "unknown_run"
  expect_rejected(unknown_trace)

  duplicate_trace <- rlang::duplicate(snapshot, shallow = FALSE)
  duplicate_trace$research_manifest$traces <- c(
    duplicate_trace$research_manifest$traces,
    list(duplicate_trace$research_manifest$traces[[expert_index]])
  )
  expect_rejected(duplicate_trace)

  reordered_traces <- rlang::duplicate(snapshot, shallow = FALSE)
  reordered_traces$research_manifest$traces <-
    rev(reordered_traces$research_manifest$traces)
  expect_rejected(reordered_traces)

  missing_run <- rlang::duplicate(snapshot, shallow = FALSE)
  missing_run$research_manifest$runtime$deputy_run_ids <-
    missing_run$research_manifest$runtime$deputy_run_ids[-1L]
  expect_rejected(missing_run)

  extra_session <- rlang::duplicate(snapshot, shallow = FALSE)
  extra_session$research_manifest$runtime$deputy_session_ids <- as.list(
    sort(c(
      unlist(
        extra_session$research_manifest$runtime$deputy_session_ids,
        use.names = FALSE
      ),
      "expert-session_ffffffffffffffff"
    ))
  )
  expect_rejected(extra_session)

  missing_terminal <- rlang::duplicate(snapshot, shallow = FALSE)
  missing_terminal$research_manifest$traces <-
    missing_terminal$research_manifest$traces[-expert_index]
  expect_rejected(missing_terminal)

  failed_terminal <- rlang::duplicate(snapshot, shallow = FALSE)
  failed_terminal$research_manifest$traces[[expert_index]]$status <-
    "provider_error"
  expect_rejected(failed_terminal)

  changed_correlation <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_correlation$research_manifest$traces[[expert_index]]$correlation_id <-
    "turn-schema-9-changed"
  expect_rejected(changed_correlation)

  changed_expert <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_expert$research_manifest$traces[[expert_index]]$expert_id <-
    other_expert@expert_id
  other_context <- tempest:::tempest_deputy_run_context(
    session$manifest,
    stage = "dialogue",
    role = "expert",
    expert_id = other_expert@expert_id
  )
  changed_expert$research_manifest$traces[[expert_index]]$agent_id <-
    tempest:::tempest_deputy_adapter_agent_id(other_context)
  expect_rejected(changed_expert)

  changed_expert_session <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_expert_session$research_manifest$traces[[
    expert_index
  ]]$deputy_session_id <-
    other_expert_session$session_id
  changed_expert_session$research_manifest$runtime$deputy_session_ids <-
    as.list(sort(c(
      moderator_trace$deputy_session_id,
      other_expert_session$session_id
    )))
  expect_rejected(changed_expert_session)

  changed_moderator_session <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_moderator_session$research_manifest$traces[[
    moderator_index
  ]]$deputy_session_id <-
    "tempest-moderator-000000000000000000000000"
  changed_moderator_session$research_manifest$runtime$deputy_session_ids <-
    as.list(sort(c(
      "tempest-moderator-000000000000000000000000",
      expert_trace$deputy_session_id
    )))
  expect_rejected(changed_moderator_session)

  changed_agent <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_agent$research_manifest$traces[[moderator_index]]$agent_id <-
    "forged-moderator-agent"
  expect_rejected(changed_agent)

  changed_trace_id <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_trace_id$research_manifest$traces[[expert_index]]$trace_id <-
    "forged-trace-id"
  expect_rejected(changed_trace_id)

  credential_trace <- rlang::duplicate(snapshot, shallow = FALSE)
  credential_trace$research_manifest$traces[[expert_index]]$correlation_id <-
    "sk-proj-0123456789abcdefghijklmnopqrstuv"
  expect_rejected(credential_trace)

  runtime_values <- list(
    deputy::Agent$new(chat = fake_chat()),
    session$chats$moderator,
    session$runtime
  )
  for (runtime_value in runtime_values) {
    runtime_snapshot <- rlang::duplicate(snapshot, shallow = FALSE)
    runtime_snapshot$research_manifest$traces[[
      moderator_index
    ]]$runtime_object <-
      runtime_value
    expect_rejected(runtime_snapshot)
  }

  expect_length(stage_indexes, 2L)
})

test_that("pending Deputy runs block schema 9 persistence", {
  skip_if_not_installed("coro")
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  raw_chat <- fake_chat()
  raw_chat$stream_async <- function(
    prompt = NULL,
    stream = c("text", "content"),
    controller = NULL
  ) {
    coro::async_generator(function() {
      chunk <- coro::await(promises::promise(function(resolve, reject) {
        later::later(
          function() {
            resolve(ellmer::ContentText("Late expert answer"))
          },
          0.25
        )
      }))
      coro::yield(chunk)
      coro::exhausted()
    })()
  }
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "expert")) {
        return(raw_chat)
      }
      fake_chat()
    }
  )
  expert <- test_expert(
    expert_id = "expert.pending-timeout",
    name = "Pending Timeout Expert",
    initial_questions = "What remains pending?"
  )
  session <- tempest_session(
    "Pending Deputy persistence",
    config = cfg,
    experts = list(expert),
    session_id = "pending-deputy-persistence"
  )

  settled <- await_tempest_promise(tempest_session_warmup_async(
    session,
    timeout_s = 0.02,
    max_parallel_experts = 1L
  ))
  expect_null(settled$error)
  expect_identical(
    settled$value@orientations[[1L]]$failure_kind,
    "timeout"
  )
  expect_length(tempest:::tempest_session_deputy_traces(session), 0L)
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(session),
    1L
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
  immediate_dir <- file.path(withr::local_tempdir(), "pending")
  expect_error(
    tempest_session_save(session, immediate_dir),
    class = "tempest_session_save_error"
  )
  expect_identical(dir.exists(immediate_dir), FALSE)

  deadline <- Sys.time() + 2
  while (
    length(tempest:::tempest_session_pending_deputy_runs(session)) > 0L &&
      Sys.time() < deadline
  ) {
    later::run_now(0.05)
  }
  traces <- tempest:::tempest_session_deputy_traces(session)
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(session),
    0L
  )
  expect_length(traces, 1L)
  expect_identical(traces[[1L]]$status, "interrupted")

  snapshot <- tempest_session_snapshot(session)
  expect_identical("pending_deputy_runs" %in% names(snapshot), FALSE)
  expect_identical(
    Filter(
      \(trace) identical(trace$trace_type, "deputy_run"),
      snapshot$research_manifest$traces
    ),
    traces
  )
  bundle_dir <- file.path(withr::local_tempdir(), "settled")
  tempest_session_save(session, bundle_dir)
  resumed <- tempest_session_resume(bundle_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(resumed),
    traces
  )
  expect_length(
    tempest:::tempest_session_pending_deputy_runs(resumed),
    0L
  )
})

test_that("session snapshots reject credentials outside evidence payloads", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  token <- "sk-proj-0123456789abcdefghijklmnopqrstuv"
  make_session <- function(topic = "Credential boundary") {
    tempest_session(
      topic,
      config = cfg,
      experts = list(test_expert(expert_id = "expert.credential-boundary"))
    )
  }

  session <- make_session()
  session$title <- token
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  session <- make_session()
  session$artifacts[["suggested_questions"]] <- paste(
    "Authorization: Bearer",
    token
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  session <- make_session()
  tempest:::tempest_session_set_report_value(
    session,
    paste0("# Credential boundary\n\nAuthorization: Bearer ", token, "\n")
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  for (encoded_token in c(
    "sk\\-proj\\-0123456789abcdefghijklmnopqrstuv",
    "sk&#45;proj&#45;0123456789abcdefghijklmnopqrstuv"
  )) {
    session <- make_session()
    safe_report <- tempest_report_md(
      title = session$title,
      body = "A portable report body.",
      workspace = session$workspace,
      citation_policy = cfg@citation_policy,
      on_unsupported_claim = cfg@on_unsupported_claim,
      min_support_score = cfg@min_support_score
    )
    tempest:::tempest_session_set_report_value(
      session,
      sub(
        "A portable report body.",
        encoded_token,
        safe_report,
        fixed = TRUE
      )
    )
    expect_error(
      tempest_session_snapshot(session),
      class = "tempest_session_snapshot_error"
    )
  }

  session <- make_session()
  fixture <- test_add_verifiable_claim(
    session$workspace,
    key = "credential-boundary"
  )
  support <- test_claim_support(fixture$claim, fixture$span)
  unsafe_support <- tempest:::tempest_claim_support_to_list(support)
  unsafe_support$rationale <- paste("Authorization: Bearer", token)
  workspace_private <- session$workspace$.__enclos_env__$private
  workspace_private$claim_supports_value[[support@claim_support_id]] <-
    unsafe_support
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  scientific_title <- "SK-BR-3, SK-N-SH, and SK-MEL-28"
  session <- make_session(scientific_title)
  session$artifacts[["suggested_questions"]] <- scientific_title
  report_md <- tempest_report_md(
    title = scientific_title,
    body = scientific_title,
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  tempest:::tempest_session_set_report_value(session, report_md)
  expect_no_error(tempest_session_snapshot(session))
})

test_that("no-reference Co reports remain canonical persistence products", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  source <- fake_source("https://example.org/no-reference-session-report")
  body <- paste0("Captured session evidence [", source$id, "].")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "writer")) {
        return(fake_chat(text = list(body)))
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "No reference session report",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.no-reference-report")),
    session_id = "no-reference-session-report"
  )
  session$workspace$upsert_retrieved_resource(source)

  report_md <- session$report(
    include_references = FALSE,
    reorganize = FALSE
  )
  expect_match(report_md, "^# No reference session report", perl = TRUE)
  expect_match(report_md, paste0("[", source$id, "]"), fixed = TRUE)
  expect_no_match(report_md, paste0("[^", source$id, "]"), fixed = TRUE)
  expect_no_match(report_md, "## References", fixed = TRUE)
  expect_no_error(tempest:::tempest_final_report_validate(
    report_md = report_md,
    workspace = session$workspace,
    title = session$title,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  ))
  expect_no_error(tempest_session_snapshot(session))

  bundle <- file.path(withr::local_tempdir(), "no-reference-session-report")
  tempest_session_save(session, bundle)
  restored <- tempest_session_resume(bundle, config = cfg)
  expect_identical(tempest_session_report_md(restored), report_md)
})

test_that("fenced package headings survive session report persistence", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Literal report headings",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.literal-headings")),
    session_id = "literal-report-headings"
  )
  body <- paste(
    c(
      "Example:",
      "",
      "```text",
      "## Execution review",
      "literal execution content",
      "",
      "## References",
      "",
      "literal reference content",
      "```"
    ),
    collapse = "\n"
  )
  report_md <- tempest_report_md(
    title = session$title,
    body = body,
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  tempest:::tempest_session_set_report_value(session, report_md)

  snapshot <- tempest_session_snapshot(session)
  expect_identical(snapshot$report_md, report_md)
  bundle <- file.path(withr::local_tempdir(), "literal-headings")
  tempest_session_save(session, bundle)
  restored <- tempest_session_resume(bundle, config = cfg)
  expect_identical(tempest_session_report_md(restored), report_md)

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
    attempt_id = "attempt-literal-headings",
    started_at = "2026-08-16T00:00:00Z"
  )
  cancelled <- tempest:::tempest_stage_record_cancel(
    running,
    completed_at = "2026-08-16T00:01:00Z"
  )
  tempest:::tempest_session_set_stage_records(session, list(cancelled))
  reviewed_report <- tempest:::tempest_markdown_append_execution_review(
    report_md,
    tempest:::tempest_stage_records_execution_review(list(cancelled)),
    trusted_title = session$title
  )
  tempest:::tempest_session_set_report_value(session, reviewed_report)

  reviewed_snapshot <- tempest_session_snapshot(session)
  expect_identical(reviewed_snapshot$report_md, reviewed_report)
  reviewed_bundle <- file.path(dirname(bundle), "literal-headings-reviewed")
  tempest_session_save(session, reviewed_bundle)
  reviewed_restored <- tempest_session_resume(reviewed_bundle, config = cfg)
  expect_identical(
    tempest_session_report_md(reviewed_restored),
    reviewed_report
  )
})

test_that("public session extraction persists its exact terminal record", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  source <- fake_source("https://example.org/persisted-session-extraction")
  extracted <- list(
    facts = list(list(
      claim = "Session extraction is durably recorded.",
      sources = list(list(source_id = source$id)),
      confidence = "high"
    ))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(extracted))
    }
  )
  session <- tempest_session(
    "Persisted session extraction",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.session-extraction")),
    session_id = "persisted-session-extraction"
  )
  session$workspace$upsert_retrieved_resource(source)
  expect_no_error(withCallingHandlers(
    session$extract_facts(
      paste0("Session extraction is durably recorded [", source$id, "]."),
      source_ids = source$id
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  ))
  claim <- session$workspace$list_proposed_claims()[[1]]
  expect_identical(claim@session_id, session$session_id)
  expect_identical(is.na(claim@expert_id), TRUE)
  expect_identical(is.na(claim@retrieval_step_id), TRUE)
  expect_length(claim@supporting_quotes, 0L)
  expect_length(tempest:::tempest_session_stage_records(session), 1L)
  expect_no_error(tempest_session_snapshot(session))

  bundle <- file.path(withr::local_tempdir(), "session-extraction")
  tempest_session_save(session, bundle)
  expect_r6_class(
    tempest_session_resume(bundle, config = cfg),
    "TempestSession"
  )
  records_path <- file.path(bundle, "stage_records.json")
  tempest:::tempest_write_json(records_path, list())
  manifest_path <- file.path(bundle, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_session_bundle_checksum(bundle, "stage_records.json")
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("sync and async warmups persist authoritative claim provenance", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  make_session <- function(mode) {
    topic <- paste("Persisted warmup", mode)
    session_id <- paste0("persisted-warmup-", mode)
    expert_id <- paste0("expert.persisted-warmup-", mode)
    source <- fake_source(paste0(
      "https://example.org/persisted-warmup-",
      mode
    ))
    answer <- paste0("Warmup evidence is durable [", source$id, "].")
    extraction <- list(
      facts = list(list(
        claim = "Warmup evidence is durable.",
        sources = list(list(source_id = source$id)),
        confidence = "high"
      ))
    )
    mindmap <- list(
      nodes = list(
        list(
          id = "root",
          label = topic,
          parent = NULL,
          notes = "",
          source_ids = character()
        ),
        list(
          id = "warmup-evidence",
          label = "Warmup evidence",
          parent = "root",
          notes = "Durable evidence",
          source_ids = source$id
        )
      ),
      edges = list()
    )
    chat_factory <- function(role, model, system_prompt, echo) {
      text <- if (identical(role, "expert")) answer else ""
      structured <- switch(
        role,
        judge = extraction,
        mindmap = mindmap,
        list()
      )
      fake_chat(
        text = list(text),
        structured = if (length(structured) > 0L) {
          list(structured)
        } else {
          list()
        }
      )
    }
    cfg <- tempest_config(chat_fn = chat_factory)
    session <- tempest_session(
      topic,
      config = cfg,
      experts = list(test_expert(
        expert_id = expert_id,
        initial_questions = "What evidence should orient the panel?"
      )),
      session_id = session_id
    )
    session$workspace$upsert_retrieved_resource(source)
    list(
      session = session,
      config = cfg,
      session_id = session_id,
      expert_id = expert_id
    )
  }

  for (mode in c("sync", "async")) {
    fixture <- make_session(mode)
    if (identical(mode, "sync")) {
      expect_no_error(withCallingHandlers(
        fixture$session$warmup(verbose = FALSE),
        dsprrr_cache_security_warning = function(condition) {
          invokeRestart("muffleWarning")
        }
      ))
    } else {
      settled <- await_tempest_promise(tempest_session_warmup_async(
        fixture$session,
        timeout_s = 1,
        max_parallel_experts = 1
      ))
      expect_null(settled$error)
      expect_identical(settled$value@status, "succeeded")
    }

    claims <- fixture$session$workspace$list_proposed_claims()
    records <- tempest:::tempest_session_stage_records(fixture$session)
    expect_length(claims, 1L)
    expect_length(records, 1L)
    claim <- claims[[1]]
    record <- records[[1]]
    expect_identical(claim@session_id, fixture$session_id)
    expect_identical(claim@expert_id, fixture$expert_id)
    record_correlation <- record@trace_references$correlation_id
    if (is.null(record_correlation)) {
      record_correlation <- NA_character_
    }
    expect_identical(
      claim@retrieval_step_id,
      record_correlation
    )
    expect_identical(
      record@trace_references$research_run_id,
      fixture$session_id
    )
    expect_identical(record@trace_references$expert_id, fixture$expert_id)
    snapshot <- tempest_session_snapshot(fixture$session)
    expect_identical(
      snapshot$workspace$proposed_claims[[1]]$session_id,
      fixture$session_id
    )

    bundle <- file.path(withr::local_tempdir(), mode)
    tempest_session_save(fixture$session, bundle)
    restored <- tempest_session_resume(bundle, config = fixture$config)
    restored_claim <- restored$workspace$list_proposed_claims()[[1]]
    expect_identical(restored_claim@session_id, fixture$session_id)
    expect_identical(
      tempest:::tempest_stage_records_data(
        tempest:::tempest_session_stage_records(restored)
      ),
      tempest:::tempest_stage_records_data(records)
    )
  }
})

test_that("public session verification persists pair support and source proof", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    citation_policy = "strict",
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Persisted session verification",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.session-verification")),
    session_id = "persisted-session-verification"
  )
  source <- fake_source("https://example.org/persisted-session-verification")
  session$workspace$upsert_retrieved_resource(source)
  span_id <- session$workspace$add_evidence_span(tempest_evidence_span(
    source_id = source$id,
    quote = source$content_text,
    evidence_span_id = "span.persisted-session-verification"
  ))
  session$workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim.persisted-session-verification",
    claim_text = "Session verification commits durable proof",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list(source$content_text)
  ))
  judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.95,
      rationale = "The captured source supports the claim."
    ))
  )
  audit <- withCallingHandlers(
    tempest_verify_claims(session, verifier = judge),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(audit$verification_status, "supported")
  expect_length(tempest:::tempest_session_stage_records(session), 1L)
  report_md <- tempest_report_md(
    title = session$title,
    body = paste0(
      "Session verification commits durable proof [",
      source$id,
      "]."
    ),
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  tempest:::tempest_session_set_report_value(session, report_md)
  expect_no_error(tempest_session_snapshot(session))

  forged_report <- sub(
    paste0("# ", session$title, "\n\n"),
    paste0(
      "# ",
      session$title,
      "\n\n## A false factual conclusion\n\n"
    ),
    report_md,
    fixed = TRUE
  )
  tempest:::tempest_session_set_report_value(session, forged_report)
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
  tempest:::tempest_session_set_report_value(session, report_md)

  bundle <- file.path(withr::local_tempdir(), "session-verification")
  tempest_session_save(session, bundle)
  restored <- tempest_session_resume(bundle, config = cfg)
  expect_identical(
    tempest_claim_supports(restored$workspace)$verification_status,
    "supported"
  )
  expect_length(tempest:::tempest_session_stage_records(restored), 1L)

  report_path <- file.path(bundle, "report.md")
  persisted_report <- tempest:::tempest_read_text(report_path)
  forged_persisted <- sub(
    paste0("# ", session$title, "\n\n"),
    paste0(
      "# ",
      session$title,
      "\n\n## A false factual conclusion\n\n"
    ),
    persisted_report,
    fixed = TRUE
  )
  tempest:::tempest_write_text(report_path, forged_persisted)
  manifest_path <- file.path(bundle, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["report.md"]] <-
    tempest:::tempest_session_bundle_checksum(bundle, "report.md")
  manifest$report_reference <-
    tempest:::tempest_persistence_report_reference(forged_persisted)
  bound_manifest <- tempest:::tempest_persistence_manifest_bind_report(
    tempest:::tempest_research_manifest_from_record(
      manifest$research_manifest
    ),
    forged_persisted
  )
  manifest$research_manifest <-
    tempest:::tempest_research_manifest_record(bound_manifest)
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )

  standalone <- tempest_research_workspace()
  standalone$upsert_retrieved_resource(source)
  standalone_span_id <- standalone$add_evidence_span(tempest_evidence_span(
    source_id = source$id,
    quote = source$content_text,
    evidence_span_id = "span.standalone-verification"
  ))
  standalone$add_proposed_claim(tempest_claim(
    claim_id = "claim.standalone-verification",
    claim_text = "Discarded records cannot become session proof.",
    source_ids = source$id,
    evidence_span_ids = standalone_span_id,
    supporting_quotes = list(source$content_text)
  ))
  standalone_judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.95,
      rationale = "Standalone verification has no session ledger."
    ))
  )
  withCallingHandlers(
    tempest_verify_claims(
      standalone,
      verifier = standalone_judge,
      verifier_model = cfg@models[["judge"]]
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  expect_error(
    tempest_session(
      "Unbound standalone verification",
      config = cfg,
      experts = list(test_expert(expert_id = "expert.unbound-verification")),
      retriever = tempest_retriever(config = cfg, workspace = standalone),
      session_id = "unbound-standalone-verification"
    ),
    class = "tempest_research_workspace_integrity_error"
  )
})

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
  legacy_snapshot$schema_version <- 8L
  expect_error(
    tempest:::tempest_session_restore(legacy_snapshot, config = cfg),
    class = "tempest_unsupported_format_error"
  )
})

test_that("Co-STORM snapshots cancel running stage attempts without mutation", {
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

  snapshot <- tempest_session_snapshot(session)

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
  experts <- tempest:::tempest_read_json_strict(experts_path)
  experts[[1]]$name <- "Forged but internally refingerprinted expert"
  experts[[1]]$fingerprint <-
    tempest:::tempest_expert_profile_fingerprint(experts[[1]])
  tempest:::tempest_write_json(experts_path, experts)
  manifest_path <- file.path(bundle, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_session_bundle_checksum(bundle, "experts.json")
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("schema 9 session restore protects research identity", {
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

  double_schema <- snapshot
  double_schema$schema_version <- 8.0
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

  for (field in c(
    "version",
    "state",
    "schema_version",
    "focus_areas",
    "metadata"
  )) {
    null_expert_field <- snapshot
    null_expert_field$experts[[1]][field] <- list(NULL)
    expect_error(
      tempest_session_restore(null_expert_field, config = cfg),
      class = "tempest_session_restore_error",
      info = field
    )
  }

  double_expert_schema <- snapshot
  double_expert_schema$experts[[1]]$schema_version <- 1.0
  expect_error(
    tempest_session_restore(double_expert_schema, config = cfg),
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

  session$title <- 1L
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
})

test_that("schema 9 progress history is exact and session-bound", {
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
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/session-bundle",
    title = "Session Bundle Source",
    content_text = "Bundles preserve claims."
  )
  store$upsert_retrieved_resource(source)
  span_id <- store$add_evidence_span(tempest_evidence_span(
    source_id = source$id,
    quote = "Bundles preserve claims.",
    evidence_span_id = "span.session-bundle"
  ))
  claim_id <- store$add_proposed_claim(tempest_claim(
    claim_text = "Bundles preserve claims.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Bundles preserve claims.")
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
    body = "Bundle report",
    workspace = store,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  tempest:::tempest_session_set_report_value(session, report_md)
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
  expect_equal(manifest$schema_version, 9L)
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
  expect_identical(manifest$workspace$schema_version, 5L)
  expect_setequal(names(manifest$checksums), manifest$files)
  expect_contains(
    manifest$files,
    c(
      "experts.json",
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

  schema_eight_manifest <- manifest
  schema_eight_manifest$schema_version <- 8L
  tempest:::tempest_write_json(
    file.path(bundle_dir, "session.json"),
    schema_eight_manifest
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_unsupported_format_error"
  )

  reordered_manifest <- manifest[rev(names(manifest))]
  tempest:::tempest_write_json(
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
  tempest:::tempest_write_json(
    file.path(bundle_dir, "session.json"),
    reordered_workspace
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  tempest:::tempest_write_json(file.path(bundle_dir, "session.json"), manifest)

  downgraded_manifest <- manifest
  downgraded_manifest$workspace$schema_version <- 4L
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
  expect_identical(tempest_session_report_md(restored), report_md)
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

test_that("schema 8 session bundles are rejected", {
  bundle_dir <- withr::local_tempdir()
  tempest:::tempest_write_json(
    file.path(bundle_dir, "session.json"),
    list(schema_version = 8L)
  )
  expect_error(
    tempest_session_resume(bundle_dir),
    class = "tempest_unsupported_format_error"
  )
})

test_that("stage-record sidecars are mandatory regular bundle files", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Stage-record integrity",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.stage-record-integrity"))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "session")
  tempest_session_save(session, bundle_dir)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$files <- setdiff(
    unlist(manifest$files, use.names = FALSE),
    "stage_records.json"
  )
  manifest$checksums[["stage_records.json"]] <- NULL
  unlink(file.path(bundle_dir, "stage_records.json"))
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )

  program_set <- tempest_program_set()
  run_dir <- file.path(withr::local_tempdir(), "run")
  dir.create(run_dir)
  tempest:::tempest_save_run_artifacts(
    run_dir,
    tempest_research_workspace(),
    tempest:::tempest_storm_state("Stage-record integrity"),
    tempest_research_manifest(
      "stage-record-integrity",
      config = tempest_config(),
      programs = tempest:::tempest_program_set_manifest_programs(program_set)
    ),
    program_set = program_set,
    config = tempest_config(),
    steps = "research",
    research_strategy = "key_questions"
  )
  run_manifest_path <- file.path(run_dir, "run_config.json")
  run_manifest <- tempest:::tempest_read_json_strict(run_manifest_path)
  run_manifest$files <- setdiff(
    unlist(run_manifest$files, use.names = FALSE),
    "stage_records.json"
  )
  run_manifest$checksums[["stage_records.json"]] <- NULL
  unlink(file.path(run_dir, "stage_records.json"))
  tempest:::tempest_write_json(run_manifest_path, run_manifest)

  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = tempest_config(),
      program_set = program_set,
      run_id = "stage-record-integrity"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("stage-record sidecars reject same-root symlinks", {
  skip_on_os("windows")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Stage-record symlink",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.stage-record-symlink"))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "session")
  tempest_session_save(session, bundle_dir)
  sidecar <- file.path(bundle_dir, "stage_records.json")
  unlink(sidecar)
  expect_identical(
    file.symlink(file.path(bundle_dir, "expert_sessions.json"), sidecar),
    TRUE
  )
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "stage_records.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  program_set <- tempest_program_set()
  run_dir <- file.path(withr::local_tempdir(), "run")
  dir.create(run_dir)
  tempest:::tempest_save_run_artifacts(
    run_dir,
    tempest_research_workspace(),
    tempest:::tempest_storm_state("Stage-record symlink"),
    tempest_research_manifest(
      "stage-record-symlink",
      config = cfg,
      programs = tempest:::tempest_program_set_manifest_programs(program_set)
    ),
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  run_sidecar <- file.path(run_dir, "stage_records.json")
  unlink(run_sidecar)
  expect_identical(
    file.symlink(file.path(run_dir, "references.json"), run_sidecar),
    TRUE
  )
  run_manifest_path <- file.path(run_dir, "run_config.json")
  run_manifest <- tempest:::tempest_read_json_strict(run_manifest_path)
  run_manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      run_dir,
      "stage_records.json"
    )
  tempest:::tempest_write_json(run_manifest_path, run_manifest)

  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "stage-record-symlink"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("bundle root manifests reject internal and escaping symlinks", {
  skip_on_os("windows")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  root <- withr::local_tempdir()
  make_session_bundle <- function(name) {
    bundle <- file.path(root, name)
    session <- tempest_session(
      "Root manifest symlink",
      config = cfg,
      experts = list(test_expert(expert_id = paste0("expert.", name)))
    )
    tempest_session_save(session, bundle)
    bundle
  }

  internal_session <- make_session_bundle("session-internal")
  unlink(file.path(internal_session, "session.json"))
  expect_identical(
    file.symlink(
      file.path(internal_session, "experts.json"),
      file.path(internal_session, "session.json")
    ),
    TRUE
  )
  expect_error(
    tempest_session_resume(internal_session, config = cfg),
    class = "tempest_session_restore_error"
  )

  escaping_session <- make_session_bundle("session-escaping")
  session_manifest <- file.path(escaping_session, "session.json")
  outside_session <- tempfile(
    "outside-session-",
    tmpdir = dirname(escaping_session)
  )
  expect_identical(file.copy(session_manifest, outside_session), TRUE)
  unlink(session_manifest)
  expect_identical(file.symlink(outside_session, session_manifest), TRUE)
  expect_error(
    tempest_session_resume(escaping_session, config = cfg),
    class = "tempest_session_restore_error"
  )

  program_set <- tempest_program_set()
  make_storm_bundle <- function(name) {
    bundle <- file.path(root, name)
    tempest:::tempest_save_run_artifacts(
      bundle,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Root manifest symlink"),
      tempest_research_manifest(
        name,
        config = cfg,
        programs = tempest:::tempest_program_set_manifest_programs(program_set)
      ),
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    )
    bundle
  }

  internal_storm <- make_storm_bundle("storm-internal")
  unlink(file.path(internal_storm, "run_config.json"))
  expect_identical(
    file.symlink(
      file.path(internal_storm, "workspace.json"),
      file.path(internal_storm, "run_config.json")
    ),
    TRUE
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(
      internal_storm,
      config = cfg,
      program_set = program_set,
      run_id = "storm-internal"
    ),
    class = "tempest_run_restore_error"
  )

  escaping_storm <- make_storm_bundle("storm-escaping")
  storm_manifest <- file.path(escaping_storm, "run_config.json")
  outside_storm <- tempfile(
    "outside-storm-",
    tmpdir = dirname(escaping_storm)
  )
  expect_identical(file.copy(storm_manifest, outside_storm), TRUE)
  unlink(storm_manifest)
  expect_identical(file.symlink(outside_storm, storm_manifest), TRUE)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      escaping_storm,
      config = cfg,
      program_set = program_set,
      run_id = "storm-escaping"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("stage-record sidecars bind manifest, workspace, and output kind", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  program_set <- tempest_program_set()
  extraction_program <-
    tempest:::tempest_program_set_manifest_programs(program_set)$extract_claims
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.org/stage-record-binding",
    title = "Stage record binding",
    content_text = "Stage records bind durable claim outputs."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-stage-record-binding",
    source_id = source$id,
    quote = "Stage records bind durable claim outputs.",
    extracted_by = extraction_program$program_artifact_id
  ))
  manual_span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-manual-stage-record-binding",
    source_id = source$id,
    quote = "Stage records bind durable claim outputs.",
    extracted_by = extraction_program$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_text = "Stage records bind durable claim outputs.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Stage records bind durable claim outputs."),
    retrieval_step_id = "retrieval.stage-record-binding",
    expert_id = "expert.stage-record-binding",
    session_id = "stage-record-binding"
  ))
  manual_claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_text = "Manual claims do not satisfy extraction proof.",
    source_ids = source$id,
    evidence_span_ids = manual_span_id,
    supporting_quotes = list("Stage records bind durable claim outputs.")
  ))
  session <- tempest_session(
    "Stage-record binding",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.stage-record-binding")),
    retriever = tempest_retriever(config = cfg, workspace = workspace),
    session_id = "stage-record-binding",
    program_set = program_set
  )
  expert_session <- session$expert_session_manager$get_or_create(
    "expert.stage-record-binding"
  )
  run_context <- tempest:::tempest_deputy_run_context(
    session$manifest,
    stage = "dialogue",
    role = "expert",
    expert_id = "expert.stage-record-binding"
  )
  tempest:::tempest_session_record_deputy_trace(
    session,
    list(
      agent_id = tempest:::tempest_deputy_adapter_agent_id(run_context),
      correlation_id = "retrieval.stage-record-binding",
      deputy_run_id = "trace.stage-record-binding",
      deputy_session_id = expert_session$session_id,
      expert_id = "expert.stage-record-binding",
      role = "expert",
      stage = "dialogue",
      status = "complete",
      trace_id = "trace.stage-record-binding",
      trace_type = "deputy_run"
    )
  )
  judge <- fake_chat(
    structured = list(
      list(
        status = "supported",
        score = 0.9,
        rationale = "Exact persisted pair binding."
      ),
      list(
        status = "unsupported",
        score = 0.1,
        rationale = "The span does not support the manual claim."
      )
    )
  )
  withCallingHandlers(
    tempest_verify_claims(session, verifier = judge),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  verification_records <- tempest:::tempest_session_stage_records(session)
  reference <- session$manifest@programs$extract_claims
  started <- tempest:::tempest_stage_record_start(
    "extract_claims",
    reference$program_artifact_id,
    reference$governed_procedure_ref$revision_id,
    trace_references = list(
      research_run_id = session$session_id,
      expert_id = "expert.stage-record-binding",
      correlation_id = "retrieval.stage-record-binding",
      mode = "costorm",
      role = "program"
    ),
    attempt_id = "attempt-stage-record-binding",
    started_at = "2026-08-16T00:00:00Z"
  )
  succeeded <- tempest:::tempest_stage_record_succeed(
    started,
    tempest:::tempest_stage_output_reference(
      "workspace_claims",
      c(claim_id, manual_claim_id),
      content_digest = tempest:::tempest_stage_claims_output_digest(
        list(
          workspace$get_proposed_claim(claim_id),
          workspace$get_proposed_claim(manual_claim_id)
        ),
        started,
        list(
          workspace$get_evidence_span(span_id),
          workspace$get_evidence_span(manual_span_id)
        )
      )
    ),
    support_status = "unknown",
    completed_at = "2026-08-16T00:01:00Z"
  )
  support_reference <- session$manifest@programs$verify_claim_support
  empty_support_attempt <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    support_reference$program_artifact_id,
    trace_references = list(
      min_support_score = "0.7",
      verified_at = "2026-08-16T00:00:00Z"
    ),
    attempt_id = "attempt-empty-support-binding",
    started_at = "2026-08-16T00:04:00Z"
  )
  empty_support <- tempest:::tempest_stage_record_succeed(
    empty_support_attempt,
    tempest:::tempest_stage_output_reference(
      "claim_supports",
      content_digest = paste0("sha256:", strrep("e", 64L))
    ),
    support_status = "verified",
    completed_at = "2026-08-16T00:04:30Z"
  )
  expect_no_error(tempest:::tempest_stage_records_validate_workspace(
    verification_records,
    workspace
  ))
  expect_error(
    tempest:::tempest_stage_records_validate_workspace(
      list(empty_support),
      workspace
    ),
    class = "tempest_stage_record_error"
  )
  tempest:::tempest_session_set_stage_records(
    session,
    c(list(succeeded), verification_records)
  )
  bundle_dir <- file.path(withr::local_tempdir(), "session")
  tempest_session_save(session, bundle_dir)
  expect_r6_class(
    tempest_session_resume(bundle_dir, config = cfg),
    "TempestSession"
  )

  sidecar_path <- file.path(bundle_dir, "stage_records.json")
  manifest_path <- file.path(bundle_dir, "session.json")
  valid_records <- tempest:::tempest_read_json_strict(sidecar_path)
  valid_manifest <- tempest:::tempest_read_json_strict(manifest_path)
  write_records <- function(records) {
    tempest:::tempest_write_json(sidecar_path, records)
    manifest <- valid_manifest
    manifest$checksums[["stage_records.json"]] <-
      tempest:::tempest_session_bundle_checksum(
        bundle_dir,
        "stage_records.json"
      )
    tempest:::tempest_write_json(manifest_path, manifest)
  }
  expect_rejected <- function() {
    expect_error(
      tempest_session_resume(bundle_dir, config = cfg),
      class = "tempest_session_restore_error"
    )
  }

  changed_manifest_trace <- valid_manifest
  changed_manifest_trace$research_manifest$traces[[1]]$status <- "failed"
  tempest:::tempest_write_json(manifest_path, changed_manifest_trace)
  expect_rejected()

  extra_manifest_trace <- valid_manifest
  extra_trace <- extra_manifest_trace$research_manifest$traces[[1]]
  extra_trace$trace_id <- "attempt-unrecorded"
  extra_manifest_trace$research_manifest$traces <- c(
    extra_manifest_trace$research_manifest$traces,
    list(extra_trace)
  )
  tempest:::tempest_write_json(manifest_path, extra_manifest_trace)
  expect_rejected()

  missing_manifest_trace <- valid_manifest
  missing_manifest_trace$research_manifest$traces <-
    missing_manifest_trace$research_manifest$traces[-1]
  tempest:::tempest_write_json(manifest_path, missing_manifest_trace)
  expect_rejected()

  wrong_program <- valid_records
  wrong_program[[1]]$program_artifact_id <-
    paste0("sha256:", strrep("0", 64L))
  write_records(wrong_program)
  expect_rejected()

  wrong_run <- valid_records
  wrong_run[[1]]$trace_references$research_run_id <- "transplanted-run"
  write_records(wrong_run)
  expect_rejected()

  missing_run <- valid_records
  missing_run[[1]]$trace_references$research_run_id <- NULL
  write_records(missing_run)
  expect_rejected()

  missing_trace <- valid_records
  missing_trace[[1]]$trace_references$trace_id <- NULL
  write_records(missing_trace)
  expect_rejected()

  missing_expert <- valid_records
  missing_expert[[1]]$trace_references$expert_id <- NULL
  write_records(missing_expert)
  expect_rejected()

  missing_correlation <- valid_records
  missing_correlation[[1]]$trace_references$correlation_id <- NULL
  write_records(missing_correlation)
  expect_rejected()

  unknown_claim <- valid_records
  unknown_claim[[1]]$output_reference$ids <- list("claim.unknown")
  write_records(unknown_claim)
  expect_rejected()

  substituted_claim <- valid_records
  substituted_claim[[1]]$output_reference$ids <- list(manual_claim_id)
  write_records(substituted_claim)
  expect_rejected()

  wrong_kind <- valid_records
  wrong_kind[[1]]$output_reference <- list(
    kind = "content_digest",
    ids = list(paste0("sha256:", strrep("d", 64L)))
  )
  write_records(wrong_kind)
  expect_rejected()

  write_records(tempest:::tempest_stage_records_data(list(started)))
  expect_rejected()

  duplicated <- list(valid_records[[1]], valid_records[[1]])
  write_records(duplicated)
  expect_rejected()

  extra_field <- valid_records
  extra_field[[1]]$runtime <- "not durable"
  write_records(extra_field)
  expect_rejected()

  claim_path <- file.path(bundle_dir, "workspace/proposed_claims.json")
  span_path <- file.path(bundle_dir, "workspace/evidence_spans.json")
  support_path <- file.path(bundle_dir, "workspace/claim_supports.json")
  valid_claims <- tempest:::tempest_read_json_strict(claim_path)
  valid_spans <- tempest:::tempest_read_json_strict(span_path)
  valid_supports <- tempest:::tempest_read_json_strict(support_path)
  write_workspace <- function(rel_path, value) {
    tempest:::tempest_write_json(claim_path, valid_claims)
    tempest:::tempest_write_json(span_path, valid_spans)
    tempest:::tempest_write_json(support_path, valid_supports)
    tempest:::tempest_write_json(file.path(bundle_dir, rel_path), value)
    tempest:::tempest_write_json(sidecar_path, valid_records)
    manifest <- valid_manifest
    for (workspace_path in c(
      "workspace/proposed_claims.json",
      "workspace/evidence_spans.json",
      "workspace/claim_supports.json"
    )) {
      manifest$checksums[[workspace_path]] <-
        tempest:::tempest_session_bundle_checksum(bundle_dir, workspace_path)
    }
    manifest$checksums[["stage_records.json"]] <-
      tempest:::tempest_session_bundle_checksum(
        bundle_dir,
        "stage_records.json"
      )
    tempest:::tempest_write_json(manifest_path, manifest)
  }

  changed_claims <- valid_claims
  extracted_index <- match(
    claim_id,
    vapply(valid_claims, \(claim) claim$claim_id, character(1))
  )
  changed_claims[[extracted_index]]$retrieval_query <-
    "forged immutable query"
  write_workspace("workspace/proposed_claims.json", changed_claims)
  expect_rejected()

  changed_verifier <- valid_claims
  changed_verifier[[extracted_index]]$verifier_model <- "forged-verifier"
  write_workspace("workspace/proposed_claims.json", changed_verifier)
  expect_rejected()

  changed_verification_time <- valid_claims
  changed_verification_time[[extracted_index]]$verified_at <-
    "2026-08-16T00:00:01Z"
  write_workspace(
    "workspace/proposed_claims.json",
    changed_verification_time
  )
  expect_rejected()

  changed_spans <- valid_spans
  changed_spans[[1]]$extracted_by <- support_reference$program_artifact_id
  write_workspace("workspace/evidence_spans.json", changed_spans)
  expect_rejected()

  changed_supports <- valid_supports
  changed_supports[[1]]$rationale <- "Rechecksummed but not execution-bound."
  write_workspace("workspace/claim_supports.json", changed_supports)
  expect_rejected()
})

test_that("verification stage records cover every claim-span support pair", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace()
  source <- fake_source(
    url = "https://example.org/two-claim-audit",
    title = "Two-claim audit"
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    source_id = source$id,
    quote = source$content_text,
    evidence_span_id = "span.two-claim-support"
  ))
  claim_texts <- c(
    "The first claim is supported.",
    "The second claim is supported."
  )
  claim_ids <- vapply(
    claim_texts,
    function(claim_text) {
      workspace$add_proposed_claim(tempest_claim(
        claim_text = claim_text,
        source_ids = source$id,
        evidence_span_ids = span_id,
        supporting_quotes = list(source$content_text)
      ))
    },
    character(1)
  )
  session <- tempest_session(
    "Two-claim stage audit",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.two-claim-audit")),
    retriever = tempest_retriever(config = cfg, workspace = workspace),
    session_id = "two-claim-audit"
  )
  judge <- fake_chat(
    structured = lapply(seq_along(claim_ids), function(index) {
      list(
        status = "supported",
        score = 0.9,
        rationale = paste("Exact support pair", index)
      )
    })
  )
  withCallingHandlers(
    tempest_verify_claims(session, verifier = judge),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  supports <- workspace$list_claim_supports()
  records <- tempest:::tempest_session_stage_records(session)
  bundle_dir <- file.path(withr::local_tempdir(), "session")

  tempest_session_save(session, bundle_dir)
  restored <- tempest_session_resume(bundle_dir, config = cfg)
  restored_records <- tempest:::tempest_session_stage_records(restored)

  expect_length(restored_records, 2L)
  expect_setequal(
    unlist(
      lapply(restored_records, \(record) record@output_reference$ids),
      use.names = FALSE
    ),
    vapply(supports, \(support) support@claim_support_id, "")
  )

  resources_path <- file.path(
    bundle_dir,
    "workspace/retrieved_resources.json"
  )
  manifest_path <- file.path(bundle_dir, "session.json")
  valid_resources <- tempest:::tempest_read_json_strict(resources_path)
  valid_manifest <- tempest:::tempest_read_json_strict(manifest_path)
  expect_source_tamper_rejected <- function(content) {
    resources <- valid_resources
    resources[[1]]$content <- content
    resources[[1]]$metadata$content_text <- content
    resources[[1]]$content_hash <-
      tempest:::tempest_artifact_codec_encode(
        content,
        resources[[1]]$media_type
      )$sha256
    resources[[1]]$fingerprint <-
      tempest:::tempest_resource_fingerprint(resources[[1]])
    tempest:::tempest_write_json(resources_path, resources)
    manifest <- valid_manifest
    manifest$checksums[["workspace/retrieved_resources.json"]] <-
      tempest:::tempest_session_bundle_checksum(
        bundle_dir,
        "workspace/retrieved_resources.json"
      )
    tempest:::tempest_write_json(manifest_path, manifest)
    expect_error(
      tempest_session_resume(bundle_dir, config = cfg),
      class = "tempest_session_restore_error"
    )
  }

  expect_source_tamper_rejected("")
  expect_source_tamper_rejected(
    "This replacement body is unrelated to either verified claim."
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
  report_md <- tempest_report_md(
    title = session$title,
    body = "Durable report",
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  tempest:::tempest_session_set_report_value(session, report_md)
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
  expect_identical(tempest_session_report_md(restored), report_md)
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
      programs = program_references,
      knowledge_snapshot = reference
    ),
    program_set = program_set,
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
    program_set = program_set,
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
  tempest:::tempest_save_run_artifacts(
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
    tempest:::tempest_load_run_artifacts(
      storm_dir,
      config = cfg,
      program_set = program_set
    ),
    class = "tempest_run_restore_error"
  )
  writeBin(storm_bytes, storm_sidecar)
  writeBin(charToRaw("corrupt snapshot"), storm_sidecar)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      storm_dir,
      config = cfg,
      program_set = program_set
    ),
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
    binding["grants"] <- list(NULL)
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

  invalid_bundle <- file.path(dirname(not_a_bundle), "invalid-bundle")
  dir.create(invalid_bundle)
  manifest_path <- file.path(invalid_bundle, "session.json")
  precious_path <- file.path(invalid_bundle, "precious.txt")
  writeLines("{", manifest_path)
  writeLines("preserve these bytes", precious_path)
  bundle_bytes <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, all.files = TRUE))
    stats::setNames(
      lapply(files, function(file) {
        file_path <- file.path(path, file)
        readBin(file_path, what = "raw", n = file.info(file_path)$size)
      }),
      files
    )
  }
  before <- bundle_bytes(invalid_bundle)

  expect_error(
    tempest_session_save(session, invalid_bundle, overwrite = TRUE),
    class = "tempest_session_save_error"
  )
  expect_identical(bundle_bytes(invalid_bundle), before)
})

test_that("session and STORM saves reject symbolic-link bundle roots", {
  skip_on_os("windows")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  root <- withr::local_tempdir()

  session_target <- file.path(root, "session-target")
  original_session <- tempest_session(
    "Original session root",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.root-original"))
  )
  tempest_session_save(original_session, session_target)
  session_alias <- file.path(root, "session-alias")
  expect_identical(file.symlink(session_target, session_alias), TRUE)
  replacement_session <- tempest_session(
    "Replacement session root",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.root-replacement"))
  )

  expect_error(
    tempest_session_save(
      replacement_session,
      session_alias,
      overwrite = TRUE
    ),
    class = "tempest_session_save_error"
  )
  expect_identical(Sys.readlink(session_alias), session_target)
  restored_session <- tempest_session_resume(session_target, config = cfg)
  expect_identical(restored_session$topic, "Original session root")

  program_set <- tempest_program_set()
  storm_target <- file.path(root, "storm-target")
  storm_manifest <- tempest_research_manifest(
    "storm-root-symlink",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  storm_workspace <- tempest_research_workspace()
  tempest:::tempest_save_run_artifacts(
    storm_target,
    storm_workspace,
    tempest:::tempest_storm_state(
      "STORM root symlink",
      title = "Original STORM title"
    ),
    storm_manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  storm_alias <- file.path(root, "storm-alias")
  expect_identical(file.symlink(storm_target, storm_alias), TRUE)

  expect_error(
    tempest:::tempest_save_run_artifacts(
      paste0(storm_alias, .Platform$file.sep),
      storm_workspace,
      tempest:::tempest_storm_state(
        "STORM root symlink",
        title = "Replacement STORM title"
      ),
      storm_manifest,
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
  expect_identical(Sys.readlink(storm_alias), storm_target)
  restored_storm <- tempest:::tempest_load_run_artifacts(
    storm_target,
    config = cfg,
    program_set = program_set,
    run_id = "storm-root-symlink"
  )
  expect_identical(restored_storm$state$title, "Original STORM title")
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
  root <- withr::local_tempdir()
  fresh_bundle <- function(name) {
    bundle_dir <- file.path(root, name)
    tempest_session_save(session, bundle_dir)
    bundle_dir
  }

  bundle_dir <- fresh_bundle("missing-experts")
  unlink(file.path(bundle_dir, "experts.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- fresh_bundle("malformed-claims")
  writeLines("{", file.path(bundle_dir, "workspace/proposed_claims.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- fresh_bundle("missing-transcript")
  unlink(file.path(bundle_dir, "transcript.json"))
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- fresh_bundle("unsupported-schema")
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

test_that("failed STORM replacement preserves the previous bundle byte-for-byte", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir()
  run_dir <- file.path(root, "atomic-run")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    "atomic-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  workspace <- tempest_research_workspace()
  original <- tempest:::tempest_storm_state(
    "Atomic STORM",
    title = "Original title"
  )
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    original,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  bundle_bytes <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, all.files = TRUE))
    stats::setNames(
      lapply(files, function(file) {
        readBin(
          file.path(path, file),
          what = "raw",
          n = file.info(
            file.path(path, file)
          )$size
        )
      }),
      files
    )
  }
  before <- bundle_bytes(run_dir)
  replacement <- tempest:::tempest_storm_state(
    "Atomic STORM",
    title = "Replacement title"
  )
  withr::local_options(
    tempest.run_write_hook = function(file) {
      if (identical(file, "stage_records.json")) {
        stop("injected STORM write failure")
      }
    }
  )

  expect_error(
    tempest:::tempest_save_run_artifacts(
      run_dir,
      workspace,
      replacement,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )

  expect_identical(bundle_bytes(run_dir), before)
  remnants <- list.files(root, all.files = TRUE, no.. = TRUE)
  remnants <- remnants[grepl("^\\.atomic-run-(staging|backup)-", remnants)]
  expect_identical(remnants, character())
  restored <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "atomic-run"
  )
  expect_identical(restored$state$title, "Original title")
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

test_that("partial recovery rejects missing and unsafe suggestion files", {
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
  root <- withr::local_tempdir()
  bundle_dir <- file.path(root, "missing-suggestions")

  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  unlink(questions_path)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  expect_error(
    tempest:::tempest_session_bundle_validate_manifest(
      bundle_dir,
      manifest,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- file.path(root, "symlinked-suggestions")
  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  external_path <- tempfile("tempest-external-suggestions-")
  writeLines('["Outside bundle"]', external_path)
  unlink(questions_path)
  linked <- file.symlink(external_path, questions_path)
  if (isTRUE(linked)) {
    manifest <- tempest:::tempest_read_json_strict(
      file.path(bundle_dir, "session.json")
    )
    expect_error(
      tempest:::tempest_session_bundle_validate_manifest(
        bundle_dir,
        manifest,
        partial_recovery = TRUE
      ),
      class = "tempest_session_restore_error"
    )
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
  root <- withr::local_tempdir()
  critical_files <- c(
    "experts.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "stage_records.json",
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json"
  )

  for (index in seq_along(critical_files)) {
    critical_file <- critical_files[[index]]
    bundle_dir <- file.path(root, paste0("critical-", index))
    tempest_session_save(session, bundle_dir)
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

  bundle_dir <- file.path(root, "malformed-claims")
  tempest_session_save(session, bundle_dir)
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

  bundle_dir <- file.path(root, "workflow-sidecar")
  tempest_session_save(session, bundle_dir)
  manifest_path <- file.path(bundle_dir, "session.json")
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

test_that("schema 7 run bundles restore workspace, state, and manifest", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Lithium Batteries")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/source",
    title = "Example Source",
    snippet = "Snippet",
    content_text = "Lithium batteries store energy."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    source_id = source$id,
    quote = "Lithium batteries store energy.",
    evidence_span_id = "span.lithium-run",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Lithium batteries store energy.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Lithium batteries store energy."),
    confidence = "high"
  ))
  report_md <- tempest_report_md(
    title = "Lithium Batteries",
    body = paste0(
      "Lithium batteries store energy. [",
      source$id,
      "]"
    ),
    workspace = workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
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
        title = "Technology",
        summary = "Summary",
        subsections = list()
      ))
    ),
    outline = list(
      title = "Lithium Batteries",
      sections = list(list(
        title = "Technology",
        summary = "Summary",
        subsections = list()
      ))
    ),
    lead_section = "Lithium batteries store energy.",
    draft_md = paste0(
      "Lithium batteries store energy.\n\n",
      "## Technology\n\n",
      "Lithium batteries store energy."
    ),
    report_md = report_md,
    completed_stages = c(
      "perspectives",
      "research",
      "outline",
      "write",
      "polish"
    )
  )
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source$id,
      verification_status = "supported",
      support_score = 0.9,
      rationale = "matches source"
    )),
    verified_at = "2026-08-16T00:00:00Z"
  )

  research_manifest <- tempest_research_manifest(
    research_run_id = "lithium-run",
    mode = "storm",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    status = "succeeded"
  )
  state$stage_records <- test_persistence_storm_stage_records(
    state,
    workspace,
    research_manifest
  )
  state$report_md <- tempest:::tempest_markdown_append_execution_review(
    state$report_md,
    tempest:::tempest_stage_records_execution_review(state$stage_records)
  )
  state <- tempest:::tempest_storm_state_validate(state)
  for (stage in c(
    "perspectives",
    "personas",
    "query_decomposition",
    "extract_claims",
    "draft_outline",
    "refined_outline",
    "lead_section",
    "section_writing"
  )) {
    erased <- Filter(
      \(record) !identical(record@stage, stage),
      state$stage_records
    )
    expect_error(
      tempest:::tempest_stage_records_validate_storm_coverage(erased, state),
      class = "tempest_stage_record_error"
    )
  }
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    research_manifest,
    program_set = program_set,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions",
    parallel_writing = TRUE,
    remove_duplicate = TRUE
  )

  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
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
  expect_equal(loaded$state$draft_md, state$draft_md)
  expect_equal(loaded$state$report_md, state$report_md)
  expect_length(loaded$state$stage_records, 9L)
  expect_identical(loaded$state$stage_records[[1]]@status, "succeeded")
  expect_identical(
    loaded$state$stage_records[[1]]@output_reference,
    tempest:::tempest_stage_output_reference(
      "state_field",
      c("title", "perspectives"),
      content_digest = tempest:::tempest_stage_state_output_digest(
        "perspectives",
        list(title = state$title, perspectives = state$perspectives)
      )
    )
  )
  expect_equal(loaded$state$experts[[1]]@expert_id, "expert.technical")
  expect_s7_class(loaded$state$experts[[1]], TempestExpertProfile)
  expect_false("artifact_catalog" %in% names(loaded))
  expect_s3_class(tempest_claim_supports(loaded$workspace), "tbl_df")
  expect_equal(nrow(tempest_claim_supports(loaded$workspace)), 1)
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
  expect_equal(loaded$metadata$schema_version, 7L)
  expect_identical(loaded$metadata$bundle_type, "storm")
  expect_identical(loaded$metadata$bundle_status, "complete")
  expect_type(loaded$metadata$research_manifest, "list")
  expect_equal(
    file.exists(file.path(run_dir, "research_manifest.json")),
    FALSE
  )
  expect_equal(file.exists(file.path(run_dir, "experts.json")), TRUE)
  expect_equal(file.exists(file.path(run_dir, "stage_records.json")), TRUE)
  expect_equal(file.exists(file.path(run_dir, "personas.json")), FALSE)

  sidecar_path <- file.path(run_dir, "stage_records.json")
  manifest_path <- file.path(run_dir, "run_config.json")
  records <- tempest:::tempest_read_json_strict(sidecar_path)
  valid_records <- records
  valid_manifest <- tempest:::tempest_read_json_strict(manifest_path)
  double_schema <- readLines(manifest_path, warn = FALSE)
  double_schema <- sub(
    '"schema_version": 7,',
    '"schema_version": 7.0,',
    double_schema,
    fixed = TRUE
  )
  tempest:::tempest_atomic_write_lines(double_schema, manifest_path)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )
  tempest:::tempest_write_json(manifest_path, valid_manifest)
  tempest:::tempest_write_json(sidecar_path, list())
  erased_manifest <- valid_manifest
  erased_manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      run_dir,
      "stage_records.json"
    )
  tempest:::tempest_write_json(manifest_path, erased_manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )

  tempest:::tempest_write_json(sidecar_path, valid_records)
  tempest:::tempest_write_json(manifest_path, valid_manifest)
  changed_manifest_trace <- valid_manifest
  changed_manifest_trace$research_manifest$traces[[1]]$status <- "failed"
  tempest:::tempest_write_json(manifest_path, changed_manifest_trace)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )

  tempest:::tempest_write_json(manifest_path, valid_manifest)
  records <- valid_records
  records[[1]]$program_artifact_id <- paste0("sha256:", strrep("0", 64L))
  tempest:::tempest_write_json(sidecar_path, records)
  persisted_manifest <- tempest:::tempest_read_json_strict(manifest_path)
  persisted_manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      run_dir,
      "stage_records.json"
    )
  tempest:::tempest_write_json(manifest_path, persisted_manifest)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("run restore rejects tampered expert-profile records", {
  skip_if_not_installed("jsonlite")
  run_dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
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
  manifest <- tempest_research_manifest(
    "run-integrity",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  state <- test_persistence_bind_storm_records(state, workspace, manifest)
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "perspectives",
    research_strategy = "key_questions"
  )
  experts_path <- file.path(run_dir, "experts.json")
  records <- tempest:::tempest_read_json_strict(experts_path)
  for (field in c(
    "version",
    "state",
    "schema_version",
    "focus_areas",
    "metadata"
  )) {
    null_field <- records
    null_field[[1]][field] <- list(NULL)
    expect_error(
      tempest:::tempest_experts_from_records(
        null_field,
        what = "STORM test expert profiles",
        class = c("tempest_run_restore_error", "tempest_error")
      ),
      class = "tempest_run_restore_error",
      regexp = "non-null writer fields",
      info = field
    )
  }
  double_schema <- records
  double_schema[[1]]$schema_version <- 1.0
  expect_error(
    tempest:::tempest_experts_from_records(
      double_schema,
      what = "STORM test expert profiles",
      class = c("tempest_run_restore_error", "tempest_error")
    ),
    class = "tempest_run_restore_error",
    regexp = "non-null writer fields"
  )
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
      program_set = program_set,
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
  program_set <- tempest_program_set()
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
  manifest <- tempest_research_manifest(
    "partial-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  state <- test_persistence_bind_storm_records(state, workspace, manifest)

  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions"
  )

  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
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
  expect_identical(
    loaded$state$requested_steps,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_identical(
    unlist(loaded$metadata$requested_steps, use.names = FALSE),
    loaded$state$requested_steps
  )
})

test_that("STORM requested steps persist canonically and remain immutable", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir()
  run_dir <- file.path(root, "requested-steps")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace()
  manifest <- tempest_research_manifest(
    "requested-steps",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  state <- tempest:::tempest_storm_state("Requested steps")

  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("polish", "research", "write"),
    research_strategy = "key_questions"
  )
  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "requested-steps"
  )
  expect_identical(
    loaded$state$requested_steps,
    c("research", "write", "polish")
  )
  expect_identical(loaded$completed_stages, character())

  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("write", "polish", "research"),
    research_strategy = "key_questions"
  )
  expect_error(
    tempest:::tempest_save_run_artifacts(
      run_dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = c("research", "outline"),
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
  restored <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "requested-steps"
  )
  expect_identical(restored$state$requested_steps, loaded$state$requested_steps)
})

test_that("succeeded STORM publication requires the full dependency chain", {
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  expect_error(
    tempest:::tempest_save_run_artifacts(
      withr::local_tempdir(),
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Partial publication"),
      tempest_research_manifest(
        "partial-publication",
        mode = "storm",
        config = cfg,
        programs = tempest:::tempest_program_set_manifest_programs(program_set),
        status = "succeeded"
      ),
      program_set = program_set,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error"
  )
})

test_that("completed STORM product state fails closed when artifacts drift", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-completed-state-")
    dir.create(dir)
    cfg <- tempest_config()
    outline <- list(
      title = "Durable state",
      sections = list(list(
        title = "Findings",
        summary = "Summary",
        subsections = list(list(
          title = "Evidence",
          bullets = "Durable evidence",
          needed = "What is authoritative?"
        ))
      ))
    )
    workspace <- tempest_research_workspace()
    source <- tempest:::tempest_source(
      "https://example.com/durable-state",
      title = "Durable source",
      content_text = "Durable evidence supports the durable claim."
    )
    workspace$upsert_retrieved_resource(source)
    span_id <- workspace$add_evidence_span(tempest_evidence_span(
      evidence_span_id = "span-durable-state",
      source_id = source$id,
      quote = "Durable evidence supports the durable claim.",
      extracted_by = program_references$extract_claims$program_artifact_id
    ))
    claim_id <- workspace$add_proposed_claim(tempest_claim(
      claim_id = "claim-durable-state",
      claim_text = "Durable evidence supports the durable claim.",
      source_ids = source$id,
      evidence_span_ids = span_id,
      supporting_quotes = list("Durable evidence supports the durable claim."),
      verification_status = "supported",
      support_score = 0.9
    ))
    workspace$verify_proposed_claims_batch(
      list(tempest_claim_support(
        claim_id = claim_id,
        evidence_span_id = span_id,
        source_id = source$id,
        verification_status = "supported",
        support_score = 0.9,
        rationale = "The durable source directly supports the claim."
      )),
      verified_at = "2026-08-16T00:00:00Z"
    )
    report_md <- tempest_report_md(
      title = "Durable state",
      body = paste0(
        "Durable evidence supports the durable claim. [",
        source$id,
        "]"
      ),
      workspace = workspace,
      citation_policy = cfg@citation_policy,
      on_unsupported_claim = cfg@on_unsupported_claim,
      min_support_score = cfg@min_support_score
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
      lead_section = "Durable evidence supports the durable claim.",
      draft_md = paste0(
        "Durable evidence supports the durable claim.\n\n",
        "## Findings\n\n",
        "Durable evidence supports the durable claim."
      ),
      report_md = report_md,
      completed_stages = c(
        "perspectives",
        "research",
        "outline",
        "write",
        "polish"
      )
    )
    manifest <- tempest_research_manifest(
      "completed-state",
      config = cfg,
      programs = program_references
    )
    state <- test_persistence_bind_storm_records(state, workspace, manifest)
    tempest:::tempest_save_run_artifacts(
      dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
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
        program_set = program_set,
        run_id = "completed-state"
      ),
      class = "tempest_run_restore_error"
    )
  }

  bundle <- make_bundle()
  loaded <- tempest:::tempest_load_run_artifacts(
    bundle$dir,
    config = bundle$config,
    program_set = program_set,
    run_id = "completed-state"
  )
  expect_identical(
    names(loaded$state$perspectives[[1]]),
    tempest:::tempest_storm_perspective_fields()
  )
  expect_identical(
    names(loaded$state$outline),
    tempest:::tempest_storm_outline_fields()
  )
  expect_identical(
    names(loaded$state$outline$sections[[1]]),
    tempest:::tempest_storm_outline_section_fields()
  )
  expect_identical(
    names(loaded$state$outline$sections[[1]]$subsections[[1]]),
    tempest:::tempest_storm_outline_subsection_fields()
  )

  bundle <- make_bundle()
  perspectives <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "perspectives.json")
  )
  perspectives[[1]] <- perspectives[[1]][rev(names(perspectives[[1]]))]
  rewrite_checked(bundle, "perspectives.json", perspectives)
  expect_rejected(bundle)

  bundle <- make_bundle()
  outline <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "direct_gen_outline.json")
  )
  outline <- outline[rev(names(outline))]
  rewrite_checked(bundle, "direct_gen_outline.json", outline)
  expect_rejected(bundle)

  bundle <- make_bundle()
  outline <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "storm_gen_outline.json")
  )
  outline$sections[[1]] <- outline$sections[[1]][
    rev(names(outline$sections[[1]]))
  ]
  rewrite_checked(bundle, "storm_gen_outline.json", outline)
  expect_rejected(bundle)

  bundle <- make_bundle()
  outline <- tempest:::tempest_read_json_strict(
    file.path(bundle$dir, "storm_gen_outline.json")
  )
  outline$sections[[1]]$subsections[[1]] <-
    outline$sections[[1]]$subsections[[1]][
      rev(names(outline$sections[[1]]$subsections[[1]]))
    ]
  rewrite_checked(bundle, "storm_gen_outline.json", outline)
  expect_rejected(bundle)

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
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace()
  state <- tempest:::tempest_storm_state("t")
  manifest <- tempest_research_manifest(
    "undeclared-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "polish",
    research_strategy = "key_questions"
  )
  writeLines("UNDECLARED", file.path(dir, "storm_gen_article_polished.md"))

  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
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
  program_set <- tempest_program_set()

  expect_error(
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Unowned files"),
      tempest_research_manifest(
        "unowned-files",
        config = cfg,
        programs = tempest:::tempest_program_set_manifest_programs(program_set)
      ),
      program_set = program_set,
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
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-run-")
    dir.create(dir)
    cfg <- tempest_config()
    tempest:::tempest_save_run_artifacts(
      dir,
      tempest_research_workspace(),
      tempest:::tempest_storm_state("t"),
      tempest_research_manifest(
        "manifest-run",
        config = cfg,
        programs = program_references
      ),
      program_set = program_set,
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
      program_set = program_set,
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
      program_set = program_set,
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
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/write-order",
    title = "Write order source",
    content_text = "Durable evidence is written before its manifest."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-write-order",
    source_id = source$id,
    quote = "Durable evidence is written before its manifest.",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-write-order",
    claim_text = "Durable evidence is written before its manifest.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    supporting_quotes = list(
      "Durable evidence is written before its manifest."
    ),
    verification_status = "supported",
    support_score = 0.9
  ))
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim_id,
      evidence_span_id = span_id,
      source_id = source$id,
      verification_status = "supported",
      support_score = 0.9,
      rationale = "The exact source supports the durable claim."
    )),
    verified_at = "2026-08-16T00:00:00Z"
  )
  state <- tempest:::tempest_storm_state(
    "t",
    outline = list(
      title = "Draft",
      sections = list(list(title = "Findings", summary = "Draft"))
    ),
    lead_section = "Durable evidence is written before its manifest.",
    draft_md = paste0(
      "Durable evidence is written before its manifest.\n\n",
      "## Findings\n\n",
      "Durable evidence is written before its manifest."
    ),
    completed_stages = c("research", "write")
  )
  manifest <- tempest_research_manifest(
    "write-order",
    config = cfg,
    programs = program_references
  )
  state <- test_persistence_bind_storm_records(state, workspace, manifest)

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
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
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  s1 <- tempest:::tempest_source(url = "https://example.com/a", title = "A")
  s2 <- tempest:::tempest_source(url = "https://example.com/b", title = "B")
  workspace$upsert_retrieved_resource(s1)
  workspace$upsert_retrieved_resource(s2)
  report_md <- tempest_report_md(
    title = "t",
    body = paste0("A cited claim [", s1$id, "]."),
    workspace = workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  state <- tempest:::tempest_storm_state(
    "t",
    draft_md = "# Draft",
    report_md = report_md,
    completed_stages = "polish"
  )

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    tempest_research_manifest(
      "references-run",
      config = cfg,
      programs = program_references
    ),
    program_set = program_set,
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
    program_set = program_set,
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
      program_set = program_set,
      run_id = "references-run"
    ),
    class = "tempest_run_restore_error"
  )

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    tempest_research_manifest(
      "references-run",
      config = cfg,
      programs = program_references
    ),
    program_set = program_set,
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
      program_set = program_set,
      run_id = "references-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("schema 6 STORM bundles fail closed", {
  dir <- withr::local_tempdir()
  tempest:::tempest_write_json(
    file.path(dir, "run_config.json"),
    list(schema_version = 6L)
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(dir),
    class = "tempest_unsupported_format_error"
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      list(
        schema_version = 6L,
        workspace = list(
          base_snapshot_id = NULL,
          max_sources = "unbounded",
          accepted_graft_references = list()
        )
      ),
      tempest_config()
    ),
    class = "tempest_unsupported_format_error"
  )
})

test_that("schema 7 resume protects run and config identity", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace()
  manifest <- tempest_research_manifest(
    "protected-run",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    status = "failed"
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    tempest:::tempest_storm_state("Protected run"),
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )

  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "different-run"
    ),
    class = "tempest_run_restore_error"
  )
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = tempest_config(max_search_results = 4L),
      program_set = program_set,
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
      program_set = program_set,
      run_id = "protected-run"
    ),
    class = "tempest_run_restore_error"
  )

  restored <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    program_set = program_set,
    run_id = "protected-run"
  )
  expect_identical(restored$research_manifest@status, "failed")
  expect_identical(restored$research_manifest@programs, manifest@programs)
})

test_that("STORM persistence verifies complete ProgramSet identity on resume", {
  root <- withr::local_tempdir()
  run_dir <- file.path(root, "run")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  manifest <- tempest_research_manifest(
    "program-set-resume",
    config = cfg,
    programs = program_references
  )
  state <- tempest:::tempest_storm_state("ProgramSet resume")

  tempest:::tempest_save_run_artifacts(
    run_dir,
    tempest_research_workspace(),
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research",
    research_strategy = "key_questions"
  )
  manifest_path <- file.path(run_dir, "run_config.json")
  persisted <- tempest:::tempest_read_json_strict(manifest_path)
  restored_manifest <- tempest:::tempest_research_manifest_from_record(
    persisted$research_manifest
  )

  expect_identical(restored_manifest@programs, program_references)
  expect_identical(
    test_contains_runtime_value(persisted$research_manifest),
    FALSE
  )
  program_files <- unlist(persisted$files, use.names = FALSE)
  expect_length(program_files[startsWith(program_files, "programs/")], 0L)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "explicit complete TempestProgramSet"
  )

  file_program_set <- tempest_save_program_set(
    program_set,
    file.path(root, "program-set")
  )
  relocated <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    program_set = file_program_set,
    run_id = "program-set-resume"
  )
  expect_s7_class(relocated$program_set, TempestProgramSet)
  expect_identical(
    tempest:::tempest_program_set_identity_equal(
      relocated$program_set,
      restored_manifest@programs
    ),
    TRUE
  )

  tampered <- persisted
  tampered$research_manifest$programs$personas <- NULL
  tempest:::tempest_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "every exact ProgramSet stage"
  )

  tampered <- persisted
  tampered$research_manifest$programs$perspectives$program_artifact_id <-
    paste0("sha256:", strrep("0", 64L))
  tempest:::tempest_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "identity does not match"
  )

  tampered <- persisted
  tampered$research_manifest$programs$perspectives$evaluator_version <- "999"
  tempest:::tempest_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "identity does not match"
  )

  tampered <- persisted
  tampered$research_manifest$programs$perspectives$artifact_reference <- NULL
  tempest:::tempest_write_json(manifest_path, tampered)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error",
    regexp = "research manifest is invalid"
  )
  tempest:::tempest_write_json(manifest_path, persisted)

  corrupt_program_set <- tempest_program_set()
  corrupt_program <- tempest:::tempest_program_set_program(
    corrupt_program_set,
    "perspectives"
  )
  corrupt_program$config$identity_corruption <- "changed"
  expect_error(
    tempest:::tempest_load_run_artifacts(
      run_dir,
      config = cfg,
      program_set = corrupt_program_set,
      run_id = "program-set-resume"
    ),
    class = "tempest_run_restore_error"
  )

  missing_save_dir <- file.path(root, "missing-program-set")
  expect_error(
    tempest:::tempest_save_run_artifacts(
      missing_save_dir,
      tempest_research_workspace(),
      state,
      manifest,
      config = cfg,
      steps = "research",
      research_strategy = "key_questions"
    ),
    class = "tempest_run_persistence_error",
    regexp = "explicit complete TempestProgramSet"
  )
  expect_identical(
    file.exists(file.path(missing_save_dir, "run_config.json")),
    FALSE
  )
})

test_that("schema 7 STORM bundles round-trip the complete workspace", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace(
    max_sources = 4L,
    accepted_graft_references = list(
      list(record_id = "claim.accepted", revision_id = "revision-7")
    )
  )
  source <- tempest:::tempest_source(
    "https://example.com/current-study",
    title = "Current study",
    snippet = "The study reports a reproducible result.",
    content_text = "The study reports a reproducible result."
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
    quote = "The assay was reviewed before use.",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  source_span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-current-study",
    source_id = source$id,
    quote = "The study reports a reproducible result.",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-complete-workspace",
    claim_text = "The result used a reviewed assay.",
    source_ids = c(resource@resource_id, source$id),
    evidence_span_ids = c(span_id, source_span_id),
    supporting_quotes = list(
      "The assay was reviewed before use.",
      "The study reports a reproducible result."
    ),
    confidence = "high"
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-complete-workspace",
    topic = "Assay review",
    claim_ids = claim_id,
    evidence_balance = "agreement"
  ))
  workspace$verify_proposed_claims_batch(
    lapply(
      c(span_id, source_span_id),
      function(evidence_span_id) {
        span <- workspace$get_evidence_span(evidence_span_id)
        tempest_claim_support(
          claim_id = claim_id,
          evidence_span_id = evidence_span_id,
          source_id = span@source_id,
          verification_status = "supported",
          support_score = 0.92,
          rationale = "The exact span directly supports the claim."
        )
      }
    ),
    verified_at = "2026-08-16T00:00:00Z"
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  state <- tempest:::tempest_storm_state(
    "Complete workspace",
    completed_stages = "research"
  )
  manifest <- tempest_research_manifest(
    "complete-workspace",
    config = cfg,
    programs = program_references
  )
  state <- test_persistence_bind_storm_records(state, workspace, manifest)

  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
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
    any(c("sources.json", "claims.json", "claim_supports.json") %in% declared),
    FALSE
  )
  expect_equal(file.exists(file.path(dir, "workspace.json")), TRUE)
  expect_equal(file.exists(file.path(dir, "sources.json")), FALSE)
  expect_equal(file.exists(file.path(dir, "claims.json")), FALSE)
  expect_equal(file.exists(file.path(dir, "claim_supports.json")), FALSE)

  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    program_set = program_set,
    run_id = "complete-workspace"
  )

  expect_identical(
    tempest:::tempest_research_workspace_snapshot(loaded$workspace),
    snapshot
  )

  workspace_path <- file.path(dir, "workspace.json")
  downgraded <- tempest:::tempest_read_json_strict(workspace_path)
  downgraded$schema_version <- 4L
  tempest:::tempest_write_json(workspace_path, downgraded)
  metadata$checksums[["workspace.json"]] <-
    tempest:::tempest_session_bundle_checksum(dir, "workspace.json")
  tempest:::tempest_write_json(file.path(dir, "run_config.json"), metadata)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "complete-workspace"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("STORM workspace files match the exact manifest identity", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
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
      tempest_research_manifest(
        "workspace-identity",
        config = cfg,
        programs = program_references
      ),
      program_set = program_set,
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
        program_set = program_set,
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
    tempest_session_restore(list(schema_version = 7.5)),
    class = session_class
  )
  expect_error(
    tempest:::tempest_session_bundle_validate_manifest(
      withr::local_tempdir(),
      list(schema_version = 7.5)
    ),
    class = session_class
  )
  expect_error(
    tempest:::tempest_run_bundle_validate_manifest(
      withr::local_tempdir(),
      list(schema_version = 6.5)
    ),
    class = run_class
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      list(schema_version = 6.5),
      cfg
    ),
    class = run_class
  )
  expect_error(
    tempest:::tempest_storm_restore_manifest(
      list(schema_version = 6.5),
      tempest_research_workspace(),
      tempest:::tempest_storm_state("Fractional schema"),
      cfg,
      program_set = NULL,
      run_dir = withr::local_tempdir()
    ),
    class = run_class
  )
})

test_that("schema 7 STORM manifests have an exact product envelope", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
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
    workspace <- tempest_research_workspace()
    manifest <- tempest_research_manifest(
      "exact-storm",
      config = cfg,
      programs = program_references
    )
    state <- test_persistence_bind_storm_records(state, workspace, manifest)
    tempest:::tempest_save_run_artifacts(
      dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = c("perspectives", "research"),
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }

  bundle <- make_bundle()
  manifest_path <- file.path(bundle$dir, "run_config.json")
  original <- tempest:::tempest_read_json_strict(manifest_path)
  expect_identical(
    names(original),
    tempest:::tempest_run_bundle_manifest_fields()
  )

  reordered <- original[rev(names(original))]
  tempest:::tempest_write_json(manifest_path, reordered)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  reordered_workspace <- original
  reordered_workspace$workspace <- reordered_workspace$workspace[
    rev(names(reordered_workspace$workspace))
  ]
  tempest:::tempest_write_json(manifest_path, reordered_workspace)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      bundle$dir,
      config = bundle$config,
      program_set = program_set,
      run_id = "exact-storm"
    ),
    class = "tempest_run_restore_error"
  )

  for (field in c("topic", "title")) {
    invalid <- original
    invalid[[field]] <- NULL
    tempest:::tempest_write_json(manifest_path, invalid)
    expect_error(
      tempest:::tempest_load_run_artifacts(
        bundle$dir,
        config = bundle$config,
        program_set = program_set,
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
      program_set = program_set,
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
      program_set = program_set,
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
      program_set = program_set,
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
    program_set = program_set,
    run_id = "exact-storm"
  )
  expect_identical(restored$completed_stages, character())
})

test_that("schema 7 STORM declared JSON fails closed", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
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
    workspace <- tempest_research_workspace()
    manifest <- tempest_research_manifest(
      "strict-storm",
      config = cfg,
      programs = program_references
    )
    state <- test_persistence_bind_storm_records(state, workspace, manifest)
    tempest:::tempest_save_run_artifacts(
      dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = "perspectives",
      research_strategy = "key_questions"
    )
    list(dir = dir, config = cfg)
  }

  for (file in c(
    "perspectives.json",
    "references.json",
    "stage_records.json"
  )) {
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
        program_set = program_set,
        run_id = "strict-storm"
      ),
      class = "tempest_run_restore_error"
    )
  }
})

test_that("schema 7 manifests require files implied by completed stages", {
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
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
    workspace <- tempest_research_workspace()
    manifest <- tempest_research_manifest(
      "stage-files",
      config = cfg,
      programs = program_references
    )
    state <- test_persistence_bind_storm_records(state, workspace, manifest)
    tempest:::tempest_save_run_artifacts(
      dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
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
      program_set = program_set,
      run_id = "stage-files"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("STORM resume accepts only an equivalent supplied workspace", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  workspace <- tempest_research_workspace(
    max_sources = 8L,
    accepted_graft_references = list(
      list(record_id = "accepted-a", revision_id = "revision-a")
    )
  )
  source <- tempest:::tempest_source(
    "https://example.com/authoritative",
    title = "Authoritative source",
    snippet = "Persisted source",
    content_text = "Persisted source. This span was not persisted."
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
    evidence_span_ids = "span-authoritative",
    supporting_quotes = list("Persisted source."),
    confidence = "high"
  )
  workspace$add_extracted_claim_batch(
    list(claim),
    list(tempest_evidence_span(
      source_id = source$id,
      quote = "Persisted source.",
      evidence_span_id = "span-authoritative",
      extracted_by = tempest:::tempest_program_set_manifest_programs(
        program_set
      )$extract_claims$program_artifact_id
    ))
  )
  workspace$verify_proposed_claims_batch(
    list(tempest_claim_support(
      claim_id = claim@claim_id,
      evidence_span_id = "span-authoritative",
      source_id = source$id,
      verification_status = "supported",
      support_score = 0.9,
      rationale = "The persisted source directly supports the claim."
    )),
    verified_at = "2026-08-16T00:00:00Z"
  )
  manifest <- tempest_research_manifest(
    "authoritative-workspace",
    config = cfg,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  state <- tempest:::tempest_storm_state(
    "Authoritative workspace",
    completed_stages = "research"
  )
  state <- test_persistence_bind_storm_records(state, workspace, manifest)
  tempest:::tempest_save_run_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
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
    program_set = program_set,
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
    program_set = program_set,
    run_id = "authoritative-workspace"
  )
  expect_identical(loaded_empty$workspace, empty)
  expect_identical(
    tempest:::tempest_storm_workspace_equivalence_record(empty),
    persisted_record
  )

  fresh_workspace <- function() {
    candidate_snapshot <- snapshot
    candidate_snapshot$claim_supports <- list()
    candidate_snapshot$proposed_claims <- lapply(
      candidate_snapshot$proposed_claims,
      function(record) {
        record$verification_status <- "unverified"
        record["support_score"] <- list(NULL)
        record["verified_at"] <- list(NULL)
        record["verifier_model"] <- list(NULL)
        record
      }
    )
    tempest:::tempest_research_workspace_restore(candidate_snapshot)
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
  changed_source_metadata <- fresh_workspace()
  source_record <- changed_source_metadata$get_retrieved_resource(source$id)
  source_record <- S7::set_props(
    source_record,
    metadata = list(revision_id = "changed")
  )
  changed_source_metadata$upsert_retrieved_resource(source_record)
  changed_source_timestamp <- fresh_workspace()
  source_record <- changed_source_timestamp$get_retrieved_resource(source$id)
  source_record <- S7::set_props(
    source_record,
    retrieved_at = "2026-08-15T15:00:00Z"
  )
  changed_source_timestamp$upsert_retrieved_resource(source_record)
  changed_resource_metadata <- fresh_workspace()
  resource_record <- changed_resource_metadata$get_retrieved_resource(
    resource@resource_id
  )
  resource_record <- S7::set_props(
    resource_record,
    metadata = list(revision_id = "protocol-b")
  )
  changed_resource_metadata$upsert_retrieved_resource(resource_record)
  changed_resource_timestamp <- fresh_workspace()
  resource_record <- changed_resource_timestamp$get_retrieved_resource(
    resource@resource_id
  )
  resource_record <- S7::set_props(
    resource_record,
    retrieved_at = "2026-08-15T16:00:00Z"
  )
  changed_resource_timestamp$upsert_retrieved_resource(resource_record)
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
  changed_supports_snapshot <- snapshot
  changed_supports_snapshot$claim_supports[[1]]$rationale <-
    "A different exact support rationale."
  changed_supports <- tempest:::tempest_research_workspace_restore(
    changed_supports_snapshot
  )
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
    changed_supports
  )
  for (candidate in divergent) {
    expect_error(
      tempest:::tempest_load_run_artifacts(
        dir,
        workspace = candidate,
        config = cfg,
        program_set = program_set,
        run_id = "authoritative-workspace"
      ),
      class = "tempest_run_restore_error"
    )
  }
})
