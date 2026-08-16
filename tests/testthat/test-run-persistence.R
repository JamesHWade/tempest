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
  workspace$upsert_source(source)
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
      "resources",
      "sources",
      "proposed_claims",
      "evidence_spans",
      "disputes",
      "citation_audit"
    )
  )
  expect_equal("artifacts" %in% names(snapshot), FALSE)
  expect_length(snapshot$resources, 1L)
  expect_length(snapshot$sources, 1L)
  expect_identical(
    snapshot$resources[[1]]$resource_id,
    resource@resource_id
  )
  expect_identical(is.na(snapshot$sources[[1]]$fetched_at), TRUE)
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
  expect_equal(inherits(restored, "SourceStore"), FALSE)
  expect_identical(restored$base_snapshot_id, "snapshot-a")
  expect_equal(restored$max_sources, 4L)
  expect_equal(
    restored$list_accepted_graft_references(),
    workspace$list_accepted_graft_references()
  )
  expect_equal(restored$get_source(source$id)$title, "Workspace source")
  expect_equal(
    restored$get_proposed_claim(claim_id)@claim_text,
    "Workspaces preserve provisional evidence."
  )
  expect_equal(
    restored$get_evidence_for_claim(claim_id)[[1]]@quote,
    "workspace evidence"
  )
  expect_equal(restored$list_disputes()[[1]]@dispute_id, "dispute-a")
  expect_equal(restored$citation_audit$support_score, 0.95)
  expect_equal("artifacts" %in% names(restored), FALSE)

  workspace$record_accepted_graft_reference(list(record_id = "record-new"))
  workspace$upsert_source(tempest:::tempest_source(
    "https://example.com/workspace",
    title = "Changed after snapshot"
  ))
  current_audit <- workspace$citation_audit
  current_audit$support_score <- 0.25
  expect_equal(length(snapshot$accepted_graft_references), 2L)
  expect_equal(snapshot$sources[[1]]$title, "Workspace source")
  expect_equal(snapshot$resources[[1]]$title, "Workspace protocol")
  expect_equal(snapshot$citation_audit[[1]]$support_score, 0.95)

  snapshot$accepted_graft_references[[1]]$record_id <- "tampered"
  snapshot$sources[[1]]$title <- "Tampered snapshot"
  expect_equal(
    restored$accepted_graft_references[[1]]$record_id,
    "record-a"
  )
  expect_equal(restored$get_source(source$id)$title, "Workspace source")
})

test_that("ResearchWorkspace restores mirrored schema 3 web sources", {
  source <- tempest:::tempest_source(
    "https://example.com/schema-3-mirror",
    title = "Legacy mirror",
    snippet = "The typed resource is authoritative.",
    fetched_at = "2026-08-14T09:00:00Z"
  )
  workspace <- tempest_research_workspace(max_sources = 2L)
  workspace$upsert_source(source)
  legacy <- tempest:::tempest_research_workspace_snapshot(workspace)
  legacy$schema_version <- 3L
  legacy$resources <- list(tempest:::tempest_resource_record(
    tempest:::tempest_source_as_resource(source)
  ))
  legacy$sources <- list(source)

  restored <- tempest:::tempest_research_workspace_restore(legacy)

  expect_length(restored$retrieved_resources, 1L)
  expect_s7_class(
    restored$get_retrieved_resource(source$id),
    tempest:::TempestResource
  )
  expect_identical(restored$get_source(source$id)$title, "Legacy mirror")
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

test_that("ResearchWorkspace restore bounds legacy artifact migration", {
  withr::local_options(lifecycle_verbosity = "quiet")
  store <- quiet_source_store(
    max_sources = 3L,
    base_snapshot_id = "snapshot-legacy",
    accepted_graft_references = list(list(record_id = "record-a"))
  )
  audit <- tibble::tibble(
    claim_id = "claim-a",
    claim_text = "A legacy audit",
    verification_status = "supported",
    support_score = 1,
    rationale = "Direct support"
  )
  store$add_claim(tempest_claim(
    claim_id = "claim-a",
    claim_text = "A legacy audit",
    verification_status = "supported",
    support_score = 1
  ))
  store$set_artifact("citation_audit", audit)
  store$set_artifact("report_md", "# Legacy report")
  snapshot <- tempest:::tempest_source_store_snapshot(store)

  restored <- tempest:::tempest_research_workspace_restore(snapshot)

  expect_r6_class(restored, "ResearchWorkspace")
  expect_equal(inherits(restored, "SourceStore"), FALSE)
  expect_identical(restored$base_snapshot_id, "snapshot-legacy")
  expect_equal(restored$max_sources, 3L)
  expect_equal(
    restored$accepted_graft_references,
    list(list(record_id = "record-a"))
  )
  expect_equal(restored$citation_audit, audit)
  expect_equal("artifacts" %in% names(restored), FALSE)

  withr::local_options(lifecycle_verbosity = "warning")
  expect_no_warning({
    compatible <- tempest:::tempest_source_store_restore(snapshot)
  })
  expect_equal(compatible$get_artifact("report_md"), "# Legacy report")
  expect_equal(compatible$get_artifact("citation_audit"), audit)
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
  malformed$resources <- list(list(resource_id = "foreign-record"))
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

test_that("SourceStore snapshots restore durable ledger state", {
  withr::local_options(lifecycle_verbosity = "quiet")
  store <- quiet_source_store()
  source <- tempest:::tempest_source(
    "https://example.com/snapshot",
    title = "Snapshot Source",
    snippet = "Snapshot snippet"
  )
  store$upsert_source(source)
  span_id <- store$add_evidence_span(tempest_evidence_span(
    source_id = source$id,
    quote = "snapshot evidence"
  ))
  claim_id <- store$add_claim(tempest_claim(
    claim_text = "Snapshots preserve evidence.",
    source_ids = source$id,
    evidence_span_ids = span_id,
    confidence = "high"
  ))
  store$add_dispute(tempest_dispute(
    topic = "snapshot completeness",
    claim_ids = claim_id,
    evidence_balance = "agreement"
  ))
  store$set_artifact("report_md", "# Snapshot report")
  store$set_artifact("ignored", "not selected")

  snapshot <- tempest:::tempest_source_store_snapshot(
    store,
    artifacts = c("report_md", "missing")
  )

  expect_equal(snapshot$schema_version, 2L)
  expect_type(snapshot$resources, "list")
  expect_type(snapshot$sources, "list")
  expect_type(snapshot$claims, "list")
  expect_type(snapshot$evidence_spans, "list")
  expect_type(snapshot$disputes, "list")
  expect_equal(names(snapshot$artifacts), "report_md")

  restored <- tempest:::tempest_source_store_restore(snapshot)

  expect_r6_class(restored, "SourceStore")
  expect_equal(length(restored$list_sources()), 1)
  expect_equal(
    S7::S7_inherits(
      restored$retrieved_resources[[1]],
      tempest:::TempestResource
    ),
    FALSE
  )
  expect_equal(
    restored$get_claim(claim_id)@claim_text,
    "Snapshots preserve evidence."
  )
  expect_equal(restored$claims_for_source(source$id)[[1]]@claim_id, claim_id)
  expect_equal(
    restored$get_evidence_for_claim(claim_id)[[1]]@quote,
    "snapshot evidence"
  )
  expect_equal(restored$list_disputes()[[1]]@claim_ids, claim_id)
  expect_equal(restored$get_artifact("report_md"), "# Snapshot report")
  expect_null(restored$get_artifact("ignored"))
})

test_that("SourceStore restore rejects claims with unknown source ids", {
  snapshot <- list(
    sources = list(),
    claims = list(tempest_claim_to_list(tempest_claim(
      claim_text = "missing source",
      source_ids = "Smissing"
    )))
  )

  expect_error(
    tempest:::tempest_source_store_restore(snapshot),
    class = "tempest_source_store_restore_error"
  )
})

test_that("SourceStore restore flags malformed sources and bad schemas", {
  expect_error(
    tempest:::tempest_source_store_restore(list(
      sources = list(list(title = "No url here"))
    )),
    class = "tempest_source_store_restore_error"
  )
  expect_error(
    tempest:::tempest_source_store_restore(list(
      schema_version = 2L,
      resources = list(list(resource_id = "broken"))
    )),
    class = "tempest_source_store_restore_error"
  )
  expect_error(
    tempest:::tempest_source_store_restore(list(
      schema_version = 3L,
      resources = list()
    )),
    class = "tempest_source_store_restore_error"
  )
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
  store$upsert_source(source)
  claim_id <- store$add_claim(tempest_claim(
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
  session <- tempest_session(
    "Session persistence",
    config = cfg,
    experts = list(expert),
    retriever = tempest_retriever(config = cfg, store = store),
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
  report_spec <- tempest:::tempest_costorm_report_spec(session)
  session$artifact_catalog$register(report_spec)
  session$artifact_catalog$add(tempest_artifact(
    report_spec,
    content = "# Restored report",
    artifact_id = "report_md",
    status = "valid"
  ))
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
  restored <- tempest:::tempest_session_restore(
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
  expect_equal(
    restored$artifact_catalog$get("report_md")@content,
    "# Restored report"
  )
  expect_equal(restored$artifacts[["suggested_questions"]], c("Q1", "Q2"))
  expect_equal(
    restored$store$get_claim(claim_id)@claim_text,
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
  session <- tempest_session(
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
  restored <- tempest:::tempest_session_restore(
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
  legacy_snapshot$research_manifest <- NULL
  legacy_snapshot$store <- list(
    schema_version = 2L,
    base_snapshot_id = snapshot$workspace$base_snapshot_id,
    max_sources = snapshot$workspace$max_sources,
    accepted_graft_references = snapshot$workspace$accepted_graft_references,
    resources = snapshot$workspace$resources,
    sources = snapshot$workspace$sources,
    claims = snapshot$workspace$proposed_claims,
    evidence_spans = snapshot$workspace$evidence_spans,
    disputes = snapshot$workspace$disputes,
    artifacts = list()
  )
  legacy_snapshot$workspace <- NULL
  legacy_snapshot$artifacts <- list(
    progress_events = snapshot$progress_events
  )
  legacy_snapshot$progress_events <- NULL
  legacy_restored <- tempest:::tempest_session_restore(
    legacy_snapshot,
    config = cfg
  )
  expect_identical(tempest_execution_events(legacy_restored), history)
  expect_null(legacy_restored$artifacts[["progress_events"]])
})

test_that("schema 5 session restore protects research identity", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace(base_snapshot_id = "snapshot-a")
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
    retriever = tempest_retriever(config = cfg, store = workspace),
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
  store$upsert_source(source)
  claim_id <- store$add_claim(tempest_claim(
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
    retriever = tempest_retriever(config = cfg, store = store)
  )
  session_id <- session$session_id
  session$add_turn("User", "user", "Save this session.")
  report_spec <- tempest:::tempest_costorm_report_spec(session)
  session$artifact_catalog$register(report_spec)
  session$artifact_catalog$add(tempest_artifact(
    report_spec,
    content = "# Bundle report",
    artifact_id = "report_md",
    status = "valid"
  ))
  package_spec <- tempest_deliverable_spec(
    "response-package",
    title = "Response package",
    purpose = "Record actions and supporting material",
    instructions = "Preserve validation and evidence lineage.",
    generator_id = "generator.package",
    validator_ids = "validator.package",
    renderer_ids = c("renderer.json", "renderer.external"),
    media_types = c("application/json", "application/octet-stream")
  )
  package_validation <- tempest_validation_result(
    "validator.package",
    status = "failed",
    message = "Owner approval is missing."
  )
  session$artifact_catalog$register(package_spec)
  session$artifact_catalog$add(tempest_artifact(
    package_spec,
    content = list(actions = list(list(owner = "Team"))),
    artifact_id = "action-register",
    artifact_kind = "action-register",
    media_type = "application/json",
    resource_ids = source$id,
    claim_ids = claim_id,
    validation_results = list(package_validation),
    status = "invalid"
  ))
  session$artifact_catalog$add(tempest_artifact(
    package_spec,
    storage_ref = "host://objects/evidence-appendix",
    artifact_id = "evidence-appendix",
    artifact_kind = "appendix",
    media_type = "application/octet-stream",
    parent_artifact_ids = "action-register",
    status = "valid"
  ))
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
  config_summary <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "config.json")
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
      "config.json",
      "experts.json",
      "skills.json",
      "connection_refs.json",
      "connection_permissions.json",
      "capability_grants.json",
      "progress_events.json",
      "workspace/sources.json",
      "workspace/proposed_claims.json",
      "workspace/citation_audit.json",
      "artifacts/typed/deliverables.json",
      "artifacts/typed/index.json",
      "artifacts/suggested_questions.json"
    )
  )
  expect_false("artifacts/report_body.md" %in% manifest$files)
  expect_false("artifacts/mindmap.md" %in% manifest$files)
  expect_null(config_summary$chat_fn)

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
  expect_equal(
    restored$artifact_catalog$get("report_md")@content,
    "# Bundle report"
  )
  expect_equal(
    restored$artifact_catalog$get("action-register")@content$actions[[1]]$owner,
    "Team"
  )
  expect_equal(
    restored$artifact_catalog$get("action-register")@status,
    "invalid"
  )
  expect_equal(
    restored$artifact_catalog$get(
      "action-register"
    )@validation_results[[1]]@status,
    "failed"
  )
  expect_equal(
    restored$artifact_catalog$get("action-register")@claim_ids,
    claim_id
  )
  expect_null(restored$artifact_catalog$get("evidence-appendix")@content)
  expect_equal(
    restored$artifact_catalog$get("evidence-appendix")@storage_ref,
    "host://objects/evidence-appendix"
  )
  expect_equal(
    restored$artifacts[["suggested_questions"]],
    c("What next?", "And then?")
  )
  expect_equal(
    restored$store$get_claim(claim_id)@claim_text,
    "Bundles preserve claims."
  )
  expect_identical(restored$workspace, restored$store)
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

test_that("schema 4 session bundles translate into artifact-free workspaces", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  workspace <- tempest_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.com/legacy-session",
    title = "Legacy session source"
  )
  workspace$upsert_source(source)
  claim_id <- workspace$add_claim(tempest_claim(
    claim_text = "Legacy bundles preserve reviewed evidence.",
    source_ids = source$id,
    verification_status = "supported",
    support_score = 0.9
  ))
  workspace$set_citation_audit(tibble::tibble(
    claim_id = claim_id,
    claim_text = "Legacy bundles preserve reviewed evidence.",
    verification_status = "supported",
    support_score = 0.9,
    rationale = "Legacy citation audit"
  ))
  session <- tempest_session(
    "Legacy session bundle",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.legacy-session",
      name = "Legacy Session Expert",
      title = "Persistence analyst",
      description = "Checks the bounded session translator.",
      instructions = "Preserve established Co-STORM product state."
    )),
    retriever = tempest_retriever(config = cfg, store = workspace),
    session_id = "legacy-session"
  )
  session$artifacts[["suggested_questions"]] <- "What changed?"
  report_spec <- tempest:::tempest_costorm_report_spec(session)
  session$artifact_catalog$register(report_spec)
  session$artifact_catalog$add(tempest_artifact(
    report_spec,
    content = "# Legacy report",
    artifact_id = "report_md",
    status = "valid"
  ))
  bundle_dir <- file.path(withr::local_tempdir(), "legacy-bundle")
  tempest_session_save(session, bundle_dir)

  path_map <- c(
    "workspace/resources.json" = "store/resources.json",
    "workspace/sources.json" = "store/sources.json",
    "workspace/proposed_claims.json" = "store/claims.json",
    "workspace/evidence_spans.json" = "store/evidence_spans.json",
    "workspace/disputes.json" = "store/disputes.json",
    "workspace/citation_audit.json" = "artifacts/citation_audit.json"
  )
  dir.create(file.path(bundle_dir, "store"), showWarnings = FALSE)
  for (source_path in names(path_map)) {
    expect_equal(
      file.rename(
        file.path(bundle_dir, source_path),
        file.path(bundle_dir, path_map[[source_path]])
      ),
      TRUE
    )
  }
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  files <- as.character(unlist(manifest$files, use.names = FALSE))
  files <- vapply(
    files,
    function(path) {
      if (path %in% names(path_map)) {
        unname(path_map[[path]])
      } else {
        path
      }
    },
    character(1)
  )
  manifest$schema_version <- 4L
  manifest$status <- "complete"
  manifest$bundle_type <- NULL
  manifest$bundle_status <- NULL
  manifest$research_manifest <- NULL
  manifest$workspace <- NULL
  manifest$files <- sort(unname(files))
  manifest$checksums <- stats::setNames(
    lapply(manifest$files, function(path) {
      tempest:::tempest_session_bundle_checksum(bundle_dir, path)
    }),
    manifest$files
  )
  tempest:::tempest_write_json(manifest_path, manifest)

  restored <- tempest_session_resume(bundle_dir, config = cfg)

  expect_identical(restored$session_id, "legacy-session")
  expect_identical(restored$manifest@research_run_id, "legacy-session")
  expect_identical(restored$manifest@mode, "costorm")
  expect_identical(restored$manifest@status, "running")
  expect_identical(restored$workspace, restored$store)
  expect_equal("artifacts" %in% names(restored$workspace), FALSE)
  expect_equal(nrow(restored$workspace$citation_audit), 1L)
  expect_identical(
    restored$artifacts[["suggested_questions"]],
    "What changed?"
  )
  expect_identical(
    restored$artifact_catalog$get("report_md")@content,
    "# Legacy report"
  )
})

test_that("session bundles use host artifact codecs deterministically", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  reverse_codec <- tempest_artifact_codec(
    "host.text.reverse",
    version = "7",
    media_types = "application/x-reverse-text",
    extension = "rev",
    priority = 200,
    encode = function(content) rev(charToRaw(enc2utf8(content))),
    decode = function(bytes) rawToChar(rev(bytes)),
    supports = function(content) {
      is.character(content) && length(content) == 1L
    }
  )
  codec_registry <- tempest_artifact_codec_registry(list(reverse_codec))
  spec <- tempest_deliverable_spec(
    "host-codec-output",
    title = "Host codec output",
    purpose = "Persist host-defined typed content",
    instructions = "Preserve the exact body.",
    generator_id = "host.generate",
    renderer_ids = "host.render",
    media_types = "application/x-reverse-text"
  )
  artifact <- tempest_artifact(
    spec,
    content = "Deterministic body",
    artifact_id = "host-codec-artifact",
    media_type = "application/x-reverse-text",
    metadata = list(
      codec = list(
        codec_id = "host.text.reverse",
        codec_version = "7"
      )
    )
  )
  session <- tempest_session(
    "Host codec persistence",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.host-codec",
      name = "Host Codec Expert",
      title = "Artifact specialist",
      description = "Produces host-defined artifact formats.",
      instructions = "Preserve deterministic artifact bytes."
    ))
  )
  session$artifact_catalog$register(spec)
  session$artifact_catalog$add(artifact)
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  tempest_session_save(
    session,
    bundle_dir,
    codec_registry = codec_registry
  )
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  content_path <- manifest$artifact_files[[1]]
  bytes <- readBin(
    file.path(bundle_dir, content_path),
    what = "raw",
    n = file.info(file.path(bundle_dir, content_path))$size
  )
  expect_equal(bytes, rev(charToRaw("Deterministic body")))

  restored <- tempest_session_resume(
    bundle_dir,
    config = cfg,
    codec_registry = codec_registry
  )

  expect_equal(
    restored$artifact_catalog$get("host-codec-artifact")@content,
    "Deterministic body"
  )
  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_artifact_codec_error"
  )
  writeBin(
    charToRaw("tampered typed content"),
    file.path(
      bundle_dir,
      content_path
    )
  )
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE,
      codec_registry = codec_registry
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("session bundles persist contracts without leaking runtime bindings", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  skill <- tempest_skill(
    "skill.customer-context",
    purpose = "Interpret customer context",
    instructions = "Apply the customer's terminology and constraints."
  )
  connection <- tempest_connection_ref(
    "connection.customer-records",
    provider_id = "host.connections",
    connection_type = "customer-records",
    title = "Customer records",
    description = "Host-owned customer context."
  )
  archive_connection <- tempest_connection_ref(
    "connection.customer-archive",
    provider_id = "host.connections",
    connection_type = "customer-records",
    title = "Customer archive",
    description = "A separate host-owned customer archive."
  )
  secret <- "runtime-secret-must-not-be-persisted"
  runtime <- tempest_runtime(
    skill_specs = list(skill),
    connection_refs = list(connection, archive_connection),
    connection_bindings = list(
      "connection.customer-records" = list(api_key = secret),
      "connection.customer-archive" = list(api_key = secret)
    )
  )
  expert <- tempest_expert(
    expert_id = "expert.customer-context",
    name = "Customer Context Expert",
    title = "Customer context analyst",
    description = "Interprets customer objectives and constraints.",
    instructions = "Use the selected customer-context procedure.",
    skill_ids = skill@skill_id
  )
  permissions <- list(
    "expert.customer-context" = "connection.customer-records"
  )
  session <- tempest_session(
    "Customer objective",
    config = cfg,
    runtime = runtime,
    experts = list(expert),
    connection_permissions = permissions
  )
  expert_session <- session$expert_session_manager$get_or_create(
    expert@expert_id
  )
  historical_grants <- list(
    moderator = list(
      "customer.records.read" = list(
        status = "granted",
        decision_id = "grant-original-runtime"
      )
    )
  )
  session$capability_grants <- historical_grants
  snapshot <- tempest_session_snapshot(session)
  expect_error(
    tempest_session_restore(
      snapshot,
      config = cfg,
      runtime = runtime,
      connection_permissions = list(
        "expert.unsaved" = "connection.customer-records"
      )
    ),
    class = "tempest_session_restore_error"
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  tempest_session_save(session, bundle_dir)

  skill_records <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "skills.json")
  )
  connection_records <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "connection_refs.json")
  )
  expert_records <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "experts.json")
  )
  bundle_files <- list.files(
    bundle_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  bundle_text <- paste(
    vapply(
      bundle_files,
      \(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )

  expect_equal(skill_records[[1]]$skill_id, skill@skill_id)
  expect_match(skill_records[[1]]$fingerprint, "^[a-f0-9]{64}$")
  customer_record <- connection_records[[
    match(
      connection@connection_id,
      vapply(connection_records, \(record) record$connection_id, character(1))
    )
  ]]
  expect_equal(customer_record$connection_id, connection@connection_id)
  expect_match(customer_record$fingerprint, "^[a-f0-9]{64}$")
  expect_equal(expert_records[[1]]$expert_id, expert@expert_id)
  expect_match(expert_records[[1]]$fingerprint, "^[a-f0-9]{64}$")
  expect_no_match(bundle_text, secret, fixed = TRUE)
  expect_no_match(bundle_text, "api_key", fixed = TRUE)

  replacement_runtime <- tempest_runtime(
    skill_specs = list(skill),
    connection_refs = list(connection, archive_connection),
    connection_bindings = list(
      "connection.customer-records" = list(
        api_key = "replacement-runtime-secret"
      ),
      "connection.customer-archive" = list(
        api_key = "replacement-archive-secret"
      )
    )
  )
  restored <- tempest_session_resume(
    bundle_dir,
    config = cfg,
    runtime = replacement_runtime
  )

  expect_identical(restored$runtime, replacement_runtime)
  expect_false(identical(restored$runtime, session$runtime))
  expect_equal(restored$connection_permissions, permissions)
  expect_equal(restored$capability_grants, historical_grants)
  expect_equal(
    restored$expert_session_manager$list_sessions(),
    expert_session$session_id
  )
  restored_binding <- restored$expert_session_manager$session_profile(
    expert_session$session_id
  )
  expect_equal(restored_binding$expert_id, expert@expert_id)
  expect_equal(
    restored_binding$expert_fingerprint,
    tempest:::tempest_expert_profile_fingerprint(expert)
  )

  changed_connection <- tempest_connection_ref(
    "connection.customer-records",
    provider_id = "host.connections",
    connection_type = "customer-records",
    title = "Customer records",
    description = "A changed connection reference."
  )
  changed_connection_runtime <- tempest_runtime(
    skill_specs = list(skill),
    connection_refs = list(changed_connection, archive_connection),
    connection_bindings = list(
      "connection.customer-records" = list(
        api_key = "replacement-runtime-secret"
      ),
      "connection.customer-archive" = list(
        api_key = "replacement-archive-secret"
      )
    )
  )
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      runtime = changed_connection_runtime
    ),
    class = "tempest_session_restore_error"
  )

  restored_without_connection <- tempest_session_resume(
    bundle_dir,
    config = cfg,
    runtime = replacement_runtime,
    connection_permissions = list(
      "expert.customer-context" = character()
    )
  )
  expect_length(
    restored_without_connection$connection_permissions[[
      "expert.customer-context"
    ]],
    0L
  )
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      runtime = replacement_runtime,
      connection_permissions = list(
        "expert.customer-context" = "connection.customer-archive"
      )
    ),
    class = "tempest_session_restore_error"
  )
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      runtime = replacement_runtime,
      connection_permissions = list(
        "expert.unsaved" = "connection.customer-records"
      )
    ),
    class = "tempest_session_restore_error"
  )
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
  session <- tempest_session(
    "Integrity check",
    config = cfg,
    runtime = runtime,
    experts = list(expert)
  )
  session$expert_session_manager$get_or_create(expert@expert_id)
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)

  changed_skill <- tempest_skill(
    "skill.tamper-check",
    purpose = "Test contract integrity",
    instructions = "A changed procedure must not silently replace the saved one."
  )
  changed_runtime <- tempest_runtime(skill_specs = list(changed_skill))
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      runtime = changed_runtime
    ),
    class = "tempest_session_restore_error"
  )

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
    tempest_session_resume(bundle_dir, config = cfg, runtime = runtime),
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
    tempest_session_resume(bundle_dir, config = cfg, runtime = runtime),
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
    "skills.json",
    "connection_refs.json",
    "connection_permissions.json",
    "capability_grants.json",
    "expert_sessions.json",
    "workspace/resources.json",
    "workspace/sources.json",
    "workspace/proposed_claims.json",
    "artifacts/typed/index.json"
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
  unlink(file.path(bundle_dir, "capability_grants.json"))
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["connection_permissions.json"]] <- NULL
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

test_that("session resume ignores files that its manifest does not declare", {
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

  restored <- tempest_session_resume(bundle_dir, config = cfg)

  expect_length(restored$artifacts[["suggested_questions"]], 0L)
})

test_that("schema 4 run bundles restore workspace, state, and manifest", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Lithium Batteries")
  workspace <- tempest_research_workspace(base_snapshot_id = "snapshot-a")
  source <- tempest:::tempest_source(
    "https://example.com/source",
    title = "Example Source",
    snippet = "Snippet"
  )
  workspace$upsert_source(source)
  workspace$add_claim(tempest:::tempest_claim(
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
      sections = list(list(title = "Overview", summary = "Summary"))
    ),
    outline = list(
      title = "Lithium Batteries",
      sections = list(list(title = "Overview", summary = "Summary"))
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
    claim_id = workspace$list_claims()[[1]]@claim_id,
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
    programs = list(extract_claims = list(program_id = "sha256:program")),
    knowledge_snapshot = list(snapshot_id = "snapshot-a"),
    status = "succeeded"
  )
  artifact_catalog <- tempest_artifact_catalog()
  report_spec <- tempest:::tempest_storm_report_spec(
    "Lithium Batteries",
    cfg
  )
  artifact_catalog$register(report_spec)
  artifact_catalog$add(tempest_artifact(
    report_spec,
    content = "Polished body",
    artifact_id = "report_md",
    run_id = "lithium-run",
    status = "valid"
  ))
  tempest:::tempest_save_run_artifacts(
    run_dir,
    workspace,
    state,
    research_manifest,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions",
    parallel_writing = TRUE,
    remove_duplicate = TRUE,
    artifact_catalog = artifact_catalog
  )

  restored_catalog <- tempest_artifact_catalog()
  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    config = cfg,
    run_id = "lithium-run",
    artifact_catalog = restored_catalog
  )

  expect_equal(
    loaded$completed_stages,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_equal(length(loaded$workspace$list_sources()), 1)
  expect_equal(length(loaded$workspace$list_claims()), 1)
  expect_equal(loaded$state$title, "Lithium Batteries")
  expect_equal(loaded$state$outline$title, "Lithium Batteries")
  expect_equal(loaded$state$draft_md, "Draft body")
  expect_equal(loaded$state$report_md, "Polished body")
  expect_equal(loaded$state$experts[[1]]@expert_id, "expert.technical")
  expect_s7_class(loaded$state$experts[[1]], TempestExpertProfile)
  expect_equal(restored_catalog$get("report_md")@content, "Polished body")
  expect_equal(restored_catalog$get("report_md")@run_id, "lithium-run")
  expect_s3_class(loaded$workspace$citation_audit, "tbl_df")
  expect_equal(nrow(loaded$workspace$citation_audit), 1)
  expect_s7_class(loaded$research_manifest, TempestResearchManifest)
  expect_identical(loaded$research_manifest@research_run_id, "lithium-run")
  expect_identical(
    loaded$research_manifest@programs,
    research_manifest@programs
  )
  expect_identical(loaded$workspace$base_snapshot_id, "snapshot-a")
  expect_equal("artifacts" %in% names(loaded$workspace), FALSE)
  expect_equal(loaded$metadata$parallel_writing, TRUE)
  expect_equal(loaded$metadata$remove_duplicate, TRUE)
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
    perspectives = list(list(name = "Overview")),
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

test_that("run restore ignores undeclared legacy artifact files", {
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

  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "undeclared-run"
  )

  expect_null(loaded$state$report_md)
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

test_that("tempest_infer_completed_stages reads stages from artifact files", {
  dir <- withr::local_tempdir()
  paths <- tempest:::tempest_run_artifact_paths(dir)

  expect_length(tempest:::tempest_infer_completed_stages(paths), 0L)

  file.create(paths$perspectives, paths$experts)
  expect_setequal(
    tempest:::tempest_infer_completed_stages(paths),
    "perspectives"
  )

  file.create(paths$outline)
  expect_setequal(
    tempest:::tempest_infer_completed_stages(paths),
    c("perspectives", "outline")
  )
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
  workspace$upsert_source(s1)
  workspace$upsert_source(s2)
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

  refs <- tempest:::tempest_read_json(file.path(dir, "references.json"))
  expect_setequal(vapply(refs, function(r) r$id, character(1)), s1$id)

  loaded <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "references-run"
  )
  expect_length(loaded$state$references, 1L)
})

test_that("schema 3 run bundles translate into fixed product state", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  state <- tempest:::tempest_storm_state(
    "Legacy run",
    perspectives = list(list(name = "Overview")),
    experts = list(tempest_expert(
      expert_id = "expert.legacy",
      name = "Legacy Expert",
      title = "Researcher",
      description = "Legacy STORM bundle translation.",
      instructions = "Preserve the known product state."
    )),
    completed_stages = "perspectives"
  )
  tempest:::tempest_save_run_artifacts(
    dir,
    tempest_research_workspace(),
    state,
    tempest_research_manifest("new-envelope", config = cfg),
    config = cfg,
    steps = "perspectives",
    research_strategy = "key_questions"
  )
  path <- file.path(dir, "run_config.json")
  legacy <- tempest:::tempest_read_json_strict(path)
  legacy$schema_version <- 3L
  legacy$status <- "complete"
  legacy$bundle_type <- NULL
  legacy$bundle_status <- NULL
  legacy$research_manifest <- NULL
  legacy$workspace <- NULL
  tempest:::tempest_write_json(path, legacy)

  supplied <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "legacy-supplied-id"
  )
  derived <- tempest:::tempest_load_run_artifacts(dir, config = cfg)
  legacy$requested_steps <- c("perspectives", "research")
  tempest:::tempest_write_json(path, legacy)
  incomplete <- tempest:::tempest_load_run_artifacts(
    dir,
    config = cfg,
    run_id = "legacy-incomplete"
  )

  expect_identical(supplied$metadata$schema_version, 3L)
  expect_identical(
    supplied$research_manifest@research_run_id,
    "legacy-supplied-id"
  )
  expect_identical(
    derived$research_manifest@research_run_id,
    basename(dir)
  )
  expect_identical(supplied$research_manifest@status, "running")
  expect_identical(incomplete$research_manifest@status, "running")
  expect_length(supplied$research_manifest@programs, 0L)
  expect_identical(
    supplied$research_manifest@config_digest,
    tempest:::tempest_research_config_digest(cfg)
  )
  expect_identical(supplied$state$completed_stages, "perspectives")
  expect_identical(supplied$state$experts[[1]]@expert_id, "expert.legacy")
  expect_equal("artifacts" %in% names(supplied$workspace), FALSE)
})

test_that("schema 4 resume protects run, config, and snapshot identity", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace(base_snapshot_id = "snapshot-a")
  manifest <- tempest_research_manifest(
    "protected-run",
    config = cfg,
    programs = list(extract_claims = list(program_id = "program-a")),
    knowledge_snapshot = list(snapshot_id = "snapshot-a"),
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
    base_snapshot_id = "snapshot-complete",
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
  workspace$upsert_source(source)
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
      config = cfg,
      knowledge_snapshot = list(snapshot_id = "snapshot-complete")
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

test_that("persistence schema dispatch rejects fractional versions", {
  cfg <- tempest_config()
  session_class <- "tempest_session_restore_error"
  run_class <- "tempest_run_restore_error"

  expect_error(
    tempest_session_restore(list(schema_version = 5.5)),
    class = session_class
  )
  expect_error(
    tempest:::tempest_session_snapshot_translate_v4(
      list(schema_version = 4.5),
      cfg
    ),
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

test_that("schema 4 manifests require files implied by completed stages", {
  make_bundle <- function() {
    dir <- tempfile("tempest-stage-files-")
    dir.create(dir)
    cfg <- tempest_config()
    state <- tempest:::tempest_storm_state(
      "Stage files",
      perspectives = list(list(name = "Overview")),
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
  remove_perspectives <- function(dir, schema_version) {
    manifest_path <- file.path(dir, "run_config.json")
    manifest <- tempest:::tempest_read_json_strict(manifest_path)
    manifest$schema_version <- schema_version
    manifest$files <- setdiff(
      unlist(manifest$files, use.names = FALSE),
      "perspectives.json"
    )
    manifest$checksums[["perspectives.json"]] <- NULL
    unlink(file.path(dir, "perspectives.json"))
    if (identical(schema_version, 3L)) {
      manifest$status <- "complete"
      manifest$bundle_type <- NULL
      manifest$bundle_status <- NULL
      manifest$research_manifest <- NULL
      manifest$workspace <- NULL
    }
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

  current_dir <- remove_perspectives(make_bundle(), 4L)
  expect_error(
    tempest:::tempest_load_run_artifacts(
      current_dir,
      config = tempest_config(),
      run_id = "stage-files"
    ),
    class = "tempest_run_restore_error"
  )

  legacy_dir <- remove_perspectives(make_bundle(), 3L)
  restored <- tempest:::tempest_load_run_artifacts(
    legacy_dir,
    config = tempest_config(),
    run_id = "legacy-stage-files"
  )
  expect_identical(restored$metadata$schema_version, 3L)
  expect_length(restored$state$completed_stages, 0L)
  expect_length(restored$state$perspectives, 0L)
})

test_that("STORM resume accepts only an equivalent supplied workspace", {
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  workspace <- tempest_research_workspace(
    base_snapshot_id = "snapshot-authoritative",
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
  workspace$upsert_source(source)
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
    config = cfg,
    knowledge_snapshot = list(snapshot_id = "snapshot-authoritative")
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
    base_snapshot_id = "snapshot-authoritative",
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

  compatibility_store <- quiet_source_store(
    base_snapshot_id = "snapshot-authoritative",
    max_sources = 2L
  )
  loaded_compatibility <- tempest:::tempest_load_run_artifacts(
    dir,
    workspace = compatibility_store,
    config = cfg,
    run_id = "authoritative-workspace"
  )
  expect_identical(loaded_compatibility$workspace, compatibility_store)
  expect_length(ls(compatibility_store$artifacts, all.names = TRUE), 0L)
  expect_identical(
    tempest:::tempest_storm_workspace_equivalence_record(
      compatibility_store
    ),
    persisted_record
  )

  fresh_workspace <- function() {
    tempest:::tempest_research_workspace_restore(snapshot)
  }
  extra_source <- fresh_workspace()
  extra_source$upsert_source(tempest:::tempest_source(
    "https://example.com/extra",
    title = "Extra source"
  ))
  changed_source <- fresh_workspace()
  source_record <- changed_source$get_source(source$id)
  source_record$title <- "Changed source"
  changed_source$upsert_source(source_record)
  changed_source$set_citation_audit(workspace$citation_audit)
  changed_source_metadata <- fresh_workspace()
  source_record <- changed_source_metadata$get_source(source$id)
  source_record$meta <- list(revision_id = "changed")
  changed_source_metadata$upsert_source(source_record)
  changed_source_metadata$set_citation_audit(workspace$citation_audit)
  changed_source_timestamp <- fresh_workspace()
  source_record <- changed_source_timestamp$get_source(source$id)
  source_record$fetched_at <- "2026-08-15T15:00:00Z"
  changed_source_timestamp$upsert_source(source_record)
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
  legacy_artifacts <- quiet_source_store(
    base_snapshot_id = "snapshot-authoritative",
    max_sources = 8L
  )
  legacy_artifacts$set_artifact("unverified", list(value = "extra"))

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
    changed_audit,
    legacy_artifacts
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
