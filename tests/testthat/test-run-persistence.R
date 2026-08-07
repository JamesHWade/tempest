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

test_that("SourceStore snapshots restore durable ledger state", {
  store <- SourceStore$new()
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
  store <- SourceStore$new()
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
  restore_snapshot$artifacts$report <- "Legacy report body"
  restore_snapshot$artifacts$report_md <- "Legacy report markdown"
  restore_snapshot$artifacts$mindmap_md <- "Legacy mind map markdown"
  collector <- tempest_progress_collector(include_payload = TRUE)
  restored <- tempest:::tempest_session_restore(
    restore_snapshot,
    config = cfg,
    progress = collector$record
  )

  expect_r6_class(restored, "TempestSession")
  expect_equal(snapshot$schema_version, 4L)
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
  legacy_snapshot$artifacts$progress_events <- snapshot$progress_events
  legacy_snapshot$progress_events <- NULL
  legacy_restored <- tempest:::tempest_session_restore(
    legacy_snapshot,
    config = cfg
  )
  expect_identical(tempest_execution_events(legacy_restored), history)
  expect_null(legacy_restored$artifacts[["progress_events"]])
})

test_that("Tempest session bundles save and resume durable state", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  store <- SourceStore$new()
  source <- tempest:::tempest_source(
    "https://example.com/session-bundle",
    title = "Session Bundle Source"
  )
  store$upsert_source(source)
  claim_id <- store$add_claim(tempest_claim(
    claim_text = "Bundles preserve claims.",
    source_ids = source$id
  ))
  store$set_artifact(
    "citation_audit",
    tibble::tibble(
      claim_id = claim_id,
      claim_text = "Bundles preserve claims.",
      verification_status = "supported",
      support_score = 0.95,
      rationale = "test"
    )
  )
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
  expect_equal(manifest$status, "complete")
  expect_equal(manifest$schema_version, 4L)
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
      "store/sources.json",
      "store/claims.json",
      "artifacts/typed/deliverables.json",
      "artifacts/typed/index.json",
      "artifacts/suggested_questions.json",
      "artifacts/citation_audit.json"
    )
  )
  expect_false("artifacts/report_body.md" %in% manifest$files)
  expect_false("artifacts/mindmap.md" %in% manifest$files)
  expect_null(config_summary$chat_fn)

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
  expect_equal(nrow(restored$store$get_artifact("citation_audit")), 1)
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
  writeLines("{", file.path(bundle_dir, "store/claims.json"))
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
  session$topic <- "Replacement bundle"
  withr::local_options(
    tempest.session_write_hook = function(file) {
      if (identical(file, "store/claims.json")) {
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
    "store/resources.json",
    "store/sources.json",
    "store/claims.json",
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
  claims_path <- file.path(bundle_dir, "store/claims.json")
  writeLines("{", claims_path)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["store/claims.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "store/claims.json"
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

test_that("run artifacts save and load store state", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Lithium Batteries")
  store <- SourceStore$new()
  source <- tempest:::tempest_source(
    "https://example.com/source",
    title = "Example Source",
    snippet = "Snippet"
  )
  store$upsert_source(source)
  store$add_claim(tempest:::tempest_claim(
    claim_text = "Lithium batteries store energy.",
    source_ids = source$id,
    confidence = "high"
  ))
  store$set_artifact("title", "Lithium Batteries")
  store$set_artifact(
    "perspectives",
    list(list(
      name = "Technical",
      description = "Technology",
      key_questions = c("How do they work?")
    ))
  )
  store$set_artifact(
    "experts",
    list(tempest_expert(
      expert_id = "expert.technical",
      name = "Dr. Tech",
      title = "Engineer",
      description = "Battery technology and manufacturing.",
      instructions = "Explain technical tradeoffs with source-backed claims."
    ))
  )
  store$set_artifact(
    "outline",
    list(
      title = "Lithium Batteries",
      sections = list(list(title = "Overview", summary = "Summary"))
    )
  )
  store$set_artifact("draft_md", "Draft body")
  store$set_artifact("report_md", "Polished body")
  store$set_artifact(
    "citation_audit",
    tibble::tibble(
      claim_id = store$list_claims()[[1]]@claim_id,
      claim_text = "Lithium batteries store energy.",
      verification_status = "supported",
      support_score = 0.9,
      rationale = "matches source"
    )
  )

  cfg <- tempest_config()
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
    store,
    topic = "Lithium Batteries",
    title = "Lithium Batteries",
    config = cfg,
    completed_stages = c(
      "perspectives",
      "research",
      "outline",
      "write",
      "polish"
    ),
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions",
    parallel_writing = TRUE,
    remove_duplicate = TRUE,
    artifact_catalog = artifact_catalog
  )

  restored <- SourceStore$new()
  restored_catalog <- tempest_artifact_catalog()
  loaded <- tempest:::tempest_load_run_artifacts(
    run_dir,
    restored,
    artifact_catalog = restored_catalog
  )

  expect_equal(
    loaded$completed_stages,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_equal(length(restored$list_sources()), 1)
  expect_equal(length(restored$list_claims()), 1)
  expect_equal(restored$get_artifact("title"), "Lithium Batteries")
  expect_equal(restored$get_artifact("outline")$title, "Lithium Batteries")
  expect_equal(restored$get_artifact("draft_md"), "Draft body")
  expect_equal(restored$get_artifact("report_md"), "Polished body")
  expect_equal(
    restored$get_artifact("experts")[[1]]@expert_id,
    "expert.technical"
  )
  expect_s7_class(
    restored$get_artifact("experts")[[1]],
    TempestExpertProfile
  )
  expect_equal(restored_catalog$get("report_md")@content, "Polished body")
  expect_equal(restored_catalog$get("report_md")@run_id, "lithium-run")
  expect_s3_class(restored$get_artifact("citation_audit"), "tbl_df")
  expect_equal(nrow(restored$get_artifact("citation_audit")), 1)
  expect_equal(loaded$metadata$parallel_writing, TRUE)
  expect_equal(loaded$metadata$remove_duplicate, TRUE)
  expect_equal(loaded$metadata$schema_version, 3L)
  expect_equal(file.exists(file.path(run_dir, "experts.json")), TRUE)
  expect_equal(file.exists(file.path(run_dir, "personas.json")), FALSE)
})

test_that("run restore rejects tampered expert-profile records", {
  skip_if_not_installed("jsonlite")
  run_dir <- withr::local_tempdir()
  store <- SourceStore$new()
  store$set_artifact(
    "experts",
    list(tempest_expert(
      expert_id = "expert.run-integrity",
      name = "Run Integrity Expert",
      title = "Persistence integrity analyst",
      description = "Checks STORM expert records.",
      instructions = "Require exact profile fingerprints."
    ))
  )
  tempest:::tempest_save_run_artifacts(
    run_dir,
    store,
    topic = "Run integrity",
    title = "Run integrity",
    config = tempest_config(),
    completed_stages = "perspectives",
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
    tempest:::tempest_load_run_artifacts(run_dir, SourceStore$new()),
    class = "tempest_run_restore_error"
  )
})

test_that("completed stage metadata controls resume state", {
  skip_if_not_installed("jsonlite")
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_prepare_run_dir(root, "Partial Run")
  store <- SourceStore$new()
  store$set_artifact("perspectives", list(list(name = "Overview")))
  store$set_artifact(
    "experts",
    list(tempest_expert(
      expert_id = "expert.partial",
      name = "Partial Expert",
      title = "Partial run expert",
      description = "A persisted expert for a partial run.",
      instructions = "Cover the saved perspective."
    ))
  )

  tempest:::tempest_save_run_artifacts(
    run_dir,
    store,
    topic = "Partial Run",
    title = "Partial Run",
    config = tempest_config(),
    completed_stages = "perspectives",
    steps = c("perspectives", "research", "outline", "write", "polish"),
    research_strategy = "key_questions"
  )

  loaded <- tempest:::tempest_load_run_artifacts(run_dir, SourceStore$new())

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
  store <- SourceStore$new()
  tempest:::tempest_save_run_artifacts(
    dir,
    store,
    topic = "t",
    title = "T",
    config = tempest_config(),
    completed_stages = character(),
    steps = "polish",
    research_strategy = "key_questions"
  )
  writeLines("UNDECLARED", file.path(dir, "storm_gen_article_polished.md"))

  restored <- SourceStore$new()
  tempest:::tempest_load_run_artifacts(dir, restored)

  expect_null(restored$get_artifact("report_md"))
})

test_that("run manifest failures are classed and reject escaping symlinks", {
  make_bundle <- function() {
    dir <- tempfile("tempest-run-")
    dir.create(dir)
    tempest:::tempest_save_run_artifacts(
      dir,
      SourceStore$new(),
      topic = "t",
      title = "T",
      config = tempest_config(),
      completed_stages = character(),
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
      SourceStore$new()
    ),
    class = "tempest_run_restore_error"
  )

  skip_on_os("windows")
  symlink_dir <- make_bundle()
  source_path <- file.path(symlink_dir, "sources.json")
  outside_path <- tempfile("outside-sources-", tmpdir = dirname(symlink_dir))
  expect_true(file.copy(source_path, outside_path))
  unlink(source_path)
  expect_true(file.symlink(outside_path, source_path))
  expect_error(
    tempest:::tempest_load_run_artifacts(symlink_dir, SourceStore$new()),
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
  store <- SourceStore$new()
  store$set_artifact("draft_md", "# Draft")

  tempest:::tempest_save_run_artifacts(
    dir,
    store,
    topic = "t",
    title = "T",
    config = tempest_config(),
    completed_stages = c("research", "write"),
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
  store <- SourceStore$new()
  s1 <- tempest:::tempest_source(url = "https://example.com/a", title = "A")
  s2 <- tempest:::tempest_source(url = "https://example.com/b", title = "B")
  store$upsert_source(s1)
  store$upsert_source(s2)
  store$set_artifact("report_md", paste0("A cited claim [", s1$id, "]."))

  tempest:::tempest_save_run_artifacts(
    dir,
    store,
    topic = "t",
    title = "T",
    config = tempest_config(),
    completed_stages = "polish",
    steps = "polish",
    research_strategy = "key_questions"
  )

  refs <- tempest:::tempest_read_json(file.path(dir, "references.json"))
  expect_setequal(vapply(refs, function(r) r$id, character(1)), s1$id)

  store2 <- SourceStore$new()
  tempest:::tempest_load_run_artifacts(dir, store2)
  expect_length(store2$get_artifact("references"), 1L)
})
