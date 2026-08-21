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
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$files <- setdiff(
    unlist(manifest$files, use.names = FALSE),
    "stage_records.json"
  )
  manifest$checksums[["stage_records.json"]] <- NULL
  unlink(file.path(bundle_dir, "stage_records.json"))
  tempest:::tempest_product_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg
    ),
    class = "tempest_session_restore_error"
  )

  program_set <- tempest_program_set()
  run_dir <- file.path(withr::local_tempdir(), "run")
  dir.create(run_dir)
  tempest:::tempest_storm_save_artifacts(
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
    steps = "research"
  )
  run_manifest_path <- file.path(run_dir, "run_config.json")
  run_manifest <- tempest:::tempest_product_read_json(run_manifest_path)
  run_manifest$files <- setdiff(
    unlist(run_manifest$files, use.names = FALSE),
    "stage_records.json"
  )
  run_manifest$checksums[["stage_records.json"]] <- NULL
  unlink(file.path(run_dir, "stage_records.json"))
  tempest:::tempest_product_write_json(run_manifest_path, run_manifest)

  expect_error(
    tempest:::tempest_storm_load_artifacts(
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
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      bundle_dir,
      "stage_records.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )

  program_set <- tempest_program_set()
  run_dir <- file.path(withr::local_tempdir(), "run")
  dir.create(run_dir)
  tempest:::tempest_storm_save_artifacts(
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
    steps = "research"
  )
  run_sidecar <- file.path(run_dir, "stage_records.json")
  unlink(run_sidecar)
  expect_identical(
    file.symlink(file.path(run_dir, "references.json"), run_sidecar),
    TRUE
  )
  run_manifest_path <- file.path(run_dir, "run_config.json")
  run_manifest <- tempest:::tempest_product_read_json(run_manifest_path)
  run_manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      run_dir,
      "stage_records.json"
    )
  tempest:::tempest_product_write_json(run_manifest_path, run_manifest)

  expect_error(
    tempest:::tempest_storm_load_artifacts(
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
    tempest:::tempest_storm_save_artifacts(
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
      steps = "research"
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
    tempest:::tempest_storm_load_artifacts(
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
    tempest:::tempest_storm_load_artifacts(
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
  expert <- test_expert(name = "Stage Record Binding Expert")
  workspace <- tempest_research_workspace()
  source <- fake_source(
    url = "https://example.org/stage-record-binding",
    title = "Stage record binding",
    content_text = "Stage records bind durable claim outputs."
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-stage-record-binding",
    source_id = source@resource_id,
    quote = "Stage records bind durable claim outputs.",
    extracted_by = extraction_program$program_artifact_id
  ))
  manual_span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-manual-stage-record-binding",
    source_id = source@resource_id,
    quote = "Stage records bind durable claim outputs.",
    extracted_by = extraction_program$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_text = "Stage records bind durable claim outputs.",
    source_ids = source@resource_id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Stage records bind durable claim outputs."),
    retrieval_step_id = "retrieval.stage-record-binding",
    expert_id = expert@expert_id,
    session_id = "stage-record-binding"
  ))
  manual_claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_text = "Manual claims do not satisfy extraction proof.",
    source_ids = source@resource_id,
    evidence_span_ids = manual_span_id,
    supporting_quotes = list("Stage records bind durable claim outputs.")
  ))
  session <- tempest_session(
    "Stage-record binding",
    config = cfg,
    experts = list(expert),
    retriever = tempest_retriever(config = cfg, workspace = workspace),
    session_id = "stage-record-binding",
    program_set = program_set
  )
  expert_session <- tempest:::tempest_session_expert_manager(
    session
  )$get_or_create(
    expert@expert_id
  )
  run_context <- tempest:::tempest_deputy_run_context(
    session$manifest,
    stage = "dialogue",
    role = "expert",
    expert_id = expert@expert_id
  )
  tempest:::tempest_session_record_deputy_trace(
    session,
    list(
      agent_id = tempest:::tempest_deputy_adapter_agent_id(run_context),
      correlation_id = "retrieval.stage-record-binding",
      deputy_run_id = "trace.stage-record-binding",
      deputy_session_id = expert_session$session_id,
      expert_id = expert@expert_id,
      role = "expert",
      stage = "dialogue",
      status = "complete",
      completion_disposition = "issued",
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
      deputy_run_id = "trace.stage-record-binding",
      deputy_session_id = expert_session$session_id,
      expert_id = expert@expert_id,
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
  valid_records <- tempest:::tempest_product_read_json(sidecar_path)
  valid_manifest <- tempest:::tempest_product_read_json(manifest_path)
  write_records <- function(records) {
    tempest:::tempest_product_write_json(sidecar_path, records)
    manifest <- valid_manifest
    manifest$checksums[["stage_records.json"]] <-
      tempest:::tempest_product_bundle_checksum(
        bundle_dir,
        "stage_records.json"
      )
    tempest:::tempest_product_write_json(manifest_path, manifest)
  }
  expect_rejected <- function() {
    expect_error(
      tempest_session_resume(bundle_dir, config = cfg),
      class = "tempest_session_restore_error"
    )
  }

  changed_manifest_trace <- valid_manifest
  changed_manifest_trace$research_manifest$traces[[1]]$status <- "failed"
  tempest:::tempest_product_write_json(manifest_path, changed_manifest_trace)
  expect_rejected()

  extra_manifest_trace <- valid_manifest
  extra_trace <- extra_manifest_trace$research_manifest$traces[[1]]
  extra_trace$trace_id <- "attempt-unrecorded"
  extra_manifest_trace$research_manifest$traces <- c(
    extra_manifest_trace$research_manifest$traces,
    list(extra_trace)
  )
  tempest:::tempest_product_write_json(manifest_path, extra_manifest_trace)
  expect_rejected()

  missing_manifest_trace <- valid_manifest
  missing_manifest_trace$research_manifest$traces <-
    missing_manifest_trace$research_manifest$traces[-1]
  tempest:::tempest_product_write_json(manifest_path, missing_manifest_trace)
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
  valid_claims <- tempest:::tempest_product_read_json(claim_path)
  valid_spans <- tempest:::tempest_product_read_json(span_path)
  valid_supports <- tempest:::tempest_product_read_json(support_path)
  write_workspace <- function(rel_path, value) {
    tempest:::tempest_product_write_json(claim_path, valid_claims)
    tempest:::tempest_product_write_json(span_path, valid_spans)
    tempest:::tempest_product_write_json(support_path, valid_supports)
    tempest:::tempest_product_write_json(file.path(bundle_dir, rel_path), value)
    tempest:::tempest_product_write_json(sidecar_path, valid_records)
    manifest <- valid_manifest
    for (workspace_path in c(
      "workspace/proposed_claims.json",
      "workspace/evidence_spans.json",
      "workspace/claim_supports.json"
    )) {
      manifest$checksums[[workspace_path]] <-
        tempest:::tempest_product_bundle_checksum(bundle_dir, workspace_path)
    }
    manifest$checksums[["stage_records.json"]] <-
      tempest:::tempest_product_bundle_checksum(
        bundle_dir,
        "stage_records.json"
      )
    tempest:::tempest_product_write_json(manifest_path, manifest)
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
    source_id = source@resource_id,
    quote = source@content,
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
        source_ids = source@resource_id,
        evidence_span_ids = span_id,
        supporting_quotes = list(source@content)
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
  valid_resources <- tempest:::tempest_product_read_json(resources_path)
  valid_manifest <- tempest:::tempest_product_read_json(manifest_path)
  expect_source_tamper_rejected <- function(content) {
    resources <- valid_resources
    resources[[1]]$content <- content
    resources[[1]]$metadata$content_text <- content
    resources[[1]]$content_hash <- tempest:::tempest_product_content_hash(
      content,
      resources[[1]]$media_type
    )
    resources[[1]]$fingerprint <-
      tempest:::tempest_resource_fingerprint(resources[[1]])
    tempest:::tempest_product_write_json(resources_path, resources)
    manifest <- valid_manifest
    manifest$checksums[["workspace/retrieved_resources.json"]] <-
      tempest:::tempest_product_bundle_checksum(
        bundle_dir,
        "workspace/retrieved_resources.json"
      )
    tempest:::tempest_product_write_json(manifest_path, manifest)
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

test_that("session bundles expose only product persistence inventory", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Narrow product persistence",
    config = cfg,
    experts = list(tempest_expert(
      name = "Narrow Persistence Expert",
      title = "Artifact specialist",
      description = "Checks product persistence boundaries.",
      instructions = "Persist only the scientific report product."
    ))
  )
  snapshot <- tempest_session_snapshot(session)
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  tempest_session_save(session, bundle_dir)
  manifest <- tempest:::tempest_product_read_json(
    file.path(bundle_dir, "session.json")
  )
  restored <- tempest_session_resume(bundle_dir, config = cfg)

  expect_identical(
    any(startsWith(
      unlist(manifest$files, use.names = FALSE),
      "artifacts/typed/"
    )),
    FALSE
  )
  expect_identical(
    intersect(
      names(manifest),
      c("artifact_files", "artifact_index", "deliverable_index")
    ),
    character()
  )
  expect_identical(
    intersect(
      names(snapshot),
      c("artifact_catalog", "artifacts", "deliverables")
    ),
    character()
  )
  expect_null(tempest:::tempest_session_report_value(restored))
})
