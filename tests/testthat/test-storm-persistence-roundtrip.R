test_that("schema 8 STORM bundles restore workspace, state, and manifest", {
  skip_if_not_installed("jsonlite")
  test_env <- environment()
  root <- withr::local_tempdir(pattern = "tempest-runs-")

  run_dir <- tempest:::tempest_storm_prepare_run_dir(root, "Lithium Batteries")
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  workspace <- tempest_research_workspace()
  source <- tempest_resource(
    resource_kind = "web",
    locator = "https://example.com/source",
    title = "Example Source",
    media_type = "text/html",
    content = "Lithium batteries store energy.",
    metadata = list(snippet = "Snippet")
  )
  workspace$upsert_retrieved_resource(source)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    source_id = source@resource_id,
    quote = "Lithium batteries store energy.",
    evidence_span_id = "span.lithium-run",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest:::tempest_claim(
    claim_text = "Lithium batteries store energy.",
    source_ids = source@resource_id,
    evidence_span_ids = span_id,
    supporting_quotes = list("Lithium batteries store energy."),
    confidence = "high"
  ))
  report_md <- tempest:::tempest_report_md_render(
    title = "Lithium Batteries",
    body = paste0(
      "Lithium batteries store energy. [",
      source@resource_id,
      "]"
    ),
    workspace = workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  expert <- tempest_expert(
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
      tempest:::tempest_section_markdown_heading(),
      "\n\n",
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
      source_id = source@resource_id,
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
  bound <- test_persistence_bind_storm_records(
    state,
    workspace,
    research_manifest
  )
  state <- bound$state
  research_manifest <- bound$manifest
  for (stage in c(
    "perspectives",
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
  tempest:::tempest_storm_save_artifacts(
    run_dir,
    workspace,
    state,
    research_manifest,
    program_set = program_set,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish")
  )
  pristine_dir <- run_dir
  clone_bundle <- function() {
    root <- withr::local_tempdir(
      pattern = "tempest-runs-clone-",
      .local_envir = test_env
    )
    clone <- file.path(root, "bundle")
    fs::dir_copy(pristine_dir, clone)
    clone
  }

  loaded <- tempest:::tempest_storm_load_artifacts(
    pristine_dir,
    config = cfg,
    program_set = program_set,
    run_id = "lithium-run"
  )
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(loaded$workspace),
    "sealed"
  )
  expect_error(
    loaded$workspace$upsert_retrieved_resource(fake_source(
      url = "https://example.com/injected-after-storm-restore"
    )),
    class = "tempest_research_workspace_error"
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
  expect_length(loaded$state$stage_records, 8L)
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
  expect_equal(loaded$state$experts[[1]]@expert_id, expert@expert_id)
  expect_s7_class(loaded$state$experts[[1]], TempestExpertProfile)
  expect_false("artifact_catalog" %in% names(loaded))
  expect_s3_class(
    tempest:::tempest_claim_supports_resolved(loaded$workspace),
    "tbl_df"
  )
  expect_equal(
    nrow(tempest:::tempest_claim_supports_resolved(loaded$workspace)),
    1
  )
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
  expect_equal(loaded$metadata$schema_version, 8L)
  expect_identical(loaded$metadata$bundle_type, "storm")
  expect_identical(loaded$metadata$bundle_status, "complete")
  expect_type(loaded$metadata$research_manifest, "list")
  expect_equal(
    file.exists(file.path(pristine_dir, "research_manifest.json")),
    FALSE
  )
  expect_equal(file.exists(file.path(pristine_dir, "experts.json")), TRUE)
  expect_equal(
    file.exists(file.path(pristine_dir, "stage_records.json")),
    TRUE
  )
  expect_equal(file.exists(file.path(pristine_dir, "personas.json")), FALSE)

  run_dir <- clone_bundle()
  sidecar_path <- file.path(run_dir, "stage_records.json")
  manifest_path <- file.path(run_dir, "run_config.json")
  double_schema <- readLines(manifest_path, warn = FALSE)
  double_schema <- sub(
    '"schema_version": 8,',
    '"schema_version": 8.0,',
    double_schema,
    fixed = TRUE
  )
  tempest:::tempest_atomic_write_lines(double_schema, manifest_path)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )

  run_dir <- clone_bundle()
  sidecar_path <- file.path(run_dir, "stage_records.json")
  manifest_path <- file.path(run_dir, "run_config.json")
  tempest:::tempest_product_write_json(sidecar_path, list())
  erased_manifest <- tempest:::tempest_product_read_json(manifest_path)
  erased_manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      run_dir,
      "stage_records.json"
    )
  tempest:::tempest_product_write_json(manifest_path, erased_manifest)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )

  run_dir <- clone_bundle()
  manifest_path <- file.path(run_dir, "run_config.json")
  changed_manifest_trace <- tempest:::tempest_product_read_json(manifest_path)
  changed_manifest_trace$research_manifest$traces[[1]]$status <- "failed"
  tempest:::tempest_product_write_json(manifest_path, changed_manifest_trace)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )

  run_dir <- clone_bundle()
  sidecar_path <- file.path(run_dir, "stage_records.json")
  manifest_path <- file.path(run_dir, "run_config.json")
  records <- tempest:::tempest_product_read_json(sidecar_path)
  records[[1]]$program_artifact_id <- paste0("sha256:", strrep("0", 64L))
  tempest:::tempest_product_write_json(sidecar_path, records)
  persisted_manifest <- tempest:::tempest_product_read_json(manifest_path)
  persisted_manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      run_dir,
      "stage_records.json"
    )
  tempest:::tempest_product_write_json(manifest_path, persisted_manifest)
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      run_dir,
      config = cfg,
      program_set = program_set,
      run_id = "lithium-run"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("STORM restore rejects tampered expert-profile records", {
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
  bound <- test_persistence_bind_storm_records(state, workspace, manifest)
  state <- bound$state
  manifest <- bound$manifest
  tempest:::tempest_storm_save_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "perspectives"
  )
  experts_path <- file.path(run_dir, "experts.json")
  records <- tempest:::tempest_product_read_json(experts_path)
  for (field in tempest:::tempest_expert_record_fields()) {
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
  double_schema[[1]]$schema_version <- 2.0
  expect_error(
    tempest:::tempest_experts_from_records(
      double_schema,
      what = "STORM test expert profiles",
      class = c("tempest_run_restore_error", "tempest_error")
    ),
    class = "tempest_run_restore_error",
    regexp = "non-null writer fields"
  )
  extra_field <- records
  extra_field[[1]]$state <- "active"
  expect_error(
    tempest:::tempest_experts_from_records(
      extra_field,
      what = "STORM test expert profiles",
      class = c("tempest_run_restore_error", "tempest_error")
    ),
    class = "tempest_run_restore_error"
  )
  records[[1]]$fingerprint <- strrep("0", 64)
  tempest:::tempest_product_write_json(experts_path, records)
  manifest_path <- file.path(run_dir, "run_config.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      run_dir,
      "experts.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)

  expect_error(
    tempest:::tempest_storm_load_artifacts(
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

  run_dir <- tempest:::tempest_storm_prepare_run_dir(root, "Partial Run")
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
  bound <- test_persistence_bind_storm_records(state, workspace, manifest)
  state <- bound$state
  manifest <- bound$manifest

  tempest:::tempest_storm_save_artifacts(
    run_dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = c("perspectives", "research", "outline", "write", "polish")
  )

  loaded <- tempest:::tempest_storm_load_artifacts(
    run_dir,
    config = cfg,
    program_set = program_set,
    run_id = "partial-run"
  )
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(loaded$workspace),
    "open"
  )

  expect_equal(
    tempest:::tempest_storm_stage_complete(
      loaded$completed_stages,
      "perspectives"
    ),
    TRUE
  )
  expect_equal(
    tempest:::tempest_storm_stage_complete(
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
