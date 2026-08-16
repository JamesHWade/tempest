test_that("deliverable lifecycle resolves, validates, renders, and exports", {
  calls <- character()
  registry <- tempest_operation_registry(list(
    generate = list(
      version = "2",
      kind = "generator",
      implementation = function(context) {
        calls <<- c(calls, "generate")
        context$content
      }
    ),
    validate = list(
      version = "3",
      kind = "validator",
      implementation = function(content) {
        calls <<- c(calls, "validate")
        tempest_validation_result(
          "validate",
          message = paste("Validated", content)
        )
      }
    ),
    render = list(
      version = "4",
      kind = "renderer",
      implementation = function(content) {
        calls <<- c(calls, "render")
        paste0("# ", content)
      },
      metadata = list(media_type = "text/markdown")
    ),
    export = list(
      version = "5",
      kind = "exporter",
      implementation = function(artifact) {
        calls <<- c(calls, "export")
        artifact@metadata <- list(exported = TRUE)
        artifact
      }
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer the request",
    instructions = "Be concise.",
    version = "2026.1",
    generator_id = "generate",
    validator_ids = "validate",
    renderer_ids = "render",
    exporter_ids = "export",
    operation_versions = c(
      generate = "2",
      validate = "3",
      render = "4",
      export = "5"
    )
  )
  catalog <- tempest_artifact_catalog()

  result <- tempest_generate_deliverable(
    spec,
    context = list(content = "Done"),
    registry = registry,
    catalog = catalog,
    provenance = list(
      artifact_id = "artifact-1",
      run_id = "run-1",
      resource_ids = "resource-1"
    )
  )

  expect_equal(calls, c("generate", "validate", "render", "export"))
  expect_s3_class(result, "tempest_deliverable_result")
  expect_equal(result$canonical_content, "Done")
  expect_equal(result$artifacts[[1]]@content, "# Done")
  expect_equal(result$artifacts[[1]]@status, "valid")
  expect_equal(result$artifacts[[1]]@run_id, "run-1")
  expect_equal(result$artifacts[[1]]@resource_ids, "resource-1")
  expect_equal(result$artifacts[[1]]@metadata$exported, TRUE)
  expect_identical(catalog$get("artifact-1"), result$artifacts[[1]])
  expect_equal(
    result$artifacts[[1]]@spec_fingerprint,
    tempest:::tempest_deliverable_fingerprint(spec)
  )
  expect_equal(
    unname(vapply(
      result$resolved_operations,
      function(operation) operation$version,
      character(1)
    )),
    c("2", "3", "4", "5")
  )
})

test_that("exporters cannot rewrite finalized artifact records", {
  mutate_artifact <- \(artifact) artifact
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Original"
    ),
    validate = list(
      kind = "validator",
      implementation = function() {
        tempest_validation_result(
          "validate",
          message = "The artifact is ready."
        )
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    export = list(
      kind = "exporter",
      implementation = function(artifact) mutate_artifact(artifact)
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer the request",
    instructions = "Be concise.",
    generator_id = "generate",
    validator_ids = "validate",
    renderer_ids = "render",
    exporter_ids = "export"
  )
  mutations <- list(
    content = function(artifact) {
      artifact@content <- "Rewritten"
      artifact
    },
    checksum = function(artifact) {
      artifact@checksum <- "forged-checksum"
      artifact
    },
    validation_results = function(artifact) {
      artifact@validation_results <- list(tempest_validation_result(
        "replacement-validator",
        message = "Replacement validation"
      ))
      artifact
    },
    run_id = function(artifact) {
      artifact@run_id <- "foreign-run"
      artifact
    }
  )

  for (field in names(mutations)) {
    mutate_artifact <- mutations[[field]]
    expect_error(
      tempest_generate_deliverable(
        spec,
        registry = registry,
        provenance = list(run_id = "run-1", step_id = "write")
      ),
      regexp = field,
      class = "tempest_deliverable_execution_error"
    )
  }
})

test_that("all operations resolve before generation begins", {
  generated <- FALSE
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() {
        generated <<- TRUE
        "Done"
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer",
    instructions = "Answer.",
    generator_id = "generate",
    validator_ids = "missing",
    renderer_ids = "render"
  )

  expect_error(
    tempest_generate_deliverable(spec, registry = registry),
    class = "tempest_operation_registry_error"
  )
  expect_identical(generated, FALSE)
})

test_that("failed validation keeps an inspectable invalid artifact", {
  export_calls <- 0L
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Incomplete"
    ),
    validate = list(
      kind = "validator",
      implementation = function() {
        tempest_validation_result(
          "validate",
          status = "failed",
          message = "Required section is missing."
        )
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    export = list(
      kind = "exporter",
      implementation = function(artifact) {
        export_calls <<- export_calls + 1L
        artifact
      }
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer",
    instructions = "Answer.",
    generator_id = "generate",
    validator_ids = "validate",
    renderer_ids = "render",
    exporter_ids = "export"
  )

  result <- tempest_generate_deliverable(spec, registry = registry)

  expect_equal(result$artifacts[[1]]@status, "invalid")
  expect_equal(
    result$artifacts[[1]]@validation_results[[1]]@status,
    "failed"
  )
  expect_identical(
    result$catalog$has(result$artifacts[[1]]@artifact_id),
    TRUE
  )
  expect_identical(export_calls, 0L)
})

test_that("approval requirements enter awaiting approval", {
  export_calls <- 0L
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Draft"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    ),
    export = list(
      kind = "exporter",
      implementation = function(artifact) {
        export_calls <<- export_calls + 1L
        artifact
      }
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer",
    instructions = "Answer.",
    generator_id = "generate",
    renderer_ids = "render",
    exporter_ids = "export",
    requires_approval = TRUE
  )

  result <- tempest_generate_deliverable(spec, registry = registry)

  expect_equal(result$artifacts[[1]]@status, "awaiting_approval")
  expect_identical(export_calls, 0L)
})

test_that("built-in Markdown operations honor the output contract", {
  generated_prompt <- NULL
  objective <- tempest_objective(
    "Respond to the customer",
    constraints = "Do not speculate",
    acceptance_criteria = "Include next steps"
  )
  spec <- tempest_deliverable_spec(
    "customer-response",
    title = "Customer response",
    purpose = "Resolve the request",
    instructions = "Use plain language.",
    required_fields = "Next steps",
    generator_id = "tempest.generator.markdown_report",
    validator_ids = "tempest.validator.required_fields",
    renderer_ids = "tempest.renderer.markdown_report",
    operation_versions = c(
      "tempest.generator.markdown_report" = "1",
      "tempest.validator.required_fields" = "1",
      "tempest.renderer.markdown_report" = "1"
    )
  )

  result <- tempest_generate_deliverable(
    spec,
    context = list(
      objective = objective,
      include_references = FALSE,
      workspace = tempest_research_workspace()
    ),
    runtime = list(
      generate_text = function(prompt) {
        generated_prompt <<- prompt
        "## Next steps\n\nSend the response."
      }
    )
  )

  expect_match(generated_prompt, "Resolve the request", fixed = TRUE)
  expect_match(generated_prompt, "Do not speculate", fixed = TRUE)
  expect_match(generated_prompt, "Next steps", fixed = TRUE)
  expect_equal(
    result$artifacts[[1]]@content,
    "# Customer response\n\n## Next steps\n\nSend the response.\n"
  )
  expect_equal(result$artifacts[[1]]@status, "valid")
})

test_that("built-in Markdown reports render a ResearchWorkspace", {
  workspace <- tempest_research_workspace()
  source <- fake_source("https://example.org/report-workspace")
  workspace$upsert_retrieved_resource(source)
  spec <- tempest_deliverable_spec(
    "workspace-report",
    title = "Workspace report",
    purpose = "Render provisional evidence",
    instructions = "Include source references.",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown_report"
  )

  representation <- tempest:::tempest_builtin_markdown_report_renderer(
    content = paste0("Provisional result [", source$id, "]."),
    deliverable = spec,
    context = list(workspace = workspace)
  )

  expect_match(representation$content, "## References", fixed = TRUE)
  expect_match(representation$content, source$url, fixed = TRUE)
})

test_that("no-reference reports retain canonical policy validation", {
  workspace <- tempest_research_workspace()
  source <- fake_source("https://example.org/no-reference-policy")
  workspace$upsert_retrieved_resource(source)
  claim <- tempest_claim(
    claim_text = "The supported finding is exact.",
    source_ids = source$id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  spec <- tempest_deliverable_spec(
    "strict-no-reference-report",
    title = "Strict no-reference report",
    purpose = "Render verified evidence",
    instructions = "Use verified evidence only.",
    evidence_policy = "strict",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown_report"
  )
  context <- list(
    workspace = workspace,
    include_references = FALSE,
    citation_policy = "strict",
    min_support_score = 0.7
  )
  valid <- tempest:::tempest_builtin_markdown_report_renderer(
    content = paste0(
      "The supported finding is exact [",
      source$id,
      "]."
    ),
    deliverable = spec,
    context = context
  )

  expect_match(valid$content, "# Strict no\\-reference report", fixed = TRUE)
  expect_match(valid$content, paste0("[", source$id, "]"), fixed = TRUE)
  expect_no_match(valid$content, "## References", fixed = TRUE)
  expect_no_error(tempest:::tempest_final_report_validate(
    valid$content,
    workspace,
    title = "Strict no-reference report",
    citation_policy = "strict",
    on_unsupported_claim = "flag",
    min_support_score = 0.7
  ))
  expect_error(
    tempest:::tempest_builtin_markdown_report_renderer(
      content = paste0("A false finding [", source$id, "]."),
      deliverable = spec,
      context = context
    ),
    class = "tempest_deliverable_execution_error"
  )
})

test_that("STORM report prompts preserve the existing polish contract", {
  prompt <- tempest:::tempest_storm_report_prompt(
    "# Draft",
    remove_duplicate = TRUE
  )

  expect_match(
    prompt,
    "Polish the following Markdown report.",
    fixed = TRUE
  )
  expect_match(prompt, "<draft>\n# Draft\n</draft>", fixed = TRUE)
  expect_match(prompt, "duplicate", ignore.case = TRUE)
})

test_that("Co-STORM report prompts include only threshold-verified evidence", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    citation_policy = "claim_verified",
    min_support_score = 0.95,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Verified report evidence",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.verified-report",
      name = "Verified Report Expert"
    ))
  )
  below_threshold <- test_add_verifiable_claim(
    session$workspace,
    key = "below",
    claim_text = "Below-threshold evidence must not enter the report prompt.",
    quote = "Below-threshold report evidence."
  )
  verified <- test_add_verifiable_claim(
    session$workspace,
    key = "verified",
    claim_text = "Threshold-verified evidence enters the report prompt.",
    quote = "Threshold-verified report evidence."
  )
  tempest_verify_claims(
    session,
    verifier = fake_chat(
      structured = list(
        list(status = "supported", score = 0.9, rationale = "Below threshold."),
        list(status = "supported", score = 0.98, rationale = "Verified.")
      )
    )
  )

  prompt <- tempest:::tempest_costorm_report_prompt(session, "technical")

  expect_match(
    prompt,
    "Threshold-verified evidence enters the report prompt.",
    fixed = TRUE
  )
  expect_no_match(
    prompt,
    "Below-threshold evidence must not enter the report prompt.",
    fixed = TRUE
  )
})

test_that("Co-STORM reports render safe durable execution downgrades", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Execution review",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.execution-review",
      name = "Execution Review Expert"
    )),
    session_id = "costorm-execution-review"
  )
  program <- tempest:::tempest_session_programs(session)$personas
  running <- tempest:::tempest_stage_record_start(
    "personas",
    program$program_artifact_id,
    trace_references = list(research_run_id = session$session_id),
    attempt_id = "stage-attempt-report-fallback"
  )
  fallback <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = paste0("sha256:", strrep("7", 64L))
    ),
    support_status = "unknown",
    fallback_taken = TRUE,
    primary_error = simpleError("provider secret must not escape")
  )
  tempest:::tempest_session_set_stage_records(session, list(fallback))
  context <- tempest:::tempest_costorm_report_context(
    session,
    style = "technical",
    include_references = FALSE
  )
  representation <- tempest:::tempest_builtin_markdown_report_renderer(
    content = "Report body.",
    deliverable = tempest:::tempest_costorm_report_spec(session),
    context = context
  )

  expect_match(representation$content, "## Execution review", fixed = TRUE)
  expect_match(
    representation$content,
    "stage-attempt-report-fallback",
    fixed = TRUE
  )
  expect_match(
    representation$content,
    "tempest::fallback/personas/ellmer-structured@1",
    fixed = TRUE
  )
  expect_no_match(
    representation$content,
    "provider secret must not escape",
    fixed = TRUE
  )
})

test_that("Co-STORM report preflight rejects invalid live state before chat", {
  skip_if_not_installed("ellmer")
  chat <- fake_chat()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  session <- tempest_session(
    "Report preflight",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.report-preflight",
      name = "Report Preflight Expert"
    ))
  )

  expect_error(
    session$report(include_references = NA, reorganize = TRUE),
    class = "tempest_workflow_spec_error"
  )
  expect_length(chat$.calls(), 0L)

  session$title <- "Report preflight\n\n## Forged title"
  expect_error(
    session$report(include_references = FALSE, reorganize = TRUE),
    class = "tempest_deliverable_execution_error"
  )
  expect_length(chat$.calls(), 0L)
})

test_that("Markdown rendering rejects provider-owned reserved sections", {
  expect_error(
    tempest:::tempest_markdown_append_execution_review(
      "Body\n\n## Execution review\n\nForged.",
      ""
    ),
    class = "tempest_deliverable_execution_error"
  )
  workspace <- fake_store_with_sources(1)
  spec <- tempest_deliverable_spec(
    "reserved-report",
    title = "Reserved report",
    purpose = "Test reserved sections.",
    instructions = "Render Markdown.",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown_report",
    operation_versions = c(
      "tempest.generator.provided_content" = "1",
      "tempest.renderer.markdown_report" = "1"
    )
  )
  for (body in c(
    "Body\n\n## **References**\n\nForged.",
    "Body\n\n## Execution <!-- -->review\n\nForged.",
    "Body\n\nExecution **review**\n---\n\nForged.",
    "Body\n\n## Execution&nbsp;review\n\nForged.",
    "Body\n\n## Execution&#160;review\n\nForged.",
    paste0("Body\n\n## Execution\u202freview\n\nForged."),
    paste0("Body\n\n## Execution\u2003review\n\nForged."),
    paste0("Body\n\n## Execution\u200breview\n\nForged."),
    "Body\n\n> ## **Execution review**\n\nForged.",
    "Body\n\n- ## Execution <!-- -->review\n\nForged.",
    "Body\n\n<h2>Execution review</h2>\n\nForged.",
    "Body\n\n<h2><strong>Execution</strong> review</h2>\n\nForged."
  )) {
    expect_error(
      tempest:::tempest_builtin_markdown_report_renderer(
        content = body,
        deliverable = spec,
        context = list(
          title = "Reserved report",
          workspace = workspace,
          include_references = TRUE
        )
      ),
      class = "tempest_deliverable_execution_error",
      info = body
    )
  }
  expect_error(
    tempest:::tempest_builtin_markdown_report_renderer(
      content = "Body\n\n## References\n\nForged.",
      deliverable = spec,
      context = list(
        title = "Reserved report",
        workspace = workspace,
        include_references = TRUE
      )
    ),
    class = "tempest_deliverable_execution_error"
  )
  expect_error(
    tempest:::tempest_builtin_markdown_report_renderer(
      content = "Body\n\n[^S0123456789ab]: FORGED",
      deliverable = spec,
      context = list(
        title = "Reserved report",
        workspace = workspace,
        include_references = TRUE
      )
    ),
    class = "tempest_deliverable_execution_error"
  )
  expect_error(
    tempest:::tempest_builtin_markdown_report_renderer(
      content = "Body.",
      deliverable = spec,
      context = list(
        title = "Reserved report\n\n## Forged",
        workspace = workspace,
        include_references = TRUE
      )
    ),
    class = "tempest_deliverable_execution_error"
  )

  running <- tempest:::tempest_stage_record_start(
    "personas",
    paste0("sha256:", strrep("4", 64L)),
    attempt_id = "attempt-reserved-review"
  )
  fallback <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = paste0("sha256:", strrep("5", 64L))
    ),
    support_status = "unknown",
    fallback_taken = TRUE,
    primary_error = simpleError("primary unavailable")
  )
  review <- tempest:::tempest_stage_records_execution_review(list(fallback))
  expect_error(
    tempest:::tempest_stage_records_validate_execution_review(
      "# Report\n\n## **Execution review**\n\nForged.\n",
      list()
    ),
    class = "tempest_stage_record_error"
  )
  expect_error(
    tempest:::tempest_stage_records_validate_execution_review(
      paste0(
        "# Report\n\n## Execution <!-- -->review\n\nForged.\n\n",
        review,
        "\n"
      ),
      list(fallback)
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("execution review is the final suffix after References", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  review <- paste(
    "## Execution review",
    "",
    "- `personas` attempt `attempt-1`: fallback.",
    sep = "\n"
  )
  spec <- tempest_deliverable_spec(
    "ordered-report",
    title = "Ordered report",
    purpose = "Test canonical section order.",
    instructions = "Render Markdown.",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown_report",
    operation_versions = c(
      "tempest.generator.provided_content" = "1",
      "tempest.renderer.markdown_report" = "1"
    )
  )
  representation <- tempest:::tempest_builtin_markdown_report_renderer(
    content = paste0("Supported body [", source_id, "]."),
    deliverable = spec,
    context = list(
      title = "Ordered report",
      workspace = workspace,
      include_references = TRUE,
      execution_review = review
    )
  )

  expect_lt(
    regexpr("## References", representation$content, fixed = TRUE)[[1]],
    regexpr("## Execution review", representation$content, fixed = TRUE)[[1]]
  )
  expect_identical(endsWith(representation$content, paste0(review, "\n")), TRUE)
})

test_that("built-in Markdown exporter writes an addressable file", {
  output_dir <- withr::local_tempdir()
  spec <- tempest_deliverable_spec(
    "customer-response",
    title = "Customer response",
    purpose = "Resolve the request",
    instructions = "Use plain language.",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown",
    exporter_ids = "tempest.exporter.markdown",
    filename_policy = list(filename = "response.md")
  )

  result <- tempest_generate_deliverable(
    spec,
    context = list(content = "# Response"),
    runtime = list(output_dir = output_dir)
  )

  path <- file.path(output_dir, "response.md")
  expect_true(file.exists(path))
  expect_equal(tempest:::tempest_read_text(path), "# Response")
  expect_equal(result$artifacts[[1]]@storage_ref, fs::path_abs(path))
  expect_equal(result$artifacts[[1]]@metadata$filename, "response.md")
})

test_that("built-in Markdown exporter defers unsafe artifacts", {
  output_dir <- withr::local_tempdir()
  approval_spec <- tempest_deliverable_spec(
    "approval-response",
    title = "Approval response",
    purpose = "Resolve the request",
    instructions = "Use plain language.",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown",
    exporter_ids = "tempest.exporter.markdown",
    filename_policy = list(filename = "approval.md"),
    requires_approval = TRUE
  )

  approval_result <- tempest_generate_deliverable(
    approval_spec,
    context = list(content = "# Response"),
    runtime = list(output_dir = output_dir)
  )

  expect_equal(
    approval_result$artifacts[[1]]@status,
    "awaiting_approval"
  )
  expect_identical(
    file.exists(file.path(output_dir, "approval.md")),
    FALSE
  )

  invalid_spec <- tempest_deliverable_spec(
    "invalid-response",
    title = "Invalid response",
    purpose = "Resolve the request",
    instructions = "Include a decision.",
    required_fields = "Decision",
    generator_id = "tempest.generator.provided_content",
    validator_ids = "tempest.validator.required_fields",
    renderer_ids = "tempest.renderer.markdown",
    exporter_ids = "tempest.exporter.markdown",
    filename_policy = list(filename = "invalid.md")
  )

  invalid_result <- tempest_generate_deliverable(
    invalid_spec,
    context = list(content = "# Response"),
    runtime = list(output_dir = output_dir)
  )

  expect_equal(invalid_result$artifacts[[1]]@status, "invalid")
  expect_identical(
    file.exists(file.path(output_dir, "invalid.md")),
    FALSE
  )
})

test_that("explicit retries safely replace invalid stable artifacts", {
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function(context) context$content
    ),
    validate = list(
      kind = "validator",
      implementation = function(content) {
        passed <- identical(content, "good")
        tempest_validation_result(
          "validate",
          status = if (passed) "passed" else "failed",
          message = if (passed) "Ready." else "Needs revision.",
          details = list(missing = if (passed) character() else "decision")
        )
      }
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) content
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer",
    instructions = "Answer.",
    generator_id = "generate",
    validator_ids = "validate",
    renderer_ids = "render"
  )
  catalog <- tempest_artifact_catalog()
  provenance <- list(
    artifact_id = "stable-answer",
    run_id = "run-1",
    step_id = "write"
  )

  first <- tempest_generate_deliverable(
    spec,
    context = list(content = "bad"),
    registry = registry,
    catalog = catalog,
    provenance = provenance
  )

  expect_equal(first$artifacts[[1]]@status, "invalid")
  expect_error(
    tempest_generate_deliverable(
      spec,
      context = list(content = "good"),
      registry = registry,
      catalog = catalog,
      provenance = provenance
    ),
    class = "tempest_deliverable_execution_error"
  )
  expect_error(
    tempest_generate_deliverable(
      spec,
      context = list(content = "good"),
      registry = registry,
      catalog = catalog,
      provenance = utils::modifyList(
        provenance,
        list(
          step_id = "other-step",
          replace_invalid_artifacts = TRUE
        )
      )
    ),
    class = "tempest_deliverable_execution_error"
  )
  expect_equal(catalog$get("stable-answer")@status, "invalid")

  retried <- tempest_generate_deliverable(
    spec,
    context = list(content = "good"),
    registry = registry,
    catalog = catalog,
    provenance = utils::modifyList(
      provenance,
      list(replace_invalid_artifacts = TRUE)
    )
  )

  artifact <- retried$artifacts[[1]]
  history <- artifact@metadata$tempest_prior_attempts
  expect_equal(artifact@status, "valid")
  expect_identical(catalog$get("stable-answer"), artifact)
  expect_length(history, 1L)
  expect_equal(history[[1]]$status, "invalid")
  expect_equal(
    history[[1]]$validation_results[[1]]$status,
    "failed"
  )
  expect_equal(
    history[[1]]$validation_results[[1]]$details$missing,
    "decision"
  )
})

test_that("multi-representation renderers use stable atomic artifact ids", {
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Done"
    ),
    render = list(
      kind = "renderer",
      implementation = function(content) {
        list(
          tempest_artifact_representation(
            content = content,
            artifact_kind = "primary"
          ),
          tempest_artifact_representation(
            content = paste(content, "Appendix"),
            artifact_kind = "appendix"
          )
        )
      }
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer",
    instructions = "Answer.",
    generator_id = "generate",
    renderer_ids = "render"
  )
  provenance <- list(run_id = "run-1", step_id = "write")

  first <- tempest_generate_deliverable(
    spec,
    registry = registry,
    provenance = provenance
  )
  second <- tempest_generate_deliverable(
    spec,
    registry = registry,
    provenance = provenance
  )
  first_ids <- vapply(
    first$artifacts,
    \(artifact) artifact@artifact_id,
    character(1)
  )
  second_ids <- vapply(
    second$artifacts,
    \(artifact) artifact@artifact_id,
    character(1)
  )

  expect_identical(first_ids, second_ids)
  expect_length(unique(first_ids), 2L)

  existing <- tempest_artifact(
    spec,
    content = "Existing",
    artifact_id = "bundle--2",
    producer_operation_id = "render",
    run_id = "other-run",
    step_id = "write",
    status = "valid"
  )
  catalog <- tempest_artifact_catalog(
    artifacts = list(existing),
    deliverables = list(spec)
  )

  expect_error(
    tempest_generate_deliverable(
      spec,
      registry = registry,
      catalog = catalog,
      provenance = list(
        artifact_id = "bundle",
        run_id = "run-1",
        step_id = "write"
      )
    ),
    class = "tempest_deliverable_execution_error"
  )
  expect_identical(catalog$has("bundle"), FALSE)
  expect_named(catalog$list(), "bundle--2")

  explicit <- tempest_generate_deliverable(
    spec,
    registry = registry,
    provenance = list(artifact_id = "bundle")
  )
  expect_equal(
    vapply(
      explicit$artifacts,
      \(artifact) artifact@artifact_id,
      character(1)
    ),
    c("bundle", "bundle--2")
  )
})

test_that("renderer and exporter failures are classed", {
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Done"
    ),
    render = list(
      kind = "renderer",
      implementation = function() stop("broken renderer")
    )
  ))
  spec <- tempest_deliverable_spec(
    "answer",
    title = "Answer",
    purpose = "Answer",
    instructions = "Answer.",
    generator_id = "generate",
    renderer_ids = "render"
  )

  expect_error(
    tempest_generate_deliverable(spec, registry = registry),
    class = "tempest_deliverable_execution_error"
  )
})
