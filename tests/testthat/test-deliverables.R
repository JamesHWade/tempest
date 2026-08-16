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
      include_references = FALSE
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
    "## Next steps\n\nSend the response."
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
