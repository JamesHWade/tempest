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
})

test_that("approval requirements enter awaiting approval", {
  registry <- tempest_operation_registry(list(
    generate = list(
      kind = "generator",
      implementation = function() "Draft"
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
    renderer_ids = "render",
    requires_approval = TRUE
  )

  result <- tempest_generate_deliverable(spec, registry = registry)

  expect_equal(result$artifacts[[1]]@status, "awaiting_approval")
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
