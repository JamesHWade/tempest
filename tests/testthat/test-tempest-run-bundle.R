test_that("TempestRun bundles round-trip durable run state", {
  make_run <- function() {
    objective <- tempest_objective(
      "Produce a customer brief",
      objective_id = "objective-bundle",
      deliverable_ids = "brief",
      created_at = "2026-07-18 UTC"
    )
    deliverable <- tempest_deliverable_spec(
      "brief",
      title = "Customer brief",
      purpose = "Summarize the outcome",
      instructions = "Return structured content.",
      generator_id = "generator.brief",
      renderer_ids = "renderer.json",
      media_types = "application/json"
    )
    resource <- tempest_resource(
      resource_kind = "host.document",
      locator = "documents/request-1",
      title = "Customer request",
      media_type = "text/plain",
      resource_id = "resource.request",
      content = "The customer needs a rollout plan.",
      retrieved_at = "2026-07-18T00:00:00Z"
    )
    store <- tempest_research_workspace()
    store$upsert_retrieved_resource(resource)
    artifact <- tempest_artifact(
      deliverable,
      content = list(
        outcome = "Roll out in two phases.",
        milestones = list("pilot", "launch")
      ),
      artifact_id = "brief-output",
      media_type = "application/json",
      run_id = "run-bundle",
      step_id = "publish",
      resource_ids = resource@resource_id,
      status = "valid",
      created_at = "2026-07-18 UTC"
    )
    catalog <- tempest_artifact_catalog(
      artifacts = list(artifact),
      deliverables = list(deliverable)
    )
    runtime <- tempest_operation_registry(list(
      "generator.brief" = list(
        kind = "generator",
        implementation = function() "unused"
      ),
      "renderer.json" = list(
        kind = "renderer",
        implementation = function(content) content
      ),
      publish = list(
        kind = "step",
        implementation = function() "published"
      )
    ))
    workflow <- tempest_workflow_spec(
      "bundle-workflow",
      title = "Bundle workflow",
      purpose = "Exercise durable bundles",
      steps = list(tempest_workflow_step(
        "publish",
        title = "Publish",
        purpose = "Publish the brief",
        operation_id = "publish",
        produced_artifact_ids = "brief-output",
        approval_checkpoint = TRUE
      ))
    )
    run <- tempest_run_workflow(
      objective,
      workflow,
      runtime,
      deliverables = list(deliverable),
      artifact_catalog = catalog,
      source_store = store,
      run_id = "run-bundle"
    )
    approval_id <- names(run$approvals)[[1]]
    run$record_approval(
      approval_id,
      "approved",
      note = "Approved for publication.",
      metadata = list(reviewer = "host")
    )
    list(run = run, runtime = runtime, approval_id = approval_id)
  }

  fixture <- make_run()
  root <- withr::local_tempdir()
  bundle <- file.path(root, "run")
  saved <- tempest_run_save(fixture$run, bundle)

  expect_equal(saved, normalizePath(bundle, winslash = "/"))
  expect_setequal(
    list.files(bundle),
    c("manifest.json", "snapshot.json")
  )
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle, "manifest.json")
  )
  expect_equal(manifest$bundle_type, "tempest_run")
  expect_equal(manifest$status, "complete")
  expect_equal(unlist(manifest$files), "snapshot.json")
  expect_match(
    manifest$checksums[["snapshot.json"]],
    "^[a-f0-9]{64}$"
  )
  snapshot <- tempest:::tempest_read_json_strict(
    file.path(bundle, "snapshot.json")
  )
  expect_identical(snapshot$source_store$schema_version, 5L)
  expect_null(snapshot$runtime)
  expect_null(snapshot$policy_adapter)

  restored <- tempest:::tempest_run_resume(bundle, runtime = fixture$runtime)

  expect_r6_class(restored, "TempestRun")
  expect_identical(restored$runtime, fixture$runtime)
  expect_r6_class(restored$source_store, "ResearchWorkspace")
  expect_identical(inherits(restored$source_store, "SourceStore"), FALSE)
  expect_equal(
    restored$artifact("brief-output")@content,
    list(
      milestones = list("pilot", "launch"),
      outcome = "Roll out in two phases."
    )
  )
  expect_equal(
    restored$source_store$get_retrieved_resource("resource.request")@content,
    "The customer needs a rollout plan."
  )
  expect_equal(
    restored$approvals[[fixture$approval_id]]$status,
    "approved"
  )
  expect_equal(
    restored$approvals[[fixture$approval_id]]$metadata$reviewer,
    "host"
  )
  restored$resume()
  expect_equal(restored$status, "succeeded")

  expect_error(
    tempest_run_save(fixture$run, bundle),
    class = "tempest_run_save_error"
  )
  expect_no_error(tempest_run_save(
    fixture$run,
    bundle,
    overwrite = TRUE
  ))
})

test_that("TempestRun bundles reject tampering and malformed inventories", {
  make_bundle <- function(name) {
    objective <- tempest_objective(
      "Persist safely",
      objective_id = paste0("objective-", name),
      created_at = "2026-07-18 UTC"
    )
    runtime <- tempest_operation_registry(list(
      finish = list(kind = "step", implementation = function() "done")
    ))
    workflow <- tempest_workflow_spec(
      paste0("workflow-", name),
      title = "Safe workflow",
      purpose = "Exercise validation",
      steps = list(tempest_workflow_step(
        "finish",
        title = "Finish",
        purpose = "Finish",
        operation_id = "finish"
      ))
    )
    run <- tempest_run_workflow(
      objective,
      workflow,
      runtime,
      run_id = paste0("run-", name)
    )
    path <- file.path(root, name)
    tempest_run_save(run, path)
    list(path = path, runtime = runtime)
  }
  read_manifest <- function(bundle) {
    tempest:::tempest_read_json_strict(
      file.path(bundle, "manifest.json")
    )
  }
  write_manifest <- function(bundle, manifest) {
    tempest:::tempest_write_json(
      file.path(bundle, "manifest.json"),
      manifest
    )
  }

  root <- withr::local_tempdir()

  tampered <- make_bundle("tampered")
  writeLines("{}", file.path(tampered$path, "snapshot.json"))
  expect_error(
    tempest:::tempest_run_resume(tampered$path, tampered$runtime),
    class = "tempest_run_resume_error"
  )

  missing <- make_bundle("missing")
  unlink(file.path(missing$path, "snapshot.json"))
  expect_error(
    tempest:::tempest_run_resume(missing$path, missing$runtime),
    "inventory",
    class = "tempest_run_resume_error"
  )

  extra <- make_bundle("extra")
  writeLines("unexpected", file.path(extra$path, "extra.txt"))
  expect_error(
    tempest:::tempest_run_resume(extra$path, extra$runtime),
    "inventory",
    class = "tempest_run_resume_error"
  )

  duplicate <- make_bundle("duplicate")
  manifest <- read_manifest(duplicate$path)
  manifest$files <- list("snapshot.json", "snapshot.json")
  write_manifest(duplicate$path, manifest)
  expect_error(
    tempest:::tempest_run_resume(duplicate$path, duplicate$runtime),
    "duplicate",
    class = "tempest_run_resume_error"
  )

  unsafe <- make_bundle("unsafe")
  manifest <- read_manifest(unsafe$path)
  manifest$files <- list("../snapshot.json")
  write_manifest(unsafe$path, manifest)
  expect_error(
    tempest:::tempest_run_resume(unsafe$path, unsafe$runtime),
    "unsafe",
    class = "tempest_run_resume_error"
  )

  incomplete <- make_bundle("incomplete")
  manifest <- read_manifest(incomplete$path)
  manifest$status <- "writing"
  write_manifest(incomplete$path, manifest)
  expect_error(
    tempest:::tempest_run_resume(incomplete$path, incomplete$runtime),
    "not complete",
    class = "tempest_run_resume_error"
  )

  old_evidence <- make_bundle("old-evidence")
  snapshot_path <- file.path(old_evidence$path, "snapshot.json")
  snapshot <- tempest:::tempest_read_json_strict(snapshot_path)
  snapshot$source_store$schema_version <- 4L
  tempest:::tempest_write_json(snapshot_path, snapshot)
  manifest <- read_manifest(old_evidence$path)
  manifest$checksums[["snapshot.json"]] <- digest::digest(
    snapshot_path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  write_manifest(old_evidence$path, manifest)
  expect_error(
    tempest:::tempest_run_resume(old_evidence$path, old_evidence$runtime),
    "Unsupported generic run evidence format: 4",
    class = "tempest_unsupported_format_error"
  )

  malformed_manifest <- make_bundle("malformed-manifest")
  writeLines(
    "{",
    file.path(malformed_manifest$path, "manifest.json")
  )
  expect_error(
    tempest:::tempest_run_resume(
      malformed_manifest$path,
      malformed_manifest$runtime
    ),
    class = "tempest_run_resume_error"
  )

  malformed_snapshot <- make_bundle("malformed-snapshot")
  snapshot_path <- file.path(malformed_snapshot$path, "snapshot.json")
  writeLines("{", snapshot_path)
  manifest <- read_manifest(malformed_snapshot$path)
  manifest$checksums[["snapshot.json"]] <- digest::digest(
    snapshot_path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  write_manifest(malformed_snapshot$path, manifest)
  expect_error(
    tempest:::tempest_run_resume(
      malformed_snapshot$path,
      malformed_snapshot$runtime
    ),
    class = "tempest_run_resume_error"
  )
})

test_that("TempestRun bundles never serialize executable runtime values", {
  objective <- tempest_objective(
    "Reject runtime values",
    objective_id = "objective-runtime-value",
    created_at = "2026-07-18 UTC"
  )
  runtime <- tempest_operation_registry(list(
    finish = list(kind = "step", implementation = \() "done")
  ))
  workflow <- tempest_workflow_spec(
    "runtime-value-workflow",
    title = "Runtime value workflow",
    purpose = "Reject executable values",
    steps = list(tempest_workflow_step(
      "finish",
      title = "Finish",
      purpose = "Finish",
      operation_id = "finish"
    ))
  )
  run <- tempest_run_workflow(
    objective,
    workflow,
    runtime,
    source_store = test_research_workspace(),
    runtime_context = list(runtime_client = function() "live client")
  )
  bundle <- file.path(withr::local_tempdir(), "run")

  expect_no_error(tempest_run_save(run, bundle))
  snapshot <- tempest:::tempest_read_json_strict(
    file.path(bundle, "snapshot.json")
  )
  expect_null(snapshot$runtime)
  expect_null(snapshot$runtime_context)
})

test_that("TempestRun save errors do not print file or parser details", {
  io_secret <- "sk-proj-io-secret-ABCDEFGHIJKLMNOP"
  blocked_path <- file.path(withr::local_tempdir(), io_secret)
  writeLines("blocked", blocked_path)
  missing_path <- file.path(blocked_path, "snapshot.json")
  io_error <- tryCatch(
    suppressWarnings(
      tempest:::tempest_generic_run_bundle_write_json(
        missing_path,
        list(ok = TRUE),
        "Tempest run snapshot"
      )
    ),
    error = identity
  )

  expect_s3_class(io_error, "tempest_run_save_error")
  expect_identical(
    conditionMessage(io_error),
    "Could not write Tempest run snapshot."
  )
  expect_no_match(
    paste(capture.output(print(io_error)), collapse = "\n"),
    io_secret,
    fixed = TRUE
  )
  expect_null(io_error$parent)

  runtime <- tempest_operation_registry(list(
    finish = list(kind = "step", implementation = function() "done")
  ))
  workflow <- tempest_workflow_spec(
    "overwrite-parser-workflow",
    title = "Overwrite parser workflow",
    purpose = "Exercise safe overwrite errors",
    steps = list(tempest_workflow_step(
      "finish",
      title = "Finish",
      purpose = "Finish",
      operation_id = "finish"
    ))
  )
  run <- tempest_run_workflow(
    tempest_objective("Persist safely"),
    workflow,
    runtime
  )
  bundle <- file.path(withr::local_tempdir(), "run")
  tempest_run_save(run, bundle)
  parse_secret <- "Authorization: Bearer parser-secret-token"
  writeLines(
    paste0('{"secret": "', parse_secret, '"'),
    file.path(bundle, "manifest.json")
  )

  parse_error <- rlang::catch_cnd(
    tempest_run_save(run, bundle, overwrite = TRUE)
  )

  expect_s3_class(parse_error, "tempest_run_save_error")
  expect_identical(
    conditionMessage(parse_error),
    paste(
      "Refusing to overwrite a directory that is not a recognized",
      "Tempest run bundle."
    )
  )
  expect_no_match(
    paste(capture.output(print(parse_error)), collapse = "\n"),
    parse_secret,
    fixed = TRUE
  )
  expect_null(parse_error$parent)
})
