test_that("typed artifact bundles round-trip arbitrary representations", {
  spec <- tempest_deliverable_spec(
    "response-package",
    title = "Response package",
    purpose = "Answer and enumerate actions",
    instructions = "Use evidence.",
    generator_id = "generator",
    renderer_ids = c("markdown", "json"),
    media_types = c("text/markdown", "application/json")
  )
  markdown <- tempest_artifact(
    spec,
    content = "# Response",
    artifact_id = "response-md",
    media_type = "text/markdown",
    status = "valid"
  )
  actions <- tempest_artifact(
    spec,
    content = list(actions = list(list(owner = "Team", due = "Monday"))),
    artifact_id = "response-actions",
    artifact_kind = "action-register",
    media_type = "application/json",
    parent_artifact_ids = "response-md",
    status = "valid"
  )
  external <- tempest_artifact(
    spec,
    storage_ref = "host://objects/appendix",
    artifact_id = "response-appendix",
    artifact_kind = "appendix",
    media_type = "application/octet-stream",
    parent_artifact_ids = "response-md",
    status = "valid"
  )
  catalog <- tempest_artifact_catalog(deliverables = list(spec))
  catalog$add_many(list(markdown, actions, external))
  bundle_dir <- withr::local_tempdir()

  written <- tempest:::tempest_artifact_bundle_write(catalog, bundle_dir)
  restored <- tempest:::tempest_artifact_bundle_read(
    bundle_dir,
    declared_files = written$files
  )

  expect_contains(
    written$files,
    c(
      "artifacts/typed/deliverables.json",
      "artifacts/typed/index.json"
    )
  )
  expect_length(written$content_files, 2L)
  expect_equal(restored$get("response-md")@content, "# Response")
  expect_equal(
    restored$get("response-actions")@content$actions[[1]]$owner,
    "Team"
  )
  expect_null(restored$get("response-appendix")@content)
  expect_equal(
    restored$get("response-appendix")@storage_ref,
    "host://objects/appendix"
  )
})

test_that("typed artifact bundles reject tampering and unknown codecs", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator",
    renderer_ids = "renderer"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Brief",
    artifact_id = "brief"
  )
  catalog <- tempest_artifact_catalog(deliverables = list(spec))
  catalog$add(artifact)
  bundle_dir <- withr::local_tempdir()
  written <- tempest:::tempest_artifact_bundle_write(catalog, bundle_dir)

  writeBin(
    charToRaw("tampered"),
    file.path(bundle_dir, written$content_files[[1]])
  )
  expect_error(
    tempest:::tempest_artifact_bundle_read(bundle_dir),
    class = "tempest_artifact_codec_error"
  )

  bundle_dir <- withr::local_tempdir()
  tempest:::tempest_artifact_bundle_write(catalog, bundle_dir)
  index_path <- file.path(bundle_dir, "artifacts/typed/index.json")
  index <- tempest:::tempest_read_json_strict(index_path)
  index$artifacts$brief$codec_id <- "unknown.codec"
  tempest:::tempest_write_json(index_path, index)
  expect_error(
    tempest:::tempest_artifact_bundle_read(bundle_dir),
    class = "tempest_artifact_codec_error"
  )
})

test_that("typed artifact bundles reject unsafe and undeclared paths", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator",
    renderer_ids = "renderer"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Brief",
    artifact_id = "brief"
  )
  catalog <- tempest_artifact_catalog(deliverables = list(spec))
  catalog$add(artifact)
  bundle_dir <- withr::local_tempdir()
  written <- tempest:::tempest_artifact_bundle_write(catalog, bundle_dir)
  index_path <- file.path(bundle_dir, written$index_path)
  index <- tempest:::tempest_read_json_strict(index_path)
  index$artifacts$brief$content_path <- "../outside.md"
  tempest:::tempest_write_json(index_path, index)

  expect_error(
    tempest:::tempest_artifact_bundle_read(bundle_dir),
    class = "tempest_artifact_codec_error"
  )

  bundle_dir <- withr::local_tempdir()
  written <- tempest:::tempest_artifact_bundle_write(catalog, bundle_dir)
  expect_error(
    tempest:::tempest_artifact_bundle_read(
      bundle_dir,
      declared_files = c(written$deliverables_path, written$index_path)
    ),
    class = "tempest_artifact_codec_error"
  )

  bundle_dir <- withr::local_tempdir()
  written <- tempest:::tempest_artifact_bundle_write(catalog, bundle_dir)
  index_path <- file.path(bundle_dir, written$index_path)
  index <- tempest:::tempest_read_json_strict(index_path)
  index$artifacts <- unname(index$artifacts)
  tempest:::tempest_write_json(index_path, index)
  expect_error(
    tempest:::tempest_artifact_bundle_read(bundle_dir),
    class = "tempest_artifact_codec_error"
  )
})
