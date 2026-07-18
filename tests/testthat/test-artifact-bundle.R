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
  index <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, written$index_path)
  )
  expect_equal(
    index$artifacts[["response-appendix"]]$codec_id,
    "tempest.external.reference"
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

test_that("custom codecs persist inline typed artifacts", {
  reverse_codec <- tempest_artifact_codec_definition(
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
  registry <- tempest_artifact_codec_registry(list(reverse_codec))
  spec <- tempest_deliverable_spec(
    "custom",
    title = "Custom",
    purpose = "Exercise a host codec",
    instructions = "Preserve the content.",
    generator_id = "generator",
    renderer_ids = "renderer",
    media_types = "application/x-reverse-text"
  )
  artifact <- tempest_artifact(
    spec,
    content = "Custom body",
    artifact_id = "custom-1",
    media_type = "application/x-reverse-text",
    metadata = list(
      codec = list(
        codec_id = "host.text.reverse",
        codec_version = "7"
      )
    )
  )
  catalog <- tempest_artifact_catalog(
    artifacts = list(artifact),
    deliverables = list(spec)
  )
  bundle_dir <- withr::local_tempdir()

  written <- tempest:::tempest_artifact_bundle_write(
    catalog,
    bundle_dir,
    codec_registry = registry
  )
  restored <- tempest:::tempest_artifact_bundle_read(
    bundle_dir,
    declared_files = written$files,
    codec_registry = registry
  )

  expect_equal(restored$get("custom-1")@content, "Custom body")
  expect_match(written$content_files[[1]], "\\.rev$")
  index <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, written$index_path)
  )
  expect_equal(
    index$artifacts[["custom-1"]]$codec_id,
    "host.text.reverse"
  )
  expect_null(index$codecs[["host.text.reverse"]]$encode)
})

test_that("custom external codecs honor preferences and deterministic selection", {
  external_codec <- tempest_artifact_codec_definition(
    "host.external.object",
    version = "3",
    media_types = "application/octet-stream",
    extension = "href",
    external = TRUE,
    priority = -2000
  )
  registry <- tempest_artifact_codec_registry(list(external_codec))
  spec <- tempest_deliverable_spec(
    "external",
    title = "External",
    purpose = "Exercise a host external codec",
    instructions = "Preserve the storage reference.",
    generator_id = "generator",
    renderer_ids = "renderer",
    media_types = "application/octet-stream"
  )
  declared <- tempest_artifact(
    spec,
    storage_ref = "host://objects/declared",
    artifact_id = "external-declared",
    metadata = list(
      codec = list(
        codec_id = "host.external.object",
        codec_version = "3"
      )
    )
  )
  catalog <- tempest_artifact_catalog(
    artifacts = list(declared),
    deliverables = list(spec)
  )
  bundle_dir <- withr::local_tempdir()

  written <- tempest:::tempest_artifact_bundle_write(
    catalog,
    bundle_dir,
    codec_registry = registry
  )
  index <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, written$index_path)
  )
  restored <- tempest:::tempest_artifact_bundle_read(
    bundle_dir,
    codec_registry = registry
  )

  expect_equal(
    index$artifacts[["external-declared"]]$codec_id,
    "host.external.object"
  )
  expect_equal(
    index$artifacts[["external-declared"]]$codec_version,
    "3"
  )
  expect_equal(
    restored$get("external-declared")@storage_ref,
    "host://objects/declared"
  )

  automatic <- tempest_artifact(
    spec,
    storage_ref = "host://objects/automatic",
    artifact_id = "external-automatic"
  )
  automatic_catalog <- tempest_artifact_catalog(
    artifacts = list(automatic),
    deliverables = list(spec)
  )
  automatic_dir <- withr::local_tempdir()
  custom_only <- tempest_artifact_codec_registry(
    list(external_codec),
    include_builtins = FALSE
  )
  automatic_written <- tempest:::tempest_artifact_bundle_write(
    automatic_catalog,
    automatic_dir,
    codec_registry = custom_only
  )
  automatic_index <- tempest:::tempest_read_json_strict(
    file.path(automatic_dir, automatic_written$index_path)
  )

  expect_equal(
    automatic_index$artifacts[["external-automatic"]]$codec_id,
    "host.external.object"
  )
})

test_that("bundle codecs preflight before artifact promotion", {
  codec <- tempest_artifact_codec_definition(
    "host.text.reverse",
    version = "1",
    media_types = "application/x-reverse-text",
    extension = "rev",
    encode = function(content) rev(charToRaw(content)),
    decode = function(bytes) rawToChar(rev(bytes))
  )
  registry <- tempest_artifact_codec_registry(list(codec))
  spec <- tempest_deliverable_spec(
    "custom",
    title = "Custom",
    purpose = "Exercise codec preflight",
    instructions = "Preserve content.",
    generator_id = "generator",
    renderer_ids = "renderer",
    media_types = "application/x-reverse-text"
  )
  artifact <- tempest_artifact(
    spec,
    content = "Body",
    artifact_id = "custom"
  )
  catalog <- tempest_artifact_catalog(
    artifacts = list(artifact),
    deliverables = list(spec)
  )
  bundle_dir <- withr::local_tempdir()
  tempest:::tempest_artifact_bundle_write(
    catalog,
    bundle_dir,
    codec_registry = registry
  )

  expect_error(
    tempest:::tempest_artifact_bundle_read(bundle_dir),
    class = "tempest_artifact_codec_missing_error"
  )

  index_path <- file.path(bundle_dir, "artifacts/typed/index.json")
  index <- tempest:::tempest_read_json_strict(index_path)
  index$artifacts$custom$codec_version <- "2"
  tempest:::tempest_write_json(index_path, index)
  expect_error(
    tempest:::tempest_artifact_bundle_read(
      bundle_dir,
      codec_registry = registry
    ),
    class = "tempest_artifact_codec_version_error"
  )
})
