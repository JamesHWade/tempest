test_that("canonical deliverable fingerprints ignore map ordering", {
  first <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    content_schema = list(answer = "character", risks = "character"),
    generator_id = "generator",
    renderer_ids = "renderer",
    metadata = list(owner = "host", app = "example")
  )
  second <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    content_schema = list(risks = "character", answer = "character"),
    generator_id = "generator",
    renderer_ids = "renderer",
    metadata = list(app = "example", owner = "host")
  )

  expect_equal(
    tempest:::tempest_deliverable_spec_checksum(first),
    tempest:::tempest_deliverable_spec_checksum(second)
  )
})

test_that("UTF-8 text codecs preserve exact bytes", {
  encoded <- tempest:::tempest_artifact_codec_encode(
    "Résumé — ready",
    "text/markdown"
  )
  decoded <- tempest:::tempest_artifact_codec_decode(encoded, encoded$bytes)

  expect_equal(encoded$codec_id, "tempest.text.utf8")
  expect_equal(encoded$extension, "md")
  expect_equal(decoded, "Résumé — ready")
  expect_equal(encoded$byte_size, length(encoded$bytes))
  expect_match(encoded$sha256, "^[a-f0-9]{64}$")
})

test_that("artifact codec alias creates runtime definitions", {
  codec <- tempest_artifact_codec(
    "host.plain",
    encode = function(content, media_type) charToRaw(content),
    decode = function(bytes) rawToChar(bytes),
    media_types = "text/plain",
    extension = "txt"
  )

  expect_s3_class(codec, "tempest_artifact_codec_definition")
  expect_identical(codec$codec_id, "host.plain")
})

test_that("canonical JSON codecs round-trip structured content", {
  content <- list(
    response = "Done",
    actions = list(
      list(owner = "Team", priority = 1L)
    )
  )
  encoded <- tempest:::tempest_artifact_codec_encode(
    content,
    "application/json"
  )
  decoded <- tempest:::tempest_artifact_codec_decode(encoded, encoded$bytes)

  expect_equal(encoded$codec_id, "tempest.json.canonical")
  expect_equal(decoded$response, "Done")
  expect_equal(decoded$actions[[1]]$owner, "Team")
  expect_equal(decoded$actions[[1]]$priority, 1L)
})

test_that("artifact codecs reject tampering and unknown codecs", {
  encoded <- tempest:::tempest_artifact_codec_encode("Body", "text/plain")
  tampered <- charToRaw("Other")

  expect_error(
    tempest:::tempest_artifact_codec_decode(encoded, tampered),
    class = "tempest_artifact_codec_error"
  )
  encoded$codec_id <- "host.unknown"
  expect_error(
    tempest:::tempest_artifact_codec_decode(
      encoded,
      charToRaw("Body")
    ),
    class = "tempest_artifact_codec_error"
  )
  expect_error(
    tempest:::tempest_canonical_json(list(callback = function() NULL)),
    class = "tempest_artifact_codec_error"
  )
  expect_error(
    tempest:::tempest_artifact_codec_encode(
      data.frame(value = 1),
      "application/json"
    ),
    class = "tempest_artifact_codec_error"
  )
  expect_error(
    tempest:::tempest_artifact_codec_encode(
      list(value = NA_character_),
      "application/json"
    ),
    class = "tempest_artifact_codec_error"
  )

  invalid_utf8 <- as.raw(255)
  expect_error(
    tempest:::tempest_artifact_codec_decode(
      list(
        codec_id = "tempest.text.utf8",
        codec_version = "1",
        byte_size = 1L,
        sha256 = digest::digest(
          invalid_utf8,
          algo = "sha256",
          serialize = FALSE
        )
      ),
      invalid_utf8
    ),
    class = "tempest_artifact_codec_error"
  )
})

test_that("deliverable, validation, and artifact records are validated", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator",
    validator_ids = "validator",
    renderer_ids = "renderer",
    operation_versions = c(generator = "2", renderer = "3")
  )
  validation <- tempest_validation_result(
    "validator",
    status = "warning",
    message = "Review tone.",
    created_at = "2026-07-18 UTC"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Brief",
    artifact_id = "artifact-1",
    validation_results = list(validation),
    status = "valid",
    created_at = "2026-07-18 UTC"
  )

  restored_spec <- tempest:::tempest_deliverable_spec_from_data(
    tempest:::tempest_deliverable_spec_record(spec)
  )
  restored_artifact <- tempest:::tempest_artifact_from_data(
    tempest:::tempest_artifact_record(artifact),
    restored_spec
  )

  expect_equal(restored_spec@operation_versions, spec@operation_versions)
  expect_equal(restored_artifact@content, "# Brief")
  expect_equal(
    restored_artifact@validation_results[[1]]@status,
    "warning"
  )
  expect_equal(restored_artifact@checksum, artifact@checksum)
})

test_that("record restoration detects specification and content tampering", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator",
    renderer_ids = "renderer"
  )
  spec_record <- tempest:::tempest_deliverable_spec_record(spec)
  spec_record$instructions <- "Tampered"
  expect_error(
    tempest:::tempest_deliverable_spec_from_data(spec_record),
    class = "tempest_artifact_codec_error"
  )

  artifact <- tempest_artifact(spec, content = "# Brief")
  artifact_record <- tempest:::tempest_artifact_record(artifact)
  expect_error(
    tempest:::tempest_artifact_from_data(
      artifact_record,
      spec,
      content = "Tampered"
    ),
    class = "tempest_artifact_codec_error"
  )
})

test_that("external references restore without dereferencing", {
  spec <- tempest_deliverable_spec(
    "external",
    title = "External",
    purpose = "Reference an external object",
    instructions = "Do not read it.",
    generator_id = "generator",
    renderer_ids = "renderer",
    media_types = "application/octet-stream"
  )
  artifact <- tempest_artifact(
    spec,
    storage_ref = "/path/that/does/not/exist",
    artifact_id = "external-1"
  )

  restored <- tempest:::tempest_artifact_from_data(
    tempest:::tempest_artifact_record(
      artifact,
      include_content = FALSE
    ),
    spec
  )

  expect_null(restored@content)
  expect_equal(restored@storage_ref, "/path/that/does/not/exist")
})

test_that("codec registries expose durable metadata without functions", {
  registry <- tempest_artifact_codec_registry()
  listing <- registry$list()
  contains_function <- function(value) {
    is.function(value) ||
      (is.list(value) &&
        any(vapply(value, contains_function, logical(1))))
  }

  expect_named(
    listing,
    c(
      "tempest.external.reference",
      "tempest.json.canonical",
      "tempest.text.utf8"
    )
  )
  expect_identical(contains_function(listing), FALSE)
  expect_identical(
    listing[["tempest.external.reference"]]$external,
    TRUE
  )
})

test_that("custom codecs resolve by id, version, and media type", {
  reverse_codec <- tempest_artifact_codec_definition(
    "host.text.reverse",
    version = "2026.1",
    media_types = "application/x-reverse-text",
    extension = "rev",
    encode = function(content) rev(charToRaw(enc2utf8(content))),
    decode = function(bytes) rawToChar(rev(bytes)),
    supports = function(content) {
      is.character(content) && length(content) == 1L
    }
  )
  registry <- tempest_artifact_codec_registry(
    list(reverse_codec),
    include_builtins = FALSE
  )
  encoded <- tempest:::tempest_artifact_codec_encode(
    "Custom body",
    "application/x-reverse-text",
    registry = registry
  )
  decoded <- tempest:::tempest_artifact_codec_decode(
    encoded,
    encoded$bytes,
    registry = registry
  )

  expect_equal(encoded$codec_id, "host.text.reverse")
  expect_equal(encoded$codec_version, "2026.1")
  expect_equal(encoded$extension, "rev")
  expect_equal(decoded, "Custom body")
  expect_error(
    registry$resolve("missing.codec"),
    class = "tempest_artifact_codec_missing_error"
  )
  expect_error(
    registry$resolve("host.text.reverse", version = "2"),
    class = "tempest_artifact_codec_version_error"
  )
  expect_error(
    registry$resolve(
      "host.text.reverse",
      media_type = "text/plain"
    ),
    class = "tempest_artifact_codec_media_error"
  )
})
