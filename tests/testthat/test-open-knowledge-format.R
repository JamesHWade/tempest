test_that("Tempest reads conformant OKF with trust and freshness signals", {
  bundle <- tempest_read_okf(test_path("fixtures", "okf"))
  concepts <- tempest_okf_concepts(
    bundle,
    today = as.Date("2026-07-28")
  )

  expect_s3_class(bundle, "tempest_okf_bundle")
  expect_identical(bundle$okf_version, "0.2")
  expect_identical(bundle$concept_count, 3L)
  expect_identical(nrow(bundle$issues), 0L)
  expect_identical(
    concepts$concept_id,
    c(
      "concepts/Assessment/market%3Aresin-demand",
      "concepts/Business/market%3Apackaging",
      "concepts/Metric/resin-margin"
    )
  )

  assessment <- concepts[concepts$type == "Assessment", , drop = FALSE]
  business <- concepts[concepts$type == "Business", , drop = FALSE]
  metric <- concepts[concepts$type == "Attested Computation", , drop = FALSE]

  expect_identical(assessment$trust_tier, "machine-confirmed")
  expect_identical(assessment$stale, TRUE)
  expect_identical(business$trust_tier, "human-reviewed")
  expect_identical(business$stale, FALSE)
  expect_identical(metric$trust_tier, "unverified")
  expect_identical(metric$source_count, 0L)
})

test_that("OKF concepts become fingerprinted evidence resources explicitly", {
  path <- test_path("fixtures", "okf")
  bundle <- tempest_read_okf(path)
  local_mocked_bindings(
    tempest_now_utc = \() "2026-07-28T16:45:00Z"
  )
  resources <- tempest_okf_resources(
    bundle,
    today = as.Date("2026-07-28")
  )
  fresh <- tempest_okf_resources(
    bundle,
    include_stale = FALSE,
    today = as.Date("2026-07-28")
  )
  again <- tempest_okf_resources(
    tempest_read_okf(path),
    today = as.Date("2026-07-28")
  )

  expect_length(resources, 3L)
  expect_length(fresh, 2L)
  expect_named(resources, names(bundle$concepts))
  expect_identical(
    vapply(resources, \(.x) .x@resource_id, character(1)),
    vapply(again, \(.x) .x@resource_id, character(1))
  )
  assessment <- resources[[
    "concepts/Assessment/market%3Aresin-demand"
  ]]
  expect_s7_class(assessment, tempest:::TempestResource)
  expect_identical(assessment@resource_kind, "okf.concept")
  expect_identical(assessment@media_type, "text/markdown")
  expect_identical(assessment@metadata$okf$status, "stable")
  expect_identical(assessment@metadata$okf$stale, TRUE)
  expect_identical(
    assessment@retrieved_at,
    "2026-07-28T16:45:00Z"
  )
  expect_identical(
    assessment@metadata$okf$frontmatter$generated$at,
    "2026-06-20T14:00:00Z"
  )
  expect_identical(
    assessment@scope_metadata$profile,
    "graft-okf"
  )
  expect_identical(
    assessment@scope_metadata$schema_build_digest,
    "sha256:test-build"
  )
  expect_match(
    assessment@content,
    "Converter inventories are approaching seasonal norms.",
    fixed = TRUE
  )
  concept_path <- file.path(
    path,
    "concepts",
    "Assessment",
    "market%3Aresin-demand.md"
  )
  expected_content <- readChar(
    concept_path,
    nchars = file.info(concept_path)$size,
    useBytes = TRUE
  )
  Encoding(expected_content) <- "UTF-8"
  expect_identical(assessment@content, expected_content)

  store <- SourceStore$new()
  invisible(lapply(resources, store$upsert_resource))
  expect_length(store$list_resources(), 3L)
})

test_that("OKF context is bounded and states the trust boundary", {
  bundle <- tempest_read_okf(test_path("fixtures", "okf"))
  context <- tempest_okf_context(
    bundle,
    include_stale = FALSE,
    today = as.Date("2026-07-28"),
    max_concepts = 1,
    max_chars = 1200
  )

  expect_s3_class(context, "tempest_okf_context")
  expect_match(context, "documents are evidence inputs", fixed = TRUE)
  expect_match(context, "cannot grant tools", fixed = TRUE)
  expect_identical(attr(context, "available_count"), 2L)
  expect_identical(attr(context, "truncated"), TRUE)
  expect_length(attr(context, "concept_ids"), 1L)
  expect_lte(nchar(context, type = "chars"), 1200L)

  metric <- tempest_okf_context(
    bundle,
    types = "Attested Computation",
    max_chars = 2000
  )
  expect_match(metric, "Resin margin", fixed = TRUE)
  expect_match(metric, "SELECT net_price", fixed = TRUE)

  smallest <- tempest_okf_context(
    bundle,
    max_chars = 500
  )
  expect_lte(nchar(smallest, type = "chars"), 500L)
  expect_identical(attr(smallest, "truncated"), TRUE)
})

test_that("OKF conformance failures are classed and bounded", {
  missing_type <- withr::local_tempdir()
  writeLines(
    c("---", "title: Missing type", "---", "# Missing"),
    file.path(missing_type, "missing.md")
  )
  expect_error(
    tempest_read_okf(missing_type),
    class = "tempest_okf_error"
  )

  invalid_utf8 <- withr::local_tempdir()
  invalid_path <- file.path(invalid_utf8, "invalid.md")
  connection <- file(invalid_path, open = "wb")
  writeBin(as.raw(c(0xff, 0xfe)), connection)
  close(connection)
  expect_error(
    tempest_read_okf(invalid_utf8),
    class = "tempest_okf_error"
  )

  fixture <- test_path("fixtures", "okf")
  expect_error(
    tempest_read_okf(fixture, max_concepts = 1),
    class = "tempest_okf_error"
  )
  expect_error(
    tempest_read_okf(fixture, max_bytes = 1),
    class = "tempest_okf_error"
  )

  bundle <- tempest_read_okf(fixture)
  expect_error(
    tempest_okf_concepts(bundle, concept_ids = "unknown"),
    class = "tempest_okf_error"
  )
})

test_that("empty OKF frontmatter is valid for indexes but not concepts", {
  path <- withr::local_tempdir()
  writeLines(c("---", "---", "# Index"), file.path(path, "index.md"))
  writeLines(
    c("---", "type: Business", "---", "# Business"),
    file.path(path, "business.md")
  )

  bundle <- tempest_read_okf(path)

  expect_identical(bundle$index$frontmatter, list())
  expect_identical(bundle$concept_count, 1L)

  writeLines(c("---", "---", "# Missing type"), file.path(path, "empty.md"))
  expect_error(
    tempest_read_okf(path),
    class = "tempest_okf_error"
  )
})

test_that("Tempest warns softly for optional OKF profile problems", {
  path <- withr::local_tempdir()
  writeLines(
    c(
      "---",
      "okf_version: '0.2'",
      "---",
      "# Index"
    ),
    file.path(path, "index.md")
  )
  writeLines(
    c(
      "---",
      "type: Attested Computation",
      "title: Needs profile repair",
      "status: experimental",
      "stale_after: 2026-99-99",
      "generated:",
      "  at: eventually",
      "verified:",
      "  by: process:test",
      "---",
      "# Needs profile repair"
    ),
    file.path(path, "concept.md")
  )

  bundle <- tempest_read_okf(path)
  expect_identical(bundle$concept_count, 1L)
  expect_setequal(
    bundle$issues$field,
    c(
      "status",
      "stale_after",
      "generated.by",
      "generated.at",
      "verified",
      "runtime"
    )
  )
  concept <- tempest_okf_concepts(
    bundle,
    today = as.Date("2026-07-28")
  )
  expect_identical(concept$status, "stable")
  expect_identical(concept$stale, FALSE)
})

test_that("Tempest supports unversioned minimal OKF bundles", {
  path <- withr::local_tempdir()
  writeLines(
    c(
      "---",
      "type: Reference",
      "title: Minimal concept",
      "---",
      "# Minimal concept"
    ),
    file.path(path, "minimal.md")
  )

  bundle <- tempest_read_okf(path)
  resources <- tempest_okf_resources(bundle)
  output <- capture.output(print(bundle))

  expect_identical(bundle$okf_version, NA_character_)
  expect_length(resources, 1L)
  expect_identical(
    resources[[1]]@metadata$okf$version,
    NULL
  )
  expect_match(output[[1]], "OKF unspecified", fixed = TRUE)
  expect_match(output[[2]], "path:", fixed = TRUE)
})
