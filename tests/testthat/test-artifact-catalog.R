test_that("artifact catalogs store typed artifacts by id", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    version = "2",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Brief",
    artifact_id = "artifact-1",
    status = "valid"
  )
  catalog <- tempest_artifact_catalog()

  catalog$add(artifact)

  expect_identical(catalog$get("artifact-1"), artifact)
  expect_identical(catalog$has("artifact-1"), TRUE)
  expect_identical(catalog$has("artifact-1", version = "2"), TRUE)
  expect_identical(catalog$has("artifact-1", version = "1"), FALSE)
  expect_equal(catalog$version("artifact-1"), "2")
})

test_that("catalog listings omit content by default", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Brief",
    artifact_id = "artifact-1",
    status = "invalid"
  )
  catalog <- tempest_artifact_catalog(artifacts = list(artifact))

  metadata <- catalog$list(status = "invalid")
  snapshot <- catalog$snapshot(include_content = TRUE)

  expect_named(metadata, "artifact-1")
  expect_contains(names(metadata[["artifact-1"]]), "checksum")
  expect_null(metadata[["artifact-1"]]$content)
  expect_equal(snapshot[["artifact-1"]]$content, "# Brief")
  expect_length(catalog$list(deliverable_id = "other"), 0L)
})

test_that("catalog replacement must be explicit", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  first <- tempest_artifact(
    spec,
    content = "First",
    artifact_id = "artifact-1"
  )
  second <- tempest_artifact(
    spec,
    content = "Second",
    artifact_id = "artifact-1"
  )
  catalog <- tempest_artifact_catalog(artifacts = list(first))

  expect_error(
    catalog$add(second),
    class = "tempest_artifact_catalog_error"
  )
  catalog$add(second, replace = TRUE)
  expect_equal(catalog$get("artifact-1")@content, "Second")
})

test_that("catalog persistence is atomic and classed", {
  calls <- 0L
  store <- tempest_artifact_store(
    write = function(name, value, metadata) {
      calls <<- calls + 1L
      stop("storage unavailable")
    }
  )
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  artifact <- tempest_artifact(
    spec,
    content = "# Brief",
    artifact_id = "artifact-1"
  )
  catalog <- tempest_artifact_catalog(store = store)

  expect_error(
    catalog$add(artifact),
    class = "tempest_artifact_catalog_error"
  )
  expect_identical(calls, 1L)
  expect_identical(catalog$has("artifact-1"), FALSE)
})

test_that("catalogs validate artifacts and ids", {
  catalog <- tempest_artifact_catalog()

  expect_error(
    tempest_artifact_catalog(artifacts = list("not an artifact")),
    class = "tempest_artifact_catalog_error"
  )
  expect_error(
    catalog$add("not an artifact"),
    class = "tempest_artifact_catalog_error"
  )
  expect_error(
    catalog$get("missing"),
    class = "tempest_artifact_catalog_error"
  )
  expect_null(catalog$get("missing", error = FALSE))
})
