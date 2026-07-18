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
  catalog <- tempest_artifact_catalog(deliverables = list(spec))

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
  catalog <- tempest_artifact_catalog(
    artifacts = list(artifact),
    deliverables = list(spec)
  )

  metadata <- catalog$list(status = "invalid")
  snapshot <- catalog$snapshot(include_content = TRUE)

  expect_named(metadata, "artifact-1")
  expect_contains(names(metadata[["artifact-1"]]), "checksum")
  expect_null(metadata[["artifact-1"]]$content)
  expect_equal(snapshot$artifacts[["artifact-1"]]$content, "# Brief")
  expect_named(snapshot$deliverables, "brief@1")
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
  catalog <- tempest_artifact_catalog(
    artifacts = list(first),
    deliverables = list(spec)
  )

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
    write = function(artifact) {
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
  catalog <- tempest_artifact_catalog(
    store = store,
    deliverables = list(spec)
  )

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

test_that("catalogs require and fingerprint deliverable specifications", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  changed <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Use great detail.",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  artifact <- tempest_artifact(spec, content = "# Brief")
  catalog <- tempest_artifact_catalog()

  expect_error(
    catalog$add(artifact),
    class = "tempest_artifact_catalog_error"
  )
  catalog$register(spec)
  expect_no_error(catalog$add(artifact))
  expect_error(
    catalog$register(changed),
    class = "tempest_artifact_catalog_error"
  )
  expect_equal(
    catalog$get_deliverable("brief", "1")@instructions,
    "Be concise."
  )
})

test_that("catalog snapshots restore specifications, artifacts, and lineage", {
  spec <- tempest_deliverable_spec(
    "brief",
    title = "Brief",
    purpose = "Summarize",
    instructions = "Be concise.",
    generator_id = "generator.brief",
    renderer_ids = "renderer.markdown"
  )
  parent <- tempest_artifact(
    spec,
    content = "# Parent",
    artifact_id = "parent"
  )
  child <- tempest_artifact(
    spec,
    content = "# Child",
    artifact_id = "child",
    parent_artifact_ids = "parent"
  )
  catalog <- tempest_artifact_catalog(deliverables = list(spec))
  catalog$add_many(list(parent, child))

  restored <- tempest_artifact_catalog_restore(catalog$snapshot())

  expect_equal(restored$get("child")@content, "# Child")
  expect_equal(restored$get("child")@parent_artifact_ids, "parent")
  expect_equal(
    restored$get_deliverable("brief", "1")@instructions,
    "Be concise."
  )

  invalid <- catalog$snapshot()
  invalid$artifacts$child$parent_artifact_ids <- "missing"
  expect_error(
    tempest_artifact_catalog_restore(invalid),
    class = "tempest_artifact_catalog_error"
  )
})
