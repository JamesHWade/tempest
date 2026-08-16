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

test_that("catalog lineage validation reads a ResearchWorkspace", {
  workspace <- tempest_research_workspace()
  source <- fake_source("https://example.org/catalog-workspace")
  workspace$upsert_source(source)
  span <- tempest_evidence_span(
    source_id = source$id,
    quote = "Catalog evidence"
  )
  workspace$add_evidence_span(span)
  claim <- tempest_claim(
    claim_text = "Catalog lineage resolves.",
    source_ids = source$id,
    evidence_span_ids = span@evidence_span_id
  )
  workspace$add_claim(claim)
  spec <- tempest_deliverable_spec(
    "lineage",
    title = "Lineage",
    purpose = "Validate evidence references",
    instructions = "Preserve evidence lineage.",
    generator_id = "generator.lineage",
    renderer_ids = "renderer.lineage"
  )
  artifact <- tempest_artifact(
    spec,
    content = "Resolved lineage",
    resource_ids = source$id,
    claim_ids = claim@claim_id,
    evidence_span_ids = span@evidence_span_id
  )
  catalog <- tempest_artifact_catalog(deliverables = list(spec))
  catalog$add(artifact)

  expect_no_error(tempest_artifact_catalog_restore(
    catalog$snapshot(),
    evidence_store = workspace
  ))
})

test_that("artifact store adapters cover typed inline and external artifacts", {
  spec <- tempest_deliverable_spec(
    "stored",
    title = "Stored",
    purpose = "Exercise the adapter",
    instructions = "Preserve content.",
    version = "3",
    generator_id = "generator",
    renderer_ids = "renderer",
    media_types = c("text/plain", "application/octet-stream")
  )
  inline <- tempest_artifact(
    spec,
    content = "Body",
    artifact_id = "inline",
    media_type = "text/plain"
  )
  external <- tempest_artifact(
    spec,
    storage_ref = "host://objects/external",
    artifact_id = "external",
    media_type = "application/octet-stream"
  )
  store <- tempest_memory_artifact_store()

  store$write(inline)
  store$write(external)
  listing <- store$list()

  expect_identical(store$read("inline"), inline)
  expect_null(store$read("missing"))
  expect_identical(store$exists("external", "3"), TRUE)
  expect_equal(store$version("inline"), "3")
  expect_null(store$version("missing"))
  expect_named(listing, c("external", "inline"))
  expect_null(listing$inline$content)
  expect_null(listing$external$content)
})

test_that("artifact store adapter failures and invalid listings are classed", {
  failing <- tempest_artifact_store(
    read = function(artifact_id, default) stop("read failed"),
    list_metadata = function() stop("list failed"),
    exists = function(artifact_id, version) stop("exists failed"),
    version = function(artifact_id, default) stop("version failed")
  )

  expect_error(
    failing$read("artifact"),
    class = "tempest_artifact_store_error"
  )
  expect_error(
    failing$list(),
    class = "tempest_artifact_store_error"
  )
  expect_error(
    failing$exists("artifact"),
    class = "tempest_artifact_store_error"
  )
  expect_error(
    failing$version("artifact"),
    class = "tempest_artifact_store_error"
  )

  invalid <- tempest_artifact_store(
    list_metadata = function() {
      list(
        artifact = list(
          artifact_id = "artifact",
          callback = function() NULL
        )
      )
    }
  )
  expect_error(
    invalid$list(),
    class = "tempest_artifact_store_error"
  )
})

test_that("artifact store reads enforce artifact identity", {
  spec <- tempest_deliverable_spec(
    "stored",
    title = "Stored",
    purpose = "Exercise read identity",
    instructions = "Return the requested artifact.",
    generator_id = "generator",
    renderer_ids = "renderer"
  )
  wrong_artifact <- tempest_artifact(
    spec,
    content = "Wrong artifact",
    artifact_id = "artifact-b"
  )
  store <- tempest_artifact_store(
    read = function(artifact_id, default) wrong_artifact
  )

  expect_error(
    store$read("artifact-a"),
    class = "tempest_artifact_store_error"
  )
  expect_error(
    store$version("artifact-a"),
    class = "tempest_artifact_store_error"
  )
})
