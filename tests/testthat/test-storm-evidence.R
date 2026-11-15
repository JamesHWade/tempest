test_that("STORM captures URL-only sources and preserves captured evidence", {
  workspace <- tempest_research_workspace()
  missing <- tempest_resource(
    resource_kind = "web",
    title = "Primary source",
    media_type = "text/html",
    locator = "https://example.org/missing"
  )
  captured <- tempest_resource(
    resource_kind = "web",
    title = "Primary source",
    media_type = "text/html",
    locator = "https://example.org/captured",
    content = "Exact captured evidence."
  )
  workspace$upsert_retrieved_resource(missing)
  workspace$upsert_retrieved_resource(captured)
  fetched <- character()
  retriever <- list(fetch = function(url) {
    fetched <<- c(fetched, url)
    workspace$upsert_retrieved_resource(tempest_resource(
      resource_kind = "web",
      title = "Primary source",
      media_type = "text/html",
      locator = url,
      content = "Newly fetched source text."
    ))
  })

  tempest:::tempest_storm_capture_sources(
    retriever,
    workspace,
    c(missing@resource_id, captured@resource_id, missing@resource_id)
  )

  expect_equal(fetched, missing@locator)
  expect_identical(
    workspace$get_retrieved_resource(captured@resource_id),
    captured
  )
  expect_equal(
    workspace$get_retrieved_resource(missing@resource_id)@content,
    "Newly fetched source text."
  )
})

test_that("failed captures never turn answer prose into evidence", {
  workspace <- tempest_research_workspace()
  resource <- tempest_resource(
    resource_kind = "web",
    title = "Primary source",
    media_type = "text/html",
    locator = "https://example.org/unavailable"
  )
  workspace$upsert_retrieved_resource(resource)
  retriever <- list(fetch = function(url) {
    rlang::abort("Unavailable", class = "capture_test_error")
  })

  expect_warning(
    tempest:::tempest_storm_capture_sources(
      retriever,
      workspace,
      resource@resource_id
    ),
    class = "tempest_source_capture_warning"
  )
  expect_null(workspace$get_retrieved_resource(resource@resource_id)@content)
  expect_length(workspace$list_evidence_spans(), 0L)
})

test_that("empty fetch results are explicitly reported", {
  workspace <- tempest_research_workspace()
  resource <- tempest_resource(
    resource_kind = "web",
    title = "Unavailable",
    media_type = "text/html",
    locator = "https://example.org/empty",
    metadata = list(error = "No text")
  )
  workspace$upsert_retrieved_resource(resource)
  retriever <- list(fetch = function(url) resource)
  expect_warning(
    tempest:::tempest_storm_capture_sources(
      retriever,
      workspace,
      resource@resource_id
    ),
    class = "tempest_source_capture_warning"
  )
  expect_null(workspace$get_retrieved_resource(resource@resource_id)@content)
})

test_that("source capture preserves governance and workspace errors", {
  workspace <- tempest_research_workspace()
  resource <- tempest_resource(
    resource_kind = "web",
    title = "Primary source",
    media_type = "text/html",
    locator = "https://example.org/governed"
  )
  workspace$upsert_retrieved_resource(resource)

  for (error_class in c(
    "tempest_stage_governance_error",
    "tempest_research_workspace_error"
  )) {
    retriever <- list(fetch = function(url) {
      rlang::abort("Capture denied", class = error_class)
    })
    expect_error(
      tempest:::tempest_storm_capture_sources(
        retriever,
        workspace,
        resource@resource_id
      ),
      class = error_class
    )
  }
  expect_error(
    tempest:::tempest_storm_capture_sources(list(), workspace, "unknown"),
    class = "tempest_stage_governance_error"
  )
  expect_null(workspace$get_retrieved_resource(resource@resource_id)@content)
})
