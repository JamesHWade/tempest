test_that("operation registries resolve functions by id", {
  registry <- tempest_operation_registry()
  registry$register(
    "generator.summary",
    function(value) paste("Summary:", value),
    version = "2",
    kind = "generator",
    metadata = list(owner = "tempest")
  )

  implementation <- registry$resolve(
    "generator.summary",
    version = "2",
    kind = "generator"
  )
  descriptor <- registry$describe(
    "generator.summary",
    version = "2",
    kind = "generator"
  )

  expect_equal(implementation("content"), "Summary: content")
  expect_equal(descriptor$metadata$owner, "tempest")
  expect_false("implementation" %in% names(descriptor))
  expect_identical(
    registry$has("generator.summary", kind = "generator"),
    TRUE
  )
  expect_identical(
    registry$has("generator.summary", version = "1"),
    FALSE
  )
})

test_that("registry listings never expose implementation closures", {
  registry <- tempest_operation_registry(list(
    "renderer.markdown" = list(
      version = "2026.1",
      kind = "renderer",
      implementation = function(content) content,
      metadata = list(media_type = "text/markdown")
    )
  ))

  listing <- registry$list()

  expect_named(listing, "renderer.markdown")
  expect_named(
    listing[["renderer.markdown"]],
    c("id", "version", "kind", "metadata")
  )
  expect_equal(listing[["renderer.markdown"]]$version, "2026.1")
  expect_equal(
    listing[["renderer.markdown"]]$metadata$media_type,
    "text/markdown"
  )
})

test_that("constructor accepts named function shorthand", {
  registry <- tempest_operation_registry(list(
    identity = function(value) value
  ))

  expect_identical(registry$resolve("identity")(1L), 1L)
  expect_equal(registry$list()$identity$kind, "step")
  expect_equal(registry$list()$identity$version, "1")
})

test_that("operation replacement is explicit", {
  registry <- tempest_operation_registry(list(
    operation = function() "first"
  ))

  expect_error(
    registry$register("operation", function() "second"),
    class = "tempest_operation_registry_error"
  )

  registry$register(
    "operation",
    function() "second",
    replace = TRUE
  )
  expect_equal(registry$resolve("operation")(), "second")
})

test_that("resolution failures are classed and informative", {
  registry <- tempest_operation_registry(list(
    render = list(
      version = "2",
      kind = "renderer",
      implementation = function(value) value
    )
  ))

  expect_error(
    registry$resolve("missing"),
    class = "tempest_operation_registry_error"
  )
  expect_error(
    registry$resolve("render", version = "1"),
    class = "tempest_operation_registry_error"
  )
  expect_error(
    registry$resolve("render", kind = "validator"),
    class = "tempest_operation_registry_error"
  )
  expect_identical(registry$has("missing"), FALSE)
  expect_identical(registry$has("render", kind = "anything"), FALSE)
})

test_that("operation descriptors validate runtime-only implementations", {
  expect_error(
    tempest_operation_registry(list(
      unnamed = list(kind = "renderer")
    )),
    class = "tempest_operation_registry_error"
  )
  expect_error(
    tempest_operation_registry(list(
      invalid = list(
        kind = "unknown",
        implementation = function() NULL
      )
    )),
    class = "tempest_operation_registry_error"
  )
  expect_error(
    tempest_operation_registry(list(function() NULL)),
    class = "tempest_operation_registry_error"
  )
})
