ecosystem_demo_helper <- function() {
  helper <- new.env(parent = globalenv())
  sys.source(tempest_pkg_file("examples", "ellmerverse", "fixture.R"), helper)
  helper
}

ecosystem_demo_product <- local({
  product <- NULL
  function() {
    if (is.null(product)) {
      directory <- tempfile("tempest-ecosystem-")
      on.exit(unlink(directory, recursive = TRUE), add = TRUE)
      product <<- ecosystem_demo_helper()$tempest_demo_research(directory)
    }
    product
  }
})
