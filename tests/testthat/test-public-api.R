test_that("the pre-0.2 public export baseline is exact", {
  expected <- readLines(
    test_path("fixtures", "public-exports-0.1.0.txt"),
    warn = FALSE
  )
  actual <- sort(getNamespaceExports("tempest"), method = "radix")

  expect_identical(actual, expected)
})

test_that("the frozen generic-kernel retirement set is explicit", {
  expected <- readLines(
    test_path("fixtures", "generic-kernel-exports-0.1.0.txt"),
    warn = FALSE
  )
  scheduled <- sort(
    tempest:::tempest_generic_kernel_exports,
    method = "radix"
  )
  public <- getNamespaceExports("tempest")

  expect_identical(scheduled, expected)
  expect_setequal(intersect(scheduled, public), scheduled)
  expect_length(scheduled, 43L)
})

test_that("the pre-0.2 S3 registration baseline is exact", {
  methods <- getNamespaceInfo(asNamespace("tempest"), "S3methods")
  registrations <- paste(methods[, 1], methods[, 2], sep = ".")

  expect_setequal(
    registrations,
    c("print.tempest_okf_bundle", "print.tempest_okf_context")
  )
})
