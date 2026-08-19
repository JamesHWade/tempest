test_that("the 0.2 public export surface is exact", {
  expected <- readLines(
    test_path("fixtures", "public-exports-0.2.0.txt"),
    warn = FALSE
  )
  actual <- sort(getNamespaceExports("tempest"), method = "radix")

  expect_length(expected, 62L)
  expect_identical(expected, sort(unique(expected), method = "radix"))
  expect_identical(actual, expected)
  namespace <- asNamespace("tempest")
  expect_identical(
    exists("tempest_run_restore", envir = namespace, inherits = FALSE),
    FALSE
  )
  expect_identical(
    exists("tempest_run_resume", envir = namespace, inherits = FALSE),
    FALSE
  )
})

test_that("the pre-0.2 public export fixture remains historical", {
  baseline <- readLines(
    test_path("fixtures", "public-exports-0.1.0.txt"),
    warn = FALSE
  )

  expect_length(baseline, 96L)
  expect_identical(baseline, sort(unique(baseline), method = "radix"))
})

test_that("the 0.2 S3 registration surface is exact", {
  methods <- getNamespaceInfo(asNamespace("tempest"), "S3methods")
  registrations <- paste(methods[, 1], methods[, 2], sep = ".")

  expect_identical(nrow(methods), 2L)
  expect_setequal(
    registrations,
    c("print.tempest_okf_bundle", "print.tempest_okf_context")
  )
})
