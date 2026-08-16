test_that("the 0.2 public export surface is exact", {
  baseline <- readLines(
    test_path("fixtures", "public-exports-0.1.0.txt"),
    warn = FALSE
  )
  additions <- c(
    "ResearchWorkspace",
    "tempest_compile_programs",
    "tempest_load_program_set",
    "tempest_program_set",
    "tempest_research_manifest",
    "tempest_research_workspace",
    "tempest_save_program_set"
  )
  removals <- c(
    "SourceStore",
    "TempestSession",
    "tempest_expert_session_manager",
    "tempest_load_dsprrr_modules",
    "tempest_optimize_dsprrr_modules",
    "tempest_resources",
    "tempest_run_restore",
    "tempest_run_resume",
    "tempest_save_dsprrr_modules"
  )
  expected <- sort(c(setdiff(baseline, removals), additions), method = "radix")
  actual <- sort(getNamespaceExports("tempest"), method = "radix")

  expect_length(baseline, 96L)
  expect_length(actual, 94L)
  expect_identical(actual, expected)
  expect_identical(setdiff(actual, baseline), additions)
  expect_identical(intersect(actual, removals), character())
})

test_that("the frozen generic-kernel retirement set is explicit", {
  expected <- readLines(
    test_path("fixtures", "generic-kernel-exports-0.1.0.txt"),
    warn = FALSE
  )
  expected <- setdiff(
    expected,
    c("tempest_run_restore", "tempest_run_resume")
  )
  scheduled <- sort(
    tempest:::tempest_generic_kernel_exports,
    method = "radix"
  )
  public <- getNamespaceExports("tempest")

  expect_identical(scheduled, expected)
  expect_setequal(intersect(scheduled, public), scheduled)
  expect_length(scheduled, 41L)
})

test_that("the pre-0.2 S3 registration baseline is exact", {
  methods <- getNamespaceInfo(asNamespace("tempest"), "S3methods")
  registrations <- paste(methods[, 1], methods[, 2], sep = ".")

  expect_setequal(
    registrations,
    c("print.tempest_okf_bundle", "print.tempest_okf_context")
  )
})
