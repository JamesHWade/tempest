test_that("the 0.2 public export surface is exact", {
  baseline <- readLines(
    test_path("fixtures", "public-exports-0.1.0.txt"),
    warn = FALSE
  )
  additions <- c(
    "ResearchWorkspace",
    "tempest_claim_support",
    "tempest_claim_supports",
    "tempest_compile_programs",
    "tempest_governed_procedure_ref",
    "tempest_graft_plan",
    "tempest_graft_schema",
    "tempest_load_program_set",
    "tempest_program_set",
    "tempest_promotion_bundle",
    "tempest_promotion_receipt",
    "tempest_read_promotion_bundle",
    "tempest_research_manifest",
    "tempest_research_workspace",
    "tempest_save_program_set",
    "tempest_save_promotion_bundle"
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
  expect_length(actual, 103L)
  expect_identical(actual, expected)
  expect_identical(setdiff(actual, baseline), additions)
  expect_identical(intersect(actual, removals), character())
})

test_that("the frozen generic-kernel retirement set is explicit", {
  frozen <- readLines(
    test_path("fixtures", "generic-kernel-exports-0.1.0.txt"),
    warn = FALSE
  )
  internal <- c("tempest_run_restore", "tempest_run_resume")
  expected <- setdiff(frozen, internal)
  scheduled <- tempest:::tempest_generic_kernel_exports
  public <- getNamespaceExports("tempest")

  expect_length(frozen, 43L)
  expect_identical(scheduled, expected)
  expect_setequal(intersect(scheduled, public), scheduled)
  expect_identical(intersect(internal, public), character())
  expect_length(scheduled, 41L)
  expect_length(public, 103L)
})

test_that("the pre-0.2 S3 registration baseline is exact", {
  methods <- getNamespaceInfo(asNamespace("tempest"), "S3methods")
  registrations <- paste(methods[, 1], methods[, 2], sep = ".")

  expect_setequal(
    registrations,
    c("print.tempest_okf_bundle", "print.tempest_okf_context")
  )
})
