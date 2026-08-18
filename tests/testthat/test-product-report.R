test_that("product reports avoid generic report and codec helpers", {
  reject_generic <- function(...) {
    rlang::abort("generic helper reached", class = "test_generic_reached")
  }
  local_mocked_bindings(
    tempest_deliverable_abort = reject_generic,
    tempest_artifact_codec_encode = reject_generic,
    tempest_deliverable_spec_checksum = reject_generic,
    tempest_canonical_json = reject_generic
  )
  workspace <- tempest_research_workspace()

  report <- tempest_report_md(
    "Fixture",
    "Body.",
    workspace,
    citation_policy = "none"
  )
  reference <- tempest:::tempest_product_report_reference(
    "# Report\n\nBody."
  )

  expect_identical(report, "# Fixture\n\nBody.\n")
  expect_identical(
    reference,
    list(
      report_id = "report_md",
      sha256 = paste0(
        "sha256:",
        "00341683b8128b3f18a2335210553e5afc1c6600187a2dd93e4fbc9c539d04a7"
      )
    )
  )
  expect_no_error(tempest:::tempest_product_report_reference_validate(
    reference,
    "# Report\n\nBody."
  ))
})

test_that("product report references reject content substitution", {
  reference <- tempest:::tempest_product_report_reference("Original")

  expect_error(
    tempest:::tempest_product_report_reference_validate(
      reference,
      "Substituted"
    ),
    class = "tempest_product_report_error"
  )
})
