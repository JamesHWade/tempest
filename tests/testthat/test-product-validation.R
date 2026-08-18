test_that("product validation is detached from generic workflow helpers", {
  reject_generic <- function(...) {
    rlang::abort("generic helper reached", class = "test_generic_reached")
  }
  local_mocked_bindings(
    tempest_workflow_scalar = reject_generic,
    tempest_workflow_character = reject_generic,
    tempest_workflow_list = reject_generic,
    tempest_workflow_serializable_list = reject_generic,
    tempest_workflow_flag = reject_generic,
    tempest_canonical_json = reject_generic
  )

  result <- tempest_validation_result(
    "validator.product",
    status = "warning",
    message = "Review the evidence.",
    details = list(source_ids = list("S0123456789ab")),
    created_at = "2026-08-17 UTC"
  )

  expect_identical(S7::S7_inherits(result, TempestValidationResult), TRUE)
  expect_identical(result@validator_id, "validator.product")
  expect_identical(result@status, "warning")
  expect_identical(result@details$source_ids, list("S0123456789ab"))
})

test_that("product validation reports product-owned conditions", {
  expect_error(
    tempest_validation_result(
      "validator.product",
      details = list(callback = function() NULL)
    ),
    class = "tempest_product_validation_error"
  )
  expect_error(
    tempest:::tempest_product_flag(NA, "enabled"),
    class = "tempest_product_validation_error"
  )
})

test_that("product paths reject absolute and traversing bundle entries", {
  expect_identical(
    tempest:::tempest_product_path_is_safe("workspace.json"),
    TRUE
  )
  expect_identical(
    tempest:::tempest_product_path_is_safe("data/file.json"),
    TRUE
  )
  expect_identical(tempest:::tempest_product_path_is_safe("../secret"), FALSE)
  expect_identical(tempest:::tempest_product_path_is_safe("/tmp/secret"), FALSE)
  expect_identical(tempest:::tempest_product_path_is_safe("C:/secret"), FALSE)
  expect_identical(tempest:::tempest_product_path_is_safe("data//file"), FALSE)
})

test_that("product arrays and persisted lists reject coercion and secrets", {
  expect_identical(
    tempest:::tempest_product_character_array(
      list("source-a", "source-b"),
      "ids"
    ),
    c("source-a", "source-b")
  )
  expect_error(
    tempest:::tempest_product_character_array(list(id = "source-a"), "ids"),
    class = "tempest_product_validation_error"
  )
  expect_error(
    tempest:::tempest_product_character_array(1, "ids"),
    class = "tempest_product_validation_error"
  )
  expect_error(
    tempest:::tempest_product_serializable_list(
      list(api_key = "credential-value"),
      "snapshot"
    ),
    class = "tempest_product_validation_error"
  )
})
