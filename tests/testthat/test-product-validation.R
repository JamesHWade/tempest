test_that("product validation reports product-owned conditions", {
  expect_error(
    tempest:::tempest_product_canonical_list(
      list(callback = function() NULL),
      "details"
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
