test_that("the public condition contract has exactly six catchable names", {
  expect_identical(
    tempest:::tempest_public_condition_categories(),
    c(
      "tempest_input_error",
      "tempest_execution_error",
      "tempest_persistence_error",
      "tempest_authority_error",
      "tempest_cancelled"
    )
  )
})

test_that("every Tempest condition carries exactly one public category", {
  categories <- tempest:::tempest_public_condition_categories()
  internal <- c(
    "tempest_config_error",
    "tempest_run_restore_error",
    "tempest_product_authority_error",
    "tempest_stage_execution_error",
    "tempest_run_cancelled",
    "tempest_session_snapshot_error",
    "tempest_promotion_error",
    "tempest_knowledge_error"
  )
  for (class in internal) {
    resolved <- tempest:::tempest_condition_class(class)
    expect_identical(
      length(intersect(resolved, categories)),
      1L,
      info = class
    )
    expect_identical(
      resolved[[length(resolved)]],
      "tempest_error",
      info = class
    )
    expect_identical(resolved[[1L]], class, info = class)
  }
})

test_that("condition categories map each family to its exact meaning", {
  expect_identical(
    tempest:::tempest_public_condition_category("tempest_config_error"),
    "tempest_input_error"
  )
  expect_identical(
    tempest:::tempest_public_condition_category("tempest_run_restore_error"),
    "tempest_persistence_error"
  )
  expect_identical(
    tempest:::tempest_public_condition_category(
      "tempest_product_authority_error"
    ),
    "tempest_authority_error"
  )
  expect_identical(
    tempest:::tempest_public_condition_category("tempest_run_cancelled"),
    "tempest_cancelled"
  )
  expect_identical(
    tempest:::tempest_public_condition_category("tempest_chat_error"),
    "tempest_execution_error"
  )
})

test_that("an explicit category is preserved rather than re-inferred", {
  resolved <- tempest:::tempest_condition_class(
    c("tempest_knowledge_error", "tempest_input_error")
  )

  expect_identical(
    resolved,
    c(
      "tempest_knowledge_error",
      "tempest_input_error",
      "tempest_error"
    )
  )
})

test_that("public entry points fail with one exact catchable category", {
  categories <- tempest:::tempest_public_condition_categories()
  cases <- list(
    config = function() tempest_config(models = 42),
    expert = function() tempest_expert(name = 1),
    knowledge = function() tempest_knowledge(list(not = "a view")),
    run = function() tempest_run("Topic", knowledge = "raw"),
    session = function() tempest_session("Topic", knowledge = "raw"),
    report = function() tempest_report(list()),
    sources = function() tempest_sources(list()),
    claims = function() tempest_claims(list()),
    supports = function() tempest_claim_supports(list())
  )
  for (name in names(cases)) {
    condition <- tryCatch(cases[[name]](), error = identity)
    expect_s3_class(condition, "tempest_error")
    expect_identical(
      length(intersect(class(condition), categories)),
      1L,
      info = name
    )
    expect_identical(
      length(intersect(class(condition), "tempest_input_error")),
      1L,
      info = name
    )
  }
})

test_that("public conditions retain the original parent without secrets", {
  parent <- rlang::error_cnd(
    "provider_failure",
    message = "upstream provider refused the request"
  )
  condition <- tryCatch(
    tempest:::tempest_knowledge_abort(
      "Could not resolve the accepted Graft record.",
      parent = parent
    ),
    error = identity
  )

  expect_s3_class(condition, "tempest_error")
  expect_s3_class(condition, "tempest_knowledge_error")
  expect_s3_class(condition$parent, "provider_failure")
  expect_identical(
    conditionMessage(condition$parent),
    "upstream provider refused the request"
  )
  expect_identical(
    tempest:::tempest_contract_sensitive_scalar(conditionMessage(condition)),
    FALSE
  )
})
