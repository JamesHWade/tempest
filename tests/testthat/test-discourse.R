test_that("tempest_type_turn_policy returns valid type", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_turn_policy()
  expect_s7_class(type, getFromNamespace("TypeObject", "ellmer"))
})

test_that("generic discourse manager construction fails closed", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  error <- tryCatch(tempest:::DiscourseManager$new(cfg), error = identity)
  expect_s3_class(error, "tempest_session_error")
})

test_that("TempestSession has no generic discourse manager surface", {
  skip_if_not_installed("ellmer")

  experts <- list(test_expert(
    expert_id = "expert.alice",
    name = "Dr. Alice",
    title = "Scientist",
    description = "Technical"
  ))

  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Test topic",
    config = cfg,
    experts = experts
  )

  expect_identical("discourse_manager" %in% names(session), FALSE)
})

test_that("execute_turn_decision rejects automatic routing", {
  skip_if_not_installed("ellmer")

  experts <- list(test_expert(
    expert_id = "expert.alice",
    name = "Dr. Alice",
    title = "Scientist",
    description = "Technical"
  ))

  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Test topic",
    config = cfg,
    experts = experts
  )

  decision <- list(action = "end_round", instruction = "", rationale = "test")
  error <- tryCatch(session$execute_turn_decision(decision), error = identity)
  expect_s3_class(error, "tempest_session_error")
})
