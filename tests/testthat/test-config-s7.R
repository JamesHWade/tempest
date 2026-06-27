test_that("citation policy fields validate", {
  cfg <- tempest_config()
  expect_equal(cfg@citation_policy, "source_attributed")
  expect_equal(cfg@min_support_score, 0.7)
  expect_equal(cfg@on_unsupported_claim, "flag")

  expect_error(tempest_config(citation_policy = "nope"), "citation_policy")
  expect_error(tempest_config(on_unsupported_claim = "nope"), "on_unsupported_claim")
  expect_error(tempest_config(min_support_score = 2), "\\[0, 1\\]")
})

test_that("config remains usable by make_chat", {
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    list(role = role, model = model, mock = TRUE, register_tools = function(...) NULL)
  })
  chat <- tempest_make_chat(cfg, "coordinator")
  expect_true(chat$mock)
})
