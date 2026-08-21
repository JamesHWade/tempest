test_that("tempest_type_personas returns the provider expert schema", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_personas()
  expect_s7_class(type, getFromNamespace("TypeObject", "ellmer"))
})

test_that("tempest_format_persona_details formats provider records", {
  record <- list(
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    affiliation = "Arctic Research Institute",
    background = "20 years studying polar ice dynamics.",
    focus_areas = c("Ice sheet modeling", "Sea level rise"),
    perspective = "Physical science perspective on climate change"
  )

  details <- tempest:::tempest_format_persona_details(record)

  expect_match(details, "Arctic Research Institute", fixed = TRUE)
  expect_match(details, "20 years", fixed = TRUE)
  expect_match(details, "Ice sheet modeling", fixed = TRUE)
  expect_match(details, "Physical science", fixed = TRUE)
})

test_that("tempest_render_expert_prompt accepts an expert profile", {
  expert <- tempest_expert(
    name = "Dr. Sarah Chen",
    title = "Climate Scientist",
    description = "Physical science perspective on climate change",
    instructions = "Compare observations with modeled uncertainty."
  )

  prompt <- tempest:::tempest_render_expert_prompt(expert)

  expect_match(prompt, "Dr. Sarah Chen", fixed = TRUE)
  expect_match(prompt, "Climate Scientist", fixed = TRUE)
  expect_identical(
    stringi::stri_count_fixed(
      prompt,
      "Physical science perspective on climate change"
    ),
    1L
  )
  expect_identical(
    stringi::stri_count_fixed(
      prompt,
      "Compare observations with modeled uncertainty."
    ),
    1L
  )
  expect_identical(tempest:::tempest_deputy_expert_prompt(expert), prompt)
})

test_that("tempest_render_expert_prompt rejects a missing profile", {
  expect_error(
    tempest:::tempest_render_expert_prompt(
      persona = NULL,
      expert_id = "expert.3"
    ),
    class = "tempest_config_error"
  )
})

test_that("TempestSession stores selected expert profiles", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  alice <- test_expert(
    expert_id = "expert.alice",
    name = "Dr. Alice Smith",
    title = "Computer Scientist"
  )
  bob <- test_expert(
    expert_id = "expert.bob",
    name = "Prof. Bob Jones",
    title = "Ethicist"
  )
  experts <- list(
    alice,
    bob
  )

  session <- tempest_session(
    topic = "AI in healthcare",
    config = cfg,
    experts = experts
  )

  expect_length(session$experts, 2)
  expect_equal(
    session$get_expert_names(),
    c(
      "Dr. Alice Smith",
      "Prof. Bob Jones"
    )
  )
  expect_equal(session$find_expert(alice@expert_id), 1)
  expect_null(session$find_expert("Dr. Alice Smith"))
  expect_r6_class(
    tempest:::tempest_session_expert_manager(session),
    "TempestDeputyExpertManager"
  )
})
