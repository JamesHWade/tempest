test_that("TempestSession retires experts by exact stable id", {
  skip_if_not_installed("ellmer")
  experts <- list(
    test_expert(
      expert_id = "expert.alice",
      name = "Dr. Alice Smith",
      title = "Scientist",
      description = "Technical analysis"
    ),
    test_expert(
      expert_id = "expert.bob",
      name = "Prof. Bob Jones",
      title = "Ethicist",
      description = "Ethical analysis"
    )
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session("Test topic", config = cfg, experts = experts)

  expect_identical(session$retire_expert("expert.alice"), TRUE)
  expect_equal(session$experts[[1]]@state, "retired")
  expect_equal(session$experts[[2]]@state, "active")
  expect_identical(session$retire_expert("Dr. Alice Smith"), FALSE)
  expect_identical(session$retire_expert("expert.unknown"), FALSE)
})

test_that("TempestSession get_active_experts filters retired profiles", {
  skip_if_not_installed("ellmer")
  experts <- list(
    test_expert(expert_id = "expert.alice", name = "Dr. Alice Smith"),
    test_expert(expert_id = "expert.bob", name = "Prof. Bob Jones"),
    test_expert(expert_id = "expert.carol", name = "Dr. Carol Lee")
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session("Test topic", config = cfg, experts = experts)

  expect_length(session$get_active_experts(), 3)
  session$retire_expert("expert.bob")
  active <- session$get_active_experts()

  expect_length(active, 2)
  expect_setequal(
    vapply(active, \(expert) expert@expert_id, character(1)),
    c("expert.alice", "expert.carol")
  )
})

test_that("tempest_generate_single_expert returns an expert profile", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(
        structured = list(list(
          personas = list(list(
            name = "Dr. Policy",
            title = "Policy analyst",
            affiliation = "Independent",
            background = "Studies climate policy.",
            focus_areas = "Policy",
            perspective = "Policy analysis",
            initial_questions = "Which policies matter?"
          ))
        ))
      )
    }
  )

  expert <- tempest:::tempest_generate_single_expert(
    "Climate change",
    "Policy analysis",
    list(test_expert(
      expert_id = "expert.existing",
      name = "Dr. Alice"
    )),
    cfg
  )

  expect_s7_class(expert, tempest:::TempestExpertProfile)
  expect_equal(expert@name, "Dr. Policy")
  expect_equal(expert@model_role, "expert")
  expect_contains(
    expert@required_capability_ids,
    "tempest.evidence.read"
  )
})

test_that("transcript_markdown returns the most recent turns", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.alice",
      name = "Dr. Alice"
    ))
  )
  for (i in seq_len(5)) {
    ses$add_turn(paste0("S", i), "assistant", paste("turn", i))
  }

  md <- ses$transcript_markdown(max_turns = 2)

  expect_match(md, "turn 4")
  expect_match(md, "turn 5")
  expect_no_match(md, "turn 1")
})

test_that("add_expert returns NULL at the active-expert cap", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    max_active_experts = 1,
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.alice",
      name = "Dr. Alice"
    ))
  )

  result <- suppressWarnings(ses$add_expert(area = "New area"))
  expect_null(result)
})
