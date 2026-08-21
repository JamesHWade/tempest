test_that("TempestSession retires experts by exact stable id", {
  skip_if_not_installed("ellmer")
  alice <- test_expert(
    expert_id = "expert.alice",
    name = "Dr. Alice Smith",
    title = "Scientist",
    description = "Technical analysis"
  )
  bob <- test_expert(
    expert_id = "expert.bob",
    name = "Prof. Bob Jones",
    title = "Ethicist",
    description = "Ethical analysis"
  )
  experts <- list(alice, bob)
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session("Test topic", config = cfg, experts = experts)

  expect_identical(session$retire_expert(alice@expert_id), TRUE)
  expect_identical(session$experts, experts)
  expect_identical(
    tempest:::tempest_session_retired_expert_ids(session),
    alice@expert_id
  )
  expect_identical(session$retire_expert("Dr. Alice Smith"), FALSE)
  expect_identical(session$retire_expert("expert.unknown"), FALSE)
})

test_that("TempestSession get_active_experts filters retired profiles", {
  skip_if_not_installed("ellmer")
  alice <- test_expert(expert_id = "expert.alice", name = "Dr. Alice Smith")
  bob <- test_expert(expert_id = "expert.bob", name = "Prof. Bob Jones")
  carol <- test_expert(expert_id = "expert.carol", name = "Dr. Carol Lee")
  experts <- list(alice, bob, carol)
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session("Test topic", config = cfg, experts = experts)

  expect_length(session$get_active_experts(), 3)
  session$retire_expert(bob@expert_id)
  active <- session$get_active_experts()

  expect_length(active, 2)
  expect_setequal(
    vapply(active, \(expert) expert@expert_id, character(1)),
    c(alice@expert_id, carol@expert_id)
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

  expert <- withCallingHandlers(
    tempest:::tempest_generate_single_expert(
      "Climate change",
      "Policy analysis",
      list(test_expert(
        expert_id = "expert.existing",
        name = "Dr. Alice"
      )),
      cfg,
      module = test_program_executions(
        cfg,
        "dynamic-roster-single"
      )$personas
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )

  expect_s7_class(expert, tempest:::TempestExpertProfile)
  expect_equal(expert@name, "Dr. Policy")
  expect_match(expert@expert_id, "^expert::")
  expect_match(expert@version, "^sha256-")
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
    ses$add_turn(paste0("User ", i), "user", paste("turn", i))
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

test_that("dynamic personas append one exact stage attempt", {
  skip_if_not_installed("ellmer")
  generated <- list(
    personas = list(list(
      name = "Dr. Dynamic",
      title = "Policy analyst",
      affiliation = "Independent",
      background = "Studies adaptive policy panels.",
      focus_areas = list("policy"),
      perspective = "Policy adaptation",
      initial_questions = list("Which policy gap remains?")
    ))
  )
  cfg <- tempest_config(
    max_active_experts = 2L,
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(generated))
    }
  )
  session <- tempest_session(
    "Dynamic stage records",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.existing-dynamic",
      name = "Existing expert"
    )),
    session_id = "dynamic-stage-records"
  )

  expert <- session$add_expert(
    "Policy analysis",
    name = "Dr. Renamed Dynamic"
  )
  records <- tempest:::tempest_session_stage_records(session)

  expect_s7_class(expert, tempest:::TempestExpertProfile)
  expect_identical(expert@name, "Dr. Renamed Dynamic")
  expect_length(session$experts, 2L)
  expect_length(records, 1L)
  expect_identical(records[[1]]@stage, "personas")
  expect_identical(records[[1]]@status, "succeeded")
  expect_identical(
    records[[1]]@program_artifact_id,
    session$manifest@programs$personas$program_artifact_id
  )
  expect_identical(
    records[[1]]@output_reference$content_digest,
    tempest:::tempest_stage_state_output_digest("personas", list(expert))
  )
  expect_no_error(tempest_session_snapshot(session))
})
