test_that("expert profiles expose only authored and derived fields", {
  expert <- tempest_expert(
    name = "Dr. Rivera",
    title = "Policy analyst",
    description = "Analyzes policy and market incentives.",
    instructions = "Use verified evidence and preserve uncertainty.",
    focus_areas = c("policy", "markets"),
    initial_questions = "Which incentives affect adoption?"
  )

  expect_identical(S7::S7_class(expert), TempestExpertProfile)
  expect_identical(
    S7::prop_names(expert),
    c(
      "expert_id",
      "version",
      "name",
      "title",
      "description",
      "instructions",
      "focus_areas",
      "initial_questions",
      "schema_version"
    )
  )
  expect_match(expert@expert_id, "^expert::[a-f0-9]{64}$")
  expect_match(expert@version, "^sha256-[a-f0-9]{64}$")
  expect_identical(expert@schema_version, 2L)
})

test_that("expert identity and version derive from canonical authored content", {
  make_expert <- function(description = "Reviews scientific evidence.") {
    tempest_expert(
      name = "Fixed expert",
      title = "Researcher",
      description = description,
      instructions = "Preserve uncertainty.",
      focus_areas = c("evidence", "policy"),
      initial_questions = "What evidence is strongest?"
    )
  }
  first <- make_expert()
  same <- make_expert()
  changed <- make_expert("Reviews policy evidence.")

  expect_identical(first@expert_id, same@expert_id)
  expect_identical(first@version, same@version)
  expect_false(identical(first@expert_id, changed@expert_id))
  expect_false(identical(first@version, changed@version))
})

test_that("expert constructor has no generic SDK or schema controls", {
  removed <- c(
    "expert_id",
    "version",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "model_role",
    "model_policy_ref",
    "selection_metadata",
    "initial_work_items",
    "state",
    "metadata",
    "schema_version"
  )
  expect_identical(
    intersect(names(formals(tempest_expert)), removed),
    character()
  )
})

test_that("expert profiles reject secret material and live tampering", {
  expect_error(
    tempest_expert(
      name = "Unsafe expert",
      title = "Researcher",
      description = "sk-proj-0123456789abcdefghijklmnopqrstuv",
      instructions = "Do not run."
    ),
    class = "tempest_research_expert_error"
  )

  expert <- tempest_expert(
    name = "Safe expert",
    title = "Researcher",
    description = "Reviews evidence.",
    instructions = "Preserve uncertainty."
  )
  expert@name <- "Changed after construction"
  expect_error(
    tempest:::tempest_expert_profile_record(expert),
    class = "tempest_research_expert_error"
  )
})

test_that("expert profile rows restore only exact schema 2 records", {
  expert <- tempest_expert(
    name = "Current expert",
    title = "Researcher",
    description = "Reviews scientific evidence.",
    instructions = "Preserve uncertainty.",
    focus_areas = c("evidence", "policy")
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  restored <- tempest:::tempest_expert_profile_from_data(record)

  expect_identical(restored, expert)
  expect_identical(
    names(record),
    c(tempest:::tempest_research_expert_fields(), "fingerprint")
  )
  expect_identical(record$schema_version, 2L)
  expect_match(record$fingerprint, "^[a-f0-9]{64}$")

  missing <- record
  missing$focus_areas <- NULL
  expect_error(
    tempest:::tempest_expert_profile_from_data(missing),
    class = "tempest_research_expert_error"
  )

  wrong_schema <- record
  wrong_schema$schema_version <- 1L
  wrong_schema$fingerprint <- tempest:::tempest_product_record_hash(
    wrong_schema[-length(wrong_schema)]
  )
  expect_error(
    tempest:::tempest_expert_profile_from_data(wrong_schema),
    class = "tempest_research_expert_error"
  )
})
