test_that("expert profiles declare durable skills and scoped capabilities", {
  expert <- tempest_expert(
    expert_id = "expert.policy",
    name = "Dr. Rivera",
    title = "Policy analyst",
    description = "Analyzes policy and market incentives.",
    instructions = "Use verified evidence and preserve uncertainty.",
    version = "2026.1",
    focus_areas = c("policy", "markets"),
    skill_ids = "evidence-synthesis",
    skill_versions = c("evidence-synthesis" = "2"),
    required_capability_ids = "evidence.search",
    optional_capability_ids = "evidence.fetch",
    model_role = NA_character_,
    model_policy_ref = "policy.expert",
    selection_metadata = list(regions = list("US", "CA")),
    initial_work_items = "Map active policies",
    initial_questions = "Which incentives affect adoption?"
  )

  expect_identical(S7::S7_inherits(expert, TempestExpertProfile), TRUE)
  expect_equal(expert@expert_id, "expert.policy")
  expect_equal(expert@skill_ids, "evidence-synthesis")
  expect_equal(expert@skill_versions[["evidence-synthesis"]], "2")
  expect_equal(expert@model_policy_ref, "policy.expert")
  expect_equal(expert@state, "active")
})

test_that("skills keep procedures separate from runtime operations", {
  skill <- tempest_skill(
    "evidence-synthesis",
    purpose = "Synthesize verified evidence",
    instructions = "Compare sources and preserve disagreements.",
    version = "2",
    input_schema = list(type = "object"),
    output_schema = list(type = "object"),
    required_capability_ids = c("evidence.search", "evidence.read"),
    operation_ids = "tempest.skill.synthesize",
    operation_versions = c("tempest.skill.synthesize" = "3"),
    metadata = list(owner = "research")
  )

  expect_identical(S7::S7_inherits(skill, TempestSkill), TRUE)
  expect_equal(
    skill@required_capability_ids,
    c(
      "evidence.search",
      "evidence.read"
    )
  )
  expect_equal(
    skill@operation_versions[["tempest.skill.synthesize"]],
    "3"
  )
})

test_that("capabilities reference operations and opaque connections", {
  capability <- tempest_capability_spec(
    "evidence.search",
    purpose = "Find approved evidence",
    instructions = "Use only granted read-only connections.",
    operation_id = "tempest.capability.search",
    operation_version = "2",
    connection_ref_ids = "connection.knowledge-base",
    model_roles = c("expert", "coordinator"),
    input_schema = list(query = list(type = "string")),
    output_schema = list(results = list(type = "array")),
    side_effecting = FALSE
  )

  expect_identical(
    S7::S7_inherits(capability, TempestCapabilitySpec),
    TRUE
  )
  expect_equal(capability@operation_id, "tempest.capability.search")
  expect_equal(
    capability@connection_ref_ids,
    "connection.knowledge-base"
  )
  expect_identical(capability@side_effecting, FALSE)
})

test_that("connection references contain non-secret durable metadata", {
  connection <- tempest_connection_ref(
    "connection.knowledge-base",
    provider_id = "host.connections",
    connection_type = "document-search",
    title = "Approved knowledge base",
    description = "Read-only customer documentation",
    scopes = c("documents.read", "metadata.read"),
    state = "retired",
    metadata = list(region = "us-east")
  )

  expect_identical(
    S7::S7_inherits(connection, TempestConnectionRef),
    TRUE
  )
  expect_equal(connection@provider_id, "host.connections")
  expect_equal(connection@scopes, c("documents.read", "metadata.read"))
  expect_equal(connection@state, "retired")
})

test_that("contract constructors reject invalid and executable definitions", {
  expect_error(
    tempest_expert(
      expert_id = "bad id",
      name = "Expert",
      title = "Title",
      description = "Description",
      instructions = "Instructions"
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_expert(
      expert_id = "expert.one",
      name = "Expert",
      title = "Title",
      description = "Description",
      instructions = "Instructions",
      required_capability_ids = "search",
      optional_capability_ids = "search"
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_skill(
      "skill.one",
      purpose = "Purpose",
      instructions = "Instructions",
      operation_ids = "operation.one",
      operation_versions = c(other = "1")
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_capability_spec(
      "capability.one",
      purpose = "Purpose",
      instructions = "Instructions",
      operation_id = "operation.one",
      input_schema = list(callback = function() NULL)
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_connection_ref(
      "connection.one",
      provider_id = "host",
      connection_type = "documents",
      title = "Documents",
      description = "Approved documents",
      metadata = list(api_key = "not-allowed")
    ),
    class = "tempest_workflow_spec_error"
  )
})

test_that("contract records round-trip with stable fingerprints", {
  canonical_record <- function(record) {
    jsonlite::fromJSON(
      tempest_canonical_json(record),
      simplifyVector = FALSE
    )
  }
  expert <- tempest_expert(
    expert_id = "expert.one",
    name = "Expert",
    title = "Analyst",
    description = "Analyzes evidence.",
    instructions = "Be precise.",
    skill_ids = "skill.one",
    required_capability_ids = "capability.one"
  )
  skill <- tempest_skill(
    "skill.one",
    purpose = "Analyze evidence",
    instructions = "Compare claims.",
    required_capability_ids = "capability.one"
  )
  capability <- tempest_capability_spec(
    "capability.one",
    purpose = "Read evidence",
    instructions = "Use approved sources.",
    operation_id = "operation.read",
    connection_ref_ids = "connection.one"
  )
  connection <- tempest_connection_ref(
    "connection.one",
    provider_id = "host",
    connection_type = "documents",
    title = "Documents",
    description = "Approved documents"
  )

  restored_expert <- tempest_expert_profile_from_data(
    canonical_record(tempest_expert_profile_record(expert))
  )
  restored_skill <- tempest_skill_from_data(
    canonical_record(tempest_skill_record(skill))
  )
  restored_capability <- tempest_capability_spec_from_data(
    canonical_record(tempest_capability_spec_record(capability))
  )
  restored_connection <- tempest_connection_ref_from_data(
    canonical_record(tempest_connection_ref_record(connection))
  )

  expect_equal(restored_expert@expert_id, expert@expert_id)
  expect_equal(restored_skill@skill_id, skill@skill_id)
  expect_equal(
    restored_capability@capability_id,
    capability@capability_id
  )
  expect_equal(
    restored_connection@connection_id,
    connection@connection_id
  )
  expect_match(
    tempest_expert_profile_record(expert)$fingerprint,
    "^[a-f0-9]{64}$"
  )
})

test_that("contract restoration rejects tampering and unknown schemas", {
  skill <- tempest_skill(
    "skill.one",
    purpose = "Analyze evidence",
    instructions = "Compare claims."
  )
  record <- tempest_skill_record(skill)
  record$instructions <- "Tampered"

  expect_error(
    tempest_skill_from_data(record),
    class = "tempest_artifact_codec_error"
  )

  record <- tempest_skill_record(skill)
  record$schema_version <- 2L
  expect_error(
    tempest_skill_from_data(record),
    class = "tempest_artifact_codec_error"
  )

  record <- tempest_skill_record(skill)
  record$fingerprint <- NULL
  expect_error(
    tempest_skill_from_data(record),
    class = "tempest_artifact_codec_error"
  )
})
