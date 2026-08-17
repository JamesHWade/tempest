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
      expert_id = "sk-live-secret",
      name = "Unsafe Expert",
      title = "Unsafe",
      description = "Should never be persisted.",
      instructions = "Do not run."
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_expert(
      expert_id = "expert.unsafe-metadata",
      name = "Unsafe Expert",
      title = "Unsafe",
      description = "Should never be persisted.",
      instructions = "Do not run.",
      metadata = list(
        note = "sk-proj-0123456789abcdefghijklmnopqrstuv"
      )
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_expert(
      expert_id = "expert.unsafe-google-key",
      name = "Unsafe Expert",
      title = "Unsafe",
      description = "Should never be persisted.",
      instructions = "Do not run.",
      metadata = list(note = paste0("AIza", strrep("A", 35L)))
    ),
    class = "tempest_workflow_spec_error"
  )
  expect_error(
    tempest_connection_ref(
      "https://service-token@example.org/private",
      provider_id = "provider.safe",
      connection_type = "search",
      title = "Unsafe connection"
    ),
    class = "tempest_workflow_spec_error"
  )
  benign <- tempest_expert(
    expert_id = "expert.safe-metadata",
    name = "Safe Expert",
    title = "Safe",
    description = "Keeps ordinary prose.",
    instructions = "Document uncertainty.",
    metadata = list(
      note = "A secret ballot is documented.",
      max_tokens = 128L
    )
  )
  expect_identical(benign@metadata$max_tokens, 128L)
  benign@metadata$note <- "sk-proj-0123456789abcdefghijklmnopqrstuv"
  expect_error(
    tempest:::tempest_expert_profile_record(benign),
    class = "tempest_workflow_spec_error"
  )
  benign@metadata$note <- "Safe metadata"
  benign@name <- "sk-proj-0123456789abcdefghijklmnopqrstuv"
  expect_error(
    tempest:::tempest_expert_profile_record(benign),
    class = "tempest_workflow_spec_error"
  )
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

test_that("credential-like metadata names fail after display normalization", {
  sensitive_names <- c(
    "ａｐｉ＿ｋｅｙ",
    "api&#95;key",
    "api&lowbar;key"
  )

  for (sensitive_name in sensitive_names) {
    metadata <- stats::setNames(list("hunter2hunter2"), sensitive_name)
    expect_error(
      tempest_expert(
        expert_id = "expert.sensitive-name",
        name = "Sensitive Name",
        title = "Researcher",
        description = "Tests portable metadata.",
        instructions = "Inspect evidence.",
        metadata = metadata
      ),
      class = "tempest_workflow_spec_error"
    )
  }

  expert <- tempest_expert(
    expert_id = "expert.sensitive-name",
    name = "Sensitive Name",
    title = "Researcher",
    description = "Tests portable metadata.",
    instructions = "Inspect evidence."
  )
  expert@metadata <- stats::setNames(
    list("hunter2hunter2"),
    sensitive_names[[1L]]
  )
  expect_error(
    tempest:::tempest_expert_profile_record(expert),
    class = "tempest_workflow_spec_error"
  )
})

test_that("session snapshots reject normalized credential metadata", {
  skip_if_not_installed("ellmer")
  sensitive_name <- "api&#95;key"
  session <- tempest_session(
    "Sensitive metadata name",
    config = tempest_config(
      chat_fn = function(role, model, system_prompt, echo) fake_chat()
    ),
    experts = list(test_expert(expert_id = "expert.sensitive-session"))
  )
  session$experts[[1L]]@metadata <- stats::setNames(
    list("hunter2hunter2"),
    sensitive_name
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  session <- tempest_session(
    "Sensitive metadata value",
    config = tempest_config(
      chat_fn = function(role, model, system_prompt, echo) fake_chat()
    ),
    experts = list(test_expert(expert_id = "expert.sensitive-session"))
  )
  session$experts[[1L]]@metadata <- list(
    note = paste0("AIza", strrep("A", 35L))
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )
})

test_that("credential detection preserves scientific SK identifiers", {
  scientific_ids <- c(
    "SK-BR-3 cells",
    "SK-N-SH neuroblastoma",
    "SK-MEL-28 melanoma",
    "SK-OV-3 cells",
    "SK-MEL-28-derived-resistant-subline",
    "SK\\-BR\\-3 cells",
    "SK\\-MEL\\-28 melanoma",
    "SK&#45;BR&#45;3 cells",
    "SK-**BR**-3 cells",
    paste0("SK-", intToUtf8(0x200B), "BR-3 cells"),
    paste0(
      "https://example.org/redirect?next=",
      "https%3A%2F%2Fexample.net%2Fpath"
    ),
    "https://example.org/search?token_count=12&signature_method=sha256",
    "sklearn",
    "secretome"
  )
  credentials <- c(
    "sk-proj-0123456789abcdefghijklmnopqrstuv",
    "sk-0123456789abcdefghijklmnopqrstuv",
    paste0(
      "sk-ant-api03-",
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    ),
    paste0(
      "sk-or-v1-",
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    ),
    paste0("sk", "_live_51Abcdefghijklmnopqrstuvwxyz0123456789"),
    paste0("AIza", strrep("A", 35L)),
    paste0("ASIA", strrep("A", 16L)),
    paste0("glpat-", strrep("a", 20L)),
    paste0("hf_", strrep("a", 34L)),
    paste0("npm_", strrep("a", 36L)),
    paste0("SG.", strrep("a", 22L), ".", strrep("b", 43L)),
    "sk-live-secret",
    "sk\\-proj\\-abcdefghijklmnopqrstuvwxyz1234567890",
    "sk&#45;proj&#45;abcdefghijklmnopqrstuvwxyz1234567890",
    "sk&#x2d;proj&#x2d;abcdefghijklmnopqrstuvwxyz1234567890",
    "sk&lowbar;live&lowbar;abcd1234",
    "eyJabcd&period;efghijkl&period;sigvalue",
    "https&colon;&sol;&sol;user&colon;pass&commat;example.org",
    "sk-**proj**-abcdefghijklmnopqrstuvwxyz1234567890",
    "sk<!-- -->-proj-abcdefghijklmnopqrstuvwxyz1234567890",
    paste0(
      "sk-",
      intToUtf8(0x200B),
      "proj-abcdefghijklmnopqrstuvwxyz1234567890"
    ),
    paste0(
      "sk-",
      intToUtf8(0x2060),
      "proj-abcdefghijklmnopqrstuvwxyz1234567890"
    ),
    paste0(
      "sk-",
      intToUtf8(0xFEFF),
      "proj-abcdefghijklmnopqrstuvwxyz1234567890"
    ),
    "https://alice:supersecret@example.org/private",
    "postgresql://service:p%40ssword@example.org/research",
    "https://abcdefghijklmnopqrstuvwxyz0123456789@example.org/private",
    "https://alice%3Asupersecret@example.org/private",
    "https://alice:@example.org/private",
    "https://:supersecret@example.org/private",
    "postgresql://service@example.org/research",
    paste0(
      "https://example.org/evidence?api%5Fkey=",
      "sk%2Dproj%2Dabcdefghijklmnopqrstuvwxyz1234567890"
    ),
    "https://example.org/download?token=abcdefghijklmnopqrstuvwxyz1234567890",
    "https://blob.example.org/object?sig=abcdefghijklmnopqrstuvwxyz1234567890",
    paste0(
      "https://example.org/object?X-Amz-Security-Token=",
      "abcdefghijklmnopqrstuvwxyz1234567890"
    ),
    paste0(
      "https://example.org/object?key=",
      "AIzaSyDUMMYEXAMPLE01234567890123456"
    ),
    paste0(
      "https://example.org/object?X-Goog-Signature=",
      strrep("a", 64L)
    ),
    paste0(
      "https://example.org/object?X-Amz-Credential=",
      "ASIAEXAMPLE%2F20260816%2Fus-east-1%2Fs3%2Faws4_request"
    ),
    "Authorization: Bearer sk-live-secret",
    "Authorization: Basic YWxpY2U6c2VjcmV0",
    "Proxy-Authorization: Basic YWxpY2U6c2VjcmV0",
    "Cookie: sessionid=abc123secret",
    "Set-Cookie: sessionid=abc123secret; Secure",
    "-----BEGIN OPENSSH PRIVATE KEY-----",
    "-----BEGIN RSA PRIVATE KEY-----",
    "AWS_SECRET_ACCESS_KEY=abcdefghijklmnopqrstuvwxyz"
  )

  expect_identical(
    unname(vapply(
      scientific_ids,
      tempest:::tempest_contract_sensitive_scalar,
      logical(1)
    )),
    rep(FALSE, length(scientific_ids))
  )
  expect_identical(
    unname(vapply(
      credentials,
      tempest:::tempest_contract_sensitive_scalar,
      logical(1)
    )),
    rep(TRUE, length(credentials))
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
