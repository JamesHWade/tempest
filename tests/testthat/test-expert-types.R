test_that("expert profiles preserve the fixed scientific product shape", {
  expert <- tempest_expert(
    expert_id = "expert.policy",
    name = "Dr. Rivera",
    title = "Policy analyst",
    description = "Analyzes policy and market incentives.",
    instructions = "Use verified evidence and preserve uncertainty.",
    version = "2026.1",
    focus_areas = c("policy", "markets"),
    model_role = "expert",
    selection_metadata = list(regions = list("US", "CA")),
    initial_work_items = "Map active policies",
    initial_questions = "Which incentives affect adoption?"
  )

  expect_identical(S7::S7_inherits(expert, TempestExpertProfile), TRUE)
  expect_equal(expert@expert_id, "expert.policy")
  expect_identical(expert@skill_ids, character())
  expect_identical(expert@skill_versions, character())
  expect_identical(expert@required_capability_ids, character())
  expect_identical(expert@optional_capability_ids, character())
  expect_identical(expert@model_role, "expert")
  expect_identical(expert@model_policy_ref, NA_character_)
  expect_equal(expert@state, "active")
})

test_that("expert profiles reject generic resolver semantics", {
  arguments <- list(
    skill_ids = list(skill_ids = "evidence-synthesis"),
    skill_versions = list(
      skill_ids = "evidence-synthesis",
      skill_versions = c("evidence-synthesis" = "2")
    ),
    required_capability_ids = list(
      required_capability_ids = "evidence.search"
    ),
    optional_capability_ids = list(
      optional_capability_ids = "evidence.fetch"
    ),
    model_policy_ref = list(model_policy_ref = "policy.expert"),
    model_role = list(model_role = "moderator")
  )

  for (argument in arguments) {
    expect_error(
      do.call(
        tempest_expert,
        c(
          list(
            expert_id = "expert.fixed",
            name = "Fixed expert",
            title = "Researcher",
            description = "Reviews scientific evidence.",
            instructions = "Preserve uncertainty."
          ),
          argument
        )
      ),
      class = "tempest_research_expert_error"
    )
  }

  expect_error(
    tempest_expert(
      expert_id = "expert.schema",
      name = "Schema expert",
      title = "Researcher",
      description = "Reviews scientific evidence.",
      instructions = "Preserve uncertainty.",
      schema_version = 1
    ),
    class = "tempest_research_expert_error"
  )
})

test_that("expert constructors reject invalid and executable definitions", {
  expect_error(
    tempest_expert(
      expert_id = "sk-live-secret",
      name = "Unsafe Expert",
      title = "Unsafe",
      description = "Should never be persisted.",
      instructions = "Do not run."
    ),
    class = "tempest_research_expert_error"
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
    class = "tempest_research_expert_error"
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
    class = "tempest_research_expert_error"
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
    class = "tempest_research_expert_error"
  )
  benign@metadata$note <- "Safe metadata"
  benign@name <- "sk-proj-0123456789abcdefghijklmnopqrstuv"
  expect_error(
    tempest:::tempest_expert_profile_record(benign),
    class = "tempest_research_expert_error"
  )
  expect_error(
    tempest_expert(
      expert_id = "bad id",
      name = "Expert",
      title = "Title",
      description = "Description",
      instructions = "Instructions"
    ),
    class = "tempest_research_expert_error"
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
    class = "tempest_research_expert_error"
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
      class = "tempest_research_expert_error"
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
    class = "tempest_research_expert_error"
  )
})

test_that("session snapshots reject normalized credential metadata", {
  skip_if_not_installed("ellmer")
  sensitive_name <- "api&#95;key"
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Sensitive metadata",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.sensitive-session"))
  )
  snapshot <- tempest_session_snapshot(session)
  expect_identical(session$experts[[1L]]@metadata, list())

  metadata_values <- list(
    stats::setNames(list("hunter2hunter2"), sensitive_name),
    list(note = paste0("AIza", strrep("A", 35L)))
  )
  for (metadata in metadata_values) {
    candidate <- snapshot
    candidate$experts[[1L]]$metadata <- metadata
    expect_error(
      tempest_session_restore(candidate, config = cfg),
      class = "tempest_session_restore_error",
      regexp = "credential-like"
    )
  }
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

test_that("expert records round-trip with stable fingerprints", {
  expert_record <- function(record) {
    jsonlite::fromJSON(
      jsonlite::toJSON(
        record,
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      ),
      simplifyVector = FALSE
    )
  }
  expert <- tempest_expert(
    expert_id = "expert.one",
    name = "Expert",
    title = "Analyst",
    description = "Analyzes evidence.",
    instructions = "Be precise."
  )
  restored_expert <- tempest_expert_profile_from_data(
    expert_record(tempest_expert_profile_record(expert))
  )

  expect_equal(restored_expert@expert_id, expert@expert_id)
  expect_match(
    tempest_expert_profile_record(expert)$fingerprint,
    "^[a-f0-9]{64}$"
  )
})

test_that("expert profile rows restore only the exact current product shape", {
  expert <- tempest_expert(
    expert_id = "expert.current",
    name = "Current expert",
    title = "Researcher",
    description = "Reviews scientific evidence.",
    instructions = "Preserve uncertainty."
  )
  record <- tempest_expert_profile_record(expert)

  expect_identical(
    names(record),
    c(S7::prop_names(expert), "fingerprint")
  )
  expect_identical(record$schema_version, 1L)
  expect_identical(record$skill_ids, list())
  expect_identical(record$required_capability_ids, list())

  missing <- record
  missing$skill_ids <- NULL
  expect_error(
    tempest_expert_profile_from_data(missing),
    class = "tempest_research_expert_error"
  )

  wrong_schema <- record
  wrong_schema$schema_version <- 1
  wrong_schema$fingerprint <- tempest_expert_profile_fingerprint(
    wrong_schema
  )
  expect_error(
    tempest_expert_profile_from_data(wrong_schema),
    class = "tempest_research_expert_error"
  )

  generic <- record
  generic$required_capability_ids <- list("evidence.search")
  generic$fingerprint <- tempest_expert_profile_fingerprint(generic)
  expect_error(
    tempest_expert_profile_from_data(generic),
    class = "tempest_research_expert_error"
  )
})
