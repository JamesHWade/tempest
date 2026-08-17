test_that("retrieval tools expose canonical research vocabulary", {
  skip_if_not_installed("ellmer")
  store <- tempest_research_workspace()
  store$upsert_retrieved_resource(fake_source("https://example.org/1"))
  source_id <- store$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  tools <- tempest:::tempest_tools_retrieval(retriever)
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  expect_contains(
    tool_names,
    c(
      "web_search",
      "fetch_url",
      "get_retrieved_source",
      "list_retrieved_sources",
      "list_proposed_claims",
      "add_proposed_claim",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )

  added <- by_name("add_proposed_claim")(
    claim_text = "Claim tools store source-backed claims.",
    source_ids = source_id,
    confidence = "high"
  )
  expect_contains(
    names(added),
    c(
      "claim_id",
      "claim_text",
      "source_ids",
      "confidence",
      "verification_status",
      "created_at"
    )
  )
  expect_equal(added$claim_text, "Claim tools store source-backed claims.")
  expect_equal(added$source_ids, source_id)
  expect_equal(added$confidence, "high")
  expect_equal(added$verification_status, "unverified")
  expect_equal(added$support_score, NA_real_)

  claims <- by_name("list_proposed_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)

  claim <- by_name("get_proposed_claim")(added$claim_id)
  expect_equal(claim$claim_id, added$claim_id)
  expect_equal(claim$retrieved_resources[[1]]$source_id, source_id)
  expect_equal(by_name("get_proposed_claim")("Cmissing"), NULL)

  span <- tempest:::tempest_evidence_span(
    source_id = source_id,
    quote = "Photosynthesis",
    relevance_score = 0.8
  )
  span_id <- store$add_evidence_span(span)
  store$link_evidence_to_proposed_claim(added$claim_id, span_id)
  evidence <- by_name("get_evidence_for_proposed_claim")(added$claim_id)
  expect_equal(evidence$claim$claim_id, added$claim_id)
  expect_equal(evidence$evidence_spans[[1]]$quote, "Photosynthesis")
  expect_equal(evidence$cited_sources[[1]]$source_id, source_id)
  expect_equal(by_name("get_evidence_for_proposed_claim")("Cmissing"), NULL)

  expect_length(by_name("list_unsupported_proposed_claims")(), 0)
  store$verify_proposed_claim(
    added$claim_id,
    status = "unsupported",
    score = 0.2
  )
  unsupported <- by_name("list_unsupported_proposed_claims")()
  expect_length(unsupported, 1)
  expect_equal(unsupported[[1]]$claim_id, added$claim_id)

  expect_equal(
    intersect(
      tool_names,
      c(
        "get_source",
        "list_sources",
        "list_claims",
        "add_claim",
        "get_claim",
        "get_evidence_for_claim",
        "list_unsupported_claims",
        "list_facts",
        "add_fact"
      )
    ),
    character()
  )
})

test_that("source-management tools expose claim tools without web tools", {
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  tools <- tempest:::tempest_tools_source_management(retriever)
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  expect_contains(
    tool_names,
    c(
      "get_retrieved_source",
      "list_retrieved_sources",
      "list_proposed_claims",
      "add_proposed_claim",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )
  expect_equal(intersect(tool_names, c("web_search", "fetch_url")), character())

  added <- by_name("add_proposed_claim")(
    claim_text = "Native source management stores claims.",
    source_ids = source_id
  )
  expect_equal(added$claim_text, "Native source management stores claims.")
  expect_equal(added$source_ids, source_id)

  claims <- by_name("list_proposed_claims")()
  expect_length(claims, 1)
  expect_equal(claims[[1]]$claim_id, added$claim_id)
})

test_that("claim write tools record dynamic provenance", {
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  current <- list(
    session_id = "expert-session-1",
    expert_id = "expert.climate",
    retrieval_step_id = "tool-turn-1"
  )
  tools <- tempest:::tempest_tools_source_management(
    retriever,
    claim_provenance = function() current
  )
  tool_names <- vapply(tools, function(tool) tool@name, character(1))
  by_name <- function(name) tools[[match(name, tool_names)]]

  added <- by_name("add_proposed_claim")(
    claim_text = "Dynamic provenance is recorded.",
    source_ids = source_id
  )

  expect_equal(added$session_id, "expert-session-1")
  expect_equal(added$expert_id, "expert.climate")
  expect_equal(added$retrieval_step_id, "tool-turn-1")
  claim <- store$list_proposed_claims()[[1]]
  expect_equal(claim@session_id, "expert-session-1")
  expect_equal(claim@expert_id, "expert.climate")
  expect_equal(claim@retrieval_step_id, "tool-turn-1")
})

test_that("source-management tools can be registered read-only", {
  skip_if_not_installed("ellmer")
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = fake_store_with_sources(1)
  )
  tools <- tempest:::tempest_tools_source_management(
    retriever,
    allow_claim_writes = FALSE
  )
  tool_names <- vapply(tools, function(tool) tool@name, character(1))

  expect_contains(
    tool_names,
    c(
      "get_retrieved_source",
      "list_retrieved_sources",
      "list_proposed_claims",
      "get_proposed_claim",
      "get_evidence_for_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )
  expect_equal(
    intersect(tool_names, c("add_proposed_claim", "add_fact")),
    character()
  )
})

test_that("default tool registration respects read-only evidence roles", {
  skip_if_not_installed("ellmer")
  store <- fake_store_with_sources(1)
  retriever <- tempest_retriever(
    config = tempest_config(cache_dir = withr::local_tempdir()),
    workspace = store
  )
  registered <- list()
  chat <- list(
    register_tools = function(tools) {
      registered[[length(registered) + 1L]] <<- tools
      invisible(NULL)
    }
  )

  tempest:::tempest_register_default_tools(
    chat,
    retriever,
    search_provider = "wikipedia",
    allow_claim_writes = FALSE
  )

  tool_names <- vapply(registered[[1]], function(tool) tool@name, character(1))
  expect_contains(
    tool_names,
    c(
      "web_search",
      "fetch_url",
      "get_proposed_claim",
      "list_unsupported_proposed_claims"
    )
  )
  expect_equal(
    intersect(tool_names, c("add_proposed_claim", "add_fact")),
    character()
  )
})

test_that("expert sessions resolve scoped capabilities before chat creation", {
  events <- character()
  registered <- list()
  prompts <- character()
  roles <- character()
  chat <- list(
    register_tools = function(tools) {
      events <<- c(events, "register")
      registered <<- tools
      invisible(NULL)
    }
  )
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) {
      events <<- c(events, "chat")
      roles <<- c(roles, role)
      prompts <<- c(prompts, system_prompt)
      chat
    }
  )
  capability <- tempest_capability_spec(
    "test.documents.read",
    purpose = "Read test documents",
    instructions = "Read only the test documents.",
    operation_id = "test.capability.documents.read",
    model_roles = "expert"
  )
  skill <- tempest_skill(
    "test.synthesize",
    purpose = "Synthesize test evidence",
    instructions = "Compare the available test evidence.",
    required_capability_ids = "test.documents.read"
  )
  runtime <- tempest_runtime(
    skill_specs = list(skill),
    capability_specs = list(capability),
    capability_implementations = list(
      "test.documents.read" = function(
        capability_spec,
        connections,
        context
      ) {
        events <<- c(events, "factory")
        list(
          tools = list("scoped-document-tool"),
          metadata = list(scope = "documents.read")
        )
      }
    ),
    include_builtins = FALSE
  )
  expert <- tempest_expert(
    expert_id = "expert.synthesis",
    name = "Synthesis expert",
    title = "Evidence synthesist",
    description = "Compares approved evidence.",
    instructions = "Preserve uncertainty.",
    skill_ids = "test.synthesize",
    model_role = "expert"
  )
  store <- tempest_research_workspace()
  retriever <- tempest_retriever(config = config, workspace = store)
  manager <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever
  )

  session <- manager$get_or_create("expert.synthesis")

  expect_identical(manager$workspace, retriever$workspace)
  expect_equal(events, c("factory", "chat", "register"))
  expect_equal(roles, "expert")
  expect_equal(registered, list("scoped-document-tool"))
  expect_match(prompts, "Preserve uncertainty", fixed = TRUE)
  expect_match(prompts, "Compare the available test evidence", fixed = TRUE)
  expect_match(session$session_id, "^expert-session_[a-f0-9]{16}$")
  expect_identical(session$is_new, TRUE)
  expect_equal(
    session$grants[["test.documents.read"]]$status,
    "granted"
  )
  expect_equal(session$profile$expert_id, "expert.synthesis")
  expect_equal(session$profile$expert_version, expert@version)
  expect_equal(
    session$profile$expert_fingerprint,
    tempest:::tempest_expert_profile_fingerprint(expert)
  )

  resumed <- manager$get_or_create("expert.synthesis")
  expect_identical(resumed$is_new, FALSE)
  expect_equal(resumed$session_id, session$session_id)
  expect_equal(events, c("factory", "chat", "register"))
})

test_that("expert sessions validate skills and capabilities before chat", {
  chat_calls <- 0L
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) {
      chat_calls <<- chat_calls + 1L
      list(register_tools = function(...) invisible(NULL))
    }
  )
  expert <- tempest_expert(
    expert_id = "expert.invalid-skill",
    name = "Invalid skill expert",
    title = "Tester",
    description = "Declares an unavailable skill.",
    instructions = "Test validation.",
    skill_ids = "missing.skill",
    model_role = "expert"
  )
  runtime <- tempest_runtime(include_builtins = FALSE)
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )
  manager <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever
  )

  expect_error(
    manager$get_or_create("expert.invalid-skill"),
    class = "tempest_expert_session_error"
  )
  expect_equal(chat_calls, 0L)
  expect_length(manager$list_sessions(), 0L)
})

test_that("expert connection grants are exact and runtime-only", {
  connection_calls <- 0L
  chat_calls <- 0L
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) {
      chat_calls <<- chat_calls + 1L
      list(register_tools = function(...) invisible(NULL))
    }
  )
  reference <- tempest_connection_ref(
    "customer.documents",
    provider_id = "test.host",
    connection_type = "documents",
    title = "Customer documents",
    description = "Approved customer documents"
  )
  capability <- tempest_capability_spec(
    "test.customer.search",
    purpose = "Search customer documents",
    instructions = "Search the granted customer documents.",
    operation_id = "test.capability.customer.search",
    connection_ref_ids = "customer.documents",
    model_roles = "expert"
  )
  runtime <- tempest_runtime(
    capability_specs = list(capability),
    capability_implementations = list(
      "test.customer.search" = function(
        capability_spec,
        connections,
        context
      ) {
        expect_named(connections, "customer.documents")
        list(tools = list())
      }
    ),
    connection_refs = list(reference),
    connection_bindings = list(
      "customer.documents" = function(connection_ref, context) {
        connection_calls <<- connection_calls + 1L
        list(client = "runtime-only")
      }
    ),
    include_builtins = FALSE
  )
  expert <- tempest_expert(
    expert_id = "expert.customer",
    name = "Customer expert",
    title = "Customer researcher",
    description = "Uses approved customer evidence.",
    instructions = "Respect customer scope.",
    required_capability_ids = "test.customer.search",
    model_role = "expert"
  )
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )
  denied <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever
  )

  expect_error(
    denied$get_or_create("expert.customer"),
    class = "tempest_expert_session_error"
  )
  expect_equal(connection_calls, 0L)
  expect_equal(chat_calls, 0L)

  allowed <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever,
    allowed_connection_ref_ids = list(
      "expert.customer" = "customer.documents"
    )
  )
  session <- allowed$get_or_create("expert.customer")
  expect_equal(connection_calls, 1L)
  expect_equal(chat_calls, 1L)
  expect_equal(
    session$profile$allowed_connection_ref_ids,
    "customer.documents"
  )
  expect_identical(
    grepl(
      "runtime-only",
      tempest:::tempest_canonical_json(session$profile)
    ),
    FALSE
  )
})

test_that("expert sessions bind resumes and reject retired profiles", {
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) {
      list(register_tools = function(...) invisible(NULL))
    }
  )
  first <- tempest_expert(
    expert_id = "expert.first",
    name = "First expert",
    title = "First",
    description = "First perspective.",
    instructions = "Act as the first expert.",
    model_role = "expert"
  )
  second <- tempest_expert(
    expert_id = "expert.second",
    name = "Second expert",
    title = "Second",
    description = "Second perspective.",
    instructions = "Act as the second expert.",
    model_role = "expert"
  )
  runtime <- tempest_runtime(include_builtins = FALSE)
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )
  manager <- tempest:::tempest_expert_session_manager(
    experts = list(first, second),
    runtime = runtime,
    config = config,
    retriever = retriever
  )
  session <- manager$get_or_create("expert.first")

  expect_error(
    manager$get_or_create(
      "expert.second",
      session_id = session$session_id
    ),
    class = "tempest_expert_session_error"
  )
  expect_error(
    manager$get_or_create(
      "expert.first",
      session_id = "expert-session_0000000000000000"
    ),
    class = "tempest_expert_session_error"
  )
  expect_identical(manager$retire_expert("expert.first"), TRUE)
  expect_length(manager$list_sessions(), 0L)
  expect_error(
    manager$profile("expert.first"),
    class = "tempest_expert_session_error"
  )
  expect_equal(
    manager$profile("expert.first", active_only = FALSE)@state,
    "retired"
  )
  expect_error(
    manager$get_or_create("expert.first"),
    class = "tempest_expert_session_error"
  )

  third <- tempest_expert(
    expert_id = "expert.third",
    name = "Third expert",
    title = "Third",
    description = "Third perspective.",
    instructions = "Act as the third expert.",
    model_role = "expert"
  )
  manager$add_expert(third)
  expect_equal(manager$profile("expert.third")@name, "Third expert")
  expect_equal(
    purrr::map_chr(manager$list_experts(), \(expert) expert@expert_id),
    c("expert.second", "expert.third")
  )
})

test_that("saved expert sessions reauthorize under exact bindings", {
  capability_calls <- 0L
  chat_calls <- 0L
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) {
      chat_calls <<- chat_calls + 1L
      list(register_tools = function(...) invisible(NULL))
    }
  )
  capability <- tempest_capability_spec(
    "test.restore",
    purpose = "Test restored authorization",
    instructions = "Authorize each fresh chat.",
    operation_id = "test.capability.restore",
    model_roles = "expert"
  )
  runtime <- tempest_runtime(
    capability_specs = list(capability),
    capability_implementations = list(
      "test.restore" = function(capability_spec, connections, context) {
        capability_calls <<- capability_calls + 1L
        list(tools = list(), metadata = list(call = capability_calls))
      }
    ),
    include_builtins = FALSE
  )
  expert <- tempest_expert(
    expert_id = "expert.restore",
    name = "Restore expert",
    title = "Restore tester",
    description = "Tests restored sessions.",
    instructions = "Reauthorize every restored session.",
    required_capability_ids = "test.restore",
    model_role = "expert"
  )
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )
  first_manager <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever
  )
  first <- first_manager$get_or_create("expert.restore")
  binding <- tempest:::tempest_expert_sessions_snapshot(list(
    expert_session_manager = first_manager
  ))[[1]]

  second_manager <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever
  )
  restored <- second_manager$restore_session(binding)

  expect_equal(restored$session_id, first$session_id)
  expect_identical(restored$is_new, TRUE)
  expect_equal(capability_calls, 2L)
  expect_equal(chat_calls, 2L)
  expect_equal(restored$profile$prior_grants, binding$grants)
  expect_equal(
    restored$profile$grants[["test.restore"]]$metadata$call,
    2L
  )
  expect_error(
    second_manager$restore_session(binding),
    class = "tempest_expert_session_error"
  )

  tampered <- binding
  tampered$session_id <- "expert-session_0000000000000000"
  tampered$expert_fingerprint <- paste(rep("0", 64L), collapse = "")
  expect_error(
    second_manager$restore_session(tampered),
    class = "tempest_expert_session_error"
  )
  expect_equal(capability_calls, 2L)
  expect_equal(chat_calls, 2L)
})

test_that("one delegation tool resolves the live roster by exact expert id", {
  skip_if_not_installed("ellmer")
  expert_chat <- fake_chat(text = list("First answer.", "Second answer."))
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) expert_chat
  )
  expert <- tempest_expert(
    expert_id = "expert.policy",
    name = "Policy expert",
    title = "Policy analyst",
    description = "Analyzes policy.",
    instructions = "Compare policy mechanisms.",
    model_role = "expert"
  )
  runtime <- tempest_runtime(include_builtins = FALSE)
  retriever <- tempest_retriever(
    config = config,
    workspace = tempest_research_workspace()
  )
  manager <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = runtime,
    config = config,
    retriever = retriever
  )
  tool <- tempest:::tempest_create_expert_delegation_tool(
    session_manager = manager,
    topic = "Policy outcomes",
    experts = list(expert)
  )

  expect_equal(tool@name, "delegate_to_expert")
  expect_match(tool@description, "expert.policy", fixed = TRUE)
  expect_match(tool@description, "Policy expert", fixed = TRUE)
  expect_named(
    tool@arguments@properties,
    c("expert_id", "question")
  )
  first <- tool(
    expert_id = "expert.policy",
    question = "What matters?"
  )
  first_prompt <- expert_chat$.calls()[[1]]$prompt
  expect_match(
    first_prompt,
    "evidence already in the shared session",
    fixed = TRUE
  )
  expect_match(first_prompt, "exactly one web search", fixed = TRUE)
  expect_match(first_prompt, "no more than two search results", fixed = TRUE)
  expect_match(first_prompt, "no more than 250 words", fixed = TRUE)
  expect_match(
    tool@arguments@properties$question@description,
    "One narrow, answerable evidence question",
    fixed = TRUE
  )
  second <- tool(
    expert_id = "expert.policy",
    question = "What else?"
  )
  expect_equal(first$expert_id, "expert.policy")
  expect_equal(first$response, "First answer.")
  expect_named(
    first,
    c(
      "expert_id",
      "expert",
      "response",
      "session_id",
      "source_ids",
      "claim_ids"
    )
  )
  expect_equal(second$response, "Second answer.")
  expect_equal(second$session_id, first$session_id)
  expect_length(manager$list_sessions(), 1L)
  expect_error(
    tool(expert_id = "Expert.Policy", question = "Wrong case"),
    class = "tempest_expert_session_error"
  )
  manager$retire_expert("expert.policy")
  expect_error(
    tool(expert_id = "expert.policy", question = "Retired"),
    class = "tempest_expert_session_error"
  )
  expect_identical(
    exists(
      "tempest_create_expert_tool",
      envir = asNamespace("tempest"),
      inherits = FALSE
    ),
    FALSE
  )
})

test_that("expert delegation returns and commits its native evidence", {
  skip_if_not_installed("ellmer")
  claim_text <- "Native expert evidence supports the answer."
  url <- "https://example.org/delegated-native-source"
  source_id <- tempest:::tempest_source_id(url)
  turn <- native_openai_json_turn(
    claim_text = claim_text,
    url = url,
    title = "Delegated native source"
  )
  expert_chat <- list(
    chat = function(...) claim_text,
    last_turn = function() turn
  )
  extractor <- list(
    chat_structured = function(prompt, ...) {
      expect_match(prompt, source_id, fixed = TRUE)
      list(
        facts = list(list(
          claim = claim_text,
          sources = list(list(source_id = source_id)),
          confidence = "high"
        ))
      )
    }
  )
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) expert_chat
  )
  expert <- tempest_expert(
    expert_id = "expert.native",
    name = "Native expert",
    title = "Evidence analyst",
    description = "Analyzes native citations.",
    instructions = "Use inspected evidence.",
    model_role = "expert"
  )
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    research_run_id = "native-evidence-manager",
    mode = "costorm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  extract_claims_program <- tempest:::tempest_bind_program_set(
    program_set,
    manifest
  )$extract_claims
  store <- tempest_research_workspace()
  manager <- tempest:::tempest_expert_session_manager(
    experts = list(expert),
    runtime = tempest_runtime(include_builtins = FALSE),
    config = config,
    retriever = tempest_retriever(config = config, workspace = store),
    extractor = extractor,
    extract_claims_program = extract_claims_program,
    workspace = store,
    run_id = "native-evidence-manager"
  )
  tool <- tempest:::tempest_create_expert_delegation_tool(
    session_manager = manager,
    topic = "Native evidence",
    experts = list(expert)
  )

  result <- withCallingHandlers(
    tool(
      expert_id = "expert.native",
      question = "What does the inspected evidence show?"
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(manager$extract_claims_program, extract_claims_program)
  expect_equal(result$source_ids, source_id)
  expect_length(result$claim_ids, 1L)
  expect_equal(
    store$get_retrieved_source(source_id)$title,
    "Delegated native source"
  )
  expect_equal(store$list_proposed_claims()[[1]]@claim_text, claim_text)
})

test_that("single expert generation returns deterministic scoped profiles", {
  skip_if_not_installed("ellmer")
  generated <- list(
    personas = list(list(
      name = "Dr. Rivera",
      title = "Battery policy analyst",
      affiliation = "Independent",
      background = "Studies battery policy.",
      focus_areas = list("recycling", "incentives"),
      perspective = "Policy and market incentives",
      initial_questions = list("Which incentives affect recycling?")
    ))
  )
  chat <- fake_chat(structured = list(generated, generated))
  config <- tempest_config(
    models = list(
      coordinator = "test/coordinator",
      expert = "test/expert"
    ),
    chat_fn = function(role, model, system_prompt, echo) chat
  )

  first <- tempest:::tempest_generate_single_expert(
    "Battery circularity",
    "Policy analysis",
    list(),
    config,
    module = test_program_executions(
      config,
      "tools-single-expert"
    )$personas
  )
  second <- tempest:::tempest_generate_single_expert(
    "Battery circularity",
    "Policy analysis",
    list(),
    config,
    module = test_program_executions(
      config,
      "tools-single-expert"
    )$personas
  )

  expect_identical(
    S7::S7_inherits(first, tempest:::TempestExpertProfile),
    TRUE
  )
  expect_equal(first@expert_id, second@expert_id)
  expect_match(first@expert_id, "^expert.generated-")
  expect_equal(
    first@required_capability_ids,
    c(
      "tempest.research.web",
      "tempest.evidence.read",
      "tempest.evidence.write"
    )
  )
  expect_equal(
    first@optional_capability_ids,
    "tempest.retrieval.semantic"
  )
  expect_equal(first@model_role, "expert")
})
