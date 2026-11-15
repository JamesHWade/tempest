research_review_product_dispatch <- function(
  base_chat_fn,
  calls,
  reviewer_response,
  adjudicator_response
) {
  reviewer_prefix <- "You are an independent scientific claim reviewer."
  adjudicator_prefix <- "You are an independent scientific adjudicator."
  response_chat <- function(which, response) {
    fake_chat(
      structured = list(function(prompt) {
        calls[[paste0(which, "_prompts")]] <- c(
          calls[[paste0(which, "_prompts")]],
          prompt
        )
        if (is.function(response)) response(prompt) else response
      })
    )
  }

  function(role, model, system_prompt, echo) {
    is_reviewer <- identical(role, "judge") &&
      is.character(system_prompt) &&
      length(system_prompt) == 1L &&
      startsWith(system_prompt, reviewer_prefix)
    is_adjudicator <- identical(role, "judge") &&
      is.character(system_prompt) &&
      length(system_prompt) == 1L &&
      startsWith(system_prompt, adjudicator_prefix)
    if (is_reviewer) {
      calls$reviewer <- calls$reviewer + 1L
      return(response_chat("reviewer", reviewer_response))
    }
    if (is_adjudicator) {
      calls$adjudicator <- calls$adjudicator + 1L
      return(response_chat("adjudicator", adjudicator_response))
    }
    base_chat_fn(role, model, system_prompt, echo)
  }
}

research_review_product_config <- function(
  base_config,
  calls,
  reviewer_response,
  adjudicator_response,
  research_review = TRUE,
  repair_evidence = FALSE,
  max_review_claims = 1L,
  review_concurrency = 1L
) {
  tempest_config(
    models = base_config@models,
    params = base_config@params,
    chat_fn = research_review_product_dispatch(
      base_chat_fn = base_config@chat_fn,
      calls = calls,
      reviewer_response = reviewer_response,
      adjudicator_response = adjudicator_response
    ),
    embed_fn = base_config@embed_fn,
    ragnar_store = base_config@ragnar_store,
    search_provider = base_config@search_provider,
    cache_enabled = FALSE,
    max_search_results = base_config@max_search_results,
    max_search_queries_per_turn = base_config@max_search_queries_per_turn,
    retrieve_top_k = base_config@retrieve_top_k,
    max_sources = base_config@max_sources,
    user_agent = base_config@user_agent,
    max_active_experts = base_config@max_active_experts,
    citation_policy = base_config@citation_policy,
    min_support_score = base_config@min_support_score,
    on_unsupported_claim = base_config@on_unsupported_claim,
    research_review = research_review,
    repair_evidence = repair_evidence,
    max_review_claims = max_review_claims,
    review_concurrency = review_concurrency
  )
}

research_review_storm_fixture <- function(
  reviewer_response,
  adjudicator_response,
  research_review = TRUE,
  repair_evidence = FALSE,
  max_review_claims = 1L,
  .local_envir = parent.frame()
) {
  fixture <- storm_product_fixture(.local_envir = .local_envir)
  testthat::local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      stop("Unexpected URL fetch in scientific-review product fixture.")
    },
    .env = .local_envir
  )
  calls <- new.env(parent = emptyenv())
  calls$reviewer <- 0L
  calls$adjudicator <- 0L
  calls$reviewer_prompts <- character()
  calls$adjudicator_prompts <- character()
  fixture$config <- research_review_product_config(
    base_config = fixture$config,
    calls = calls,
    reviewer_response = reviewer_response,
    adjudicator_response = adjudicator_response,
    research_review = research_review,
    repair_evidence = repair_evidence,
    max_review_claims = max_review_claims
  )
  fixture$retriever <- tempest_retriever(
    config = fixture$config,
    workspace = fixture$store
  )
  fixture$review_calls <- calls
  fixture
}

research_review_costorm_fixture <- function(
  reviewer_response,
  adjudicator_response,
  research_review = TRUE,
  repair_evidence = FALSE,
  max_review_claims = 1L,
  session_id = "costorm-research-review",
  .local_envir = parent.frame()
) {
  testthat::local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      stop("Unexpected URL fetch in scientific-review product fixture.")
    },
    .env = .local_envir
  )
  previous_cache <- dsprrr::configure_cache(enable = FALSE)
  withr::defer(
    {
      if (is.null(previous_cache)) {
        dsprrr::configure_cache()
      } else {
        do.call(dsprrr::configure_cache, previous_cache)
      }
    },
    envir = .local_envir
  )
  source <- fake_source(
    url = "https://example.org/costorm-research-review",
    title = "Co-STORM review source",
    content_text = "Co-STORM preserves research evidence across dialogue."
  )
  source_id <- source@resource_id
  store <- test_research_workspace()
  store$upsert_retrieved_resource(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  mindmap <- list(
    nodes = list(list(
      id = "root",
      label = "Co-STORM review",
      notes = "Research evidence",
      source_ids = source_id
    )),
    edges = list()
  )
  runtime <- costorm_baseline_runtime(
    source_id = source_id,
    mindmap = mindmap,
    claim_texts = c(
      "Warmup research is preserved.",
      "Moderator research is preserved."
    ),
    moderator_answers = paste0(
      "Moderator research cites evidence [",
      source_id,
      "]."
    ),
    n_mindmap_updates = 2L
  )
  calls <- new.env(parent = emptyenv())
  calls$reviewer <- 0L
  calls$adjudicator <- 0L
  calls$reviewer_prompts <- character()
  calls$adjudicator_prompts <- character()
  config <- research_review_product_config(
    base_config = runtime$config,
    calls = calls,
    reviewer_response = reviewer_response,
    adjudicator_response = adjudicator_response,
    research_review = research_review,
    repair_evidence = repair_evidence,
    max_review_claims = max_review_claims
  )
  session <- tempest_session(
    "Co-STORM review",
    config = config,
    retriever = tempest_retriever(config = config, workspace = store),
    experts = list(tempest_expert(
      name = "Dr. Review",
      title = "Research analyst",
      description = "Scientific product behavior",
      instructions = "Preserve evidence and uncertainty.",
      initial_questions = "What evidence establishes the claim?"
    )),
    progress = collector$record,
    session_id = session_id
  )
  session$warmup(verbose = FALSE)
  session$step("What should we inspect?")
  questions <- session$suggest_questions(n = 1L)
  tempest_verify_claims(
    session,
    verifier = fake_chat(
      structured = rep(
        list(list(
          status = "supported",
          score = 0.95,
          rationale = "The captured excerpt supports the exact claim."
        )),
        length(tempest:::tempest_session_workspace(
          session
        )$list_evidence_spans())
      )
    )
  )
  session$.__enclos_env__$private$reorganize_mindmap()

  list(
    config = config,
    runtime = runtime,
    session = session,
    source = source,
    source_id = source_id,
    store = store,
    events = collector$data(),
    questions = questions,
    review_calls = calls
  )
}

research_review_run_storm <- function(
  fixture,
  run_id,
  progress = function(event) invisible(event)
) {
  tempest_run(
    "Progress events",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1L,
    max_questions_per_perspective = 1L,
    output_dir = withr::local_tempdir(.local_envir = parent.frame()),
    run_id = run_id,
    progress = progress,
    verbose = FALSE
  )
}
