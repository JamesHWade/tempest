# tests/testthat/helper-mocks.R
# Reusable fakes so ledger/verification logic is testable without network or API keys.

tempest_mock_provider <- function(name = "mock", model = "fake") {
  arguments <- list(
    name = name,
    model = model,
    base_url = "https://example.invalid",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL
  )
  do.call(
    ellmer::Provider,
    arguments[names(arguments) %in% names(formals(ellmer::Provider))]
  )
}

# A deterministic Chat-compatible fake. Direct structured and text calls retain
# their original queue behavior, while the stream and state methods satisfy the
# ellmer Chat boundary used by Deputy.
fake_chat <- function(
  structured = list(),
  text = list(),
  provider_turns = list()
) {
  state <- new.env(parent = emptyenv())
  state$structured <- structured
  state$text <- text
  state$provider_turns <- provider_turns
  state$calls <- list()
  state$turns <- list()
  state$tools <- list()
  state$system_prompt <- NULL
  state$provider <- tempest_mock_provider()
  state$on_tool_request <- function(request) invisible(request)
  state$on_tool_result <- function(result) invisible(result)
  state$tokens <- data.frame(
    input = numeric(),
    output = numeric(),
    cached_input = numeric(),
    cost = numeric()
  )

  resolve <- function(value, prompt) {
    if (is.function(value)) {
      value <- value(prompt)
    }
    if (inherits(value, "condition")) {
      stop(value)
    }
    value
  }

  pop <- function(queue, prompt, empty) {
    values <- state[[queue]]
    if (length(values) == 0L) {
      return(empty)
    }
    value <- values[[1L]]
    state[[queue]] <- values[-1L]
    resolve(value, prompt)
  }

  record_call <- function(kind, prompt, transport) {
    state$calls <- c(
      state$calls,
      list(list(kind = kind, prompt = prompt, transport = transport))
    )
  }

  record_turn <- function(prompt, response) {
    if (!is.null(prompt)) {
      state$turns <- c(
        state$turns,
        list(ellmer::UserTurn(list(ellmer::ContentText(prompt))))
      )
    }
    response <- paste(as.character(response), collapse = "")
    provider_turn <- pop("provider_turns", prompt, NULL)
    if (is.null(provider_turn)) {
      provider_turn <- ellmer::AssistantTurn(
        list(ellmer::ContentText(response)),
        tokens = c(0, 0, 0),
        cost = 0
      )
    }
    state$turns <- c(state$turns, list(provider_turn))
    state$tokens <- rbind(
      state$tokens,
      data.frame(input = 0, output = 0, cached_input = 0, cost = 0)
    )
    invisible(response)
  }

  tool_name <- function(tool, fallback = NULL) {
    name <- tryCatch(tool@name, error = function(error) NULL)
    name %||% fallback
  }

  register_tools <- function(tools) {
    if (length(tools) == 0L) {
      return(invisible(NULL))
    }
    supplied_names <- names(tools) %||% rep("", length(tools))
    for (index in seq_along(tools)) {
      fallback <- supplied_names[[index]]
      if (!nzchar(fallback)) {
        fallback <- NULL
      }
      name <- tool_name(tools[[index]], fallback)
      if (is.null(name) || !nzchar(name)) {
        stop("fake_chat: tool has no name")
      }
      state$tools[[name]] <- tools[[index]]
    }
    invisible(NULL)
  }

  stream_generator <- function(prompt, async = FALSE) {
    response <- pop("text", prompt, "")
    chunks <- as.character(response)
    if (async) {
      return(coro::async_generator(function() {
        for (chunk in chunks) {
          coro::yield(ellmer::ContentText(chunk))
        }
        record_turn(prompt, response)
        coro::exhausted()
      })())
    }
    coro::generator(function() {
      for (chunk in chunks) {
        coro::yield(ellmer::ContentText(chunk))
      }
      record_turn(prompt, response)
      coro::exhausted()
    })()
  }

  chat <- NULL
  chat <- structure(
    list(
      chat_structured = function(prompt, type = NULL, ...) {
        record_call("structured", prompt, "chat_structured")
        if (length(state$structured) == 0) {
          stop("fake_chat: structured queue exhausted")
        }
        pop("structured", prompt, NULL)
      },
      chat_structured_async = function(prompt, type = NULL, ...) {
        promises::promise_resolve(chat$chat_structured(prompt, type, ...))
      },
      chat = function(prompt, ...) {
        record_call("text", prompt, "chat")
        response <- pop("text", prompt, "")
        record_turn(prompt, response)
        response
      },
      chat_async = function(prompt, ...) {
        promises::promise_resolve(chat$chat(prompt, ...))
      },
      stream = function(
        prompt = NULL,
        stream = c("text", "content"),
        controller = NULL
      ) {
        record_call("text", prompt, "stream")
        stream_generator(prompt)
      },
      stream_async = function(
        prompt = NULL,
        stream = c("text", "content"),
        controller = NULL
      ) {
        record_call("text", prompt, "stream_async")
        stream_generator(prompt, async = TRUE)
      },
      get_turns = function() state$turns,
      set_turns = function(turns) {
        state$turns <- turns
        invisible(NULL)
      },
      get_system_prompt = function() state$system_prompt,
      set_system_prompt = function(prompt) {
        state$system_prompt <- prompt
        invisible(NULL)
      },
      get_tools = function() state$tools,
      set_tools = function(tools) {
        state$tools <- list()
        register_tools(tools)
      },
      register_tool = function(tool) {
        register_tools(list(tool))
      },
      register_tools = register_tools,
      get_tokens = function() state$tokens,
      get_provider = function() state$provider,
      get_model = function() "fake",
      last_turn = function(role = "assistant") {
        role_class <- switch(
          role,
          assistant = "ellmer::AssistantTurn",
          user = "ellmer::UserTurn",
          system = "ellmer::SystemTurn",
          NULL
        )
        turns <- state$turns
        if (!is.null(role_class)) {
          turns <- Filter(\(turn) inherits(turn, role_class), turns)
        }
        if (length(turns) == 0L) {
          return(NULL)
        }
        tail(turns, 1L)[[1L]]
      },
      on_tool_request = function(callback) {
        state$on_tool_request <- callback
        invisible(NULL)
      },
      on_tool_result = function(callback) {
        state$on_tool_result <- callback
        invisible(NULL)
      },
      clone = function() chat,
      .calls = function() state$calls,
      .callbacks = function() {
        list(
          on_tool_request = state$on_tool_request,
          on_tool_result = state$on_tool_result
        )
      },
      .queues = function() {
        list(
          structured = state$structured,
          text = state$text,
          provider_turns = state$provider_turns
        )
      }
    ),
    class = c("Chat", "list")
  )
  chat
}

# A fake judge that returns a fixed verification verdict for every claim.
fake_judge <- function(
  status = "supported",
  score = 0.9,
  rationale = "matches source"
) {
  fake_chat(structured = list()) # placeholder; verdict queue set per-test via fake_verdicts()
}

# Build a verdict queue (one per claim-span pair) for verification tests.
fake_verdicts <- function(...) {
  verdicts <- list(...)
  fake_chat(structured = verdicts)
}

# Fixture builders.
fake_source <- function(
  url = "https://example.org/a",
  title = "Example A",
  content_text = "Photosynthesis converts light to chemical energy."
) {
  tempest_resource(
    resource_kind = "web",
    locator = url,
    title = title,
    media_type = "text/html",
    content = content_text,
    retrieved_at = "2026-01-01T00:00:00Z"
  )
}

fake_store_with_sources <- function(n = 2) {
  store <- test_research_workspace()
  for (i in seq_len(n)) {
    store$upsert_retrieved_resource(fake_source(
      url = paste0("https://example.org/", i),
      title = paste("Example", i),
      content_text = paste("Body text for source", i)
    ))
  }
  store
}

fake_stage_inputs <- function(stage) {
  switch(
    stage,
    perspectives = list(
      topic = "Topic",
      seed_context = "Seed context",
      n_experts = 1L
    ),
    personas = list(
      topic = "Topic",
      n_experts = 1L,
      requirements = "Distinct experts"
    ),
    query_decomposition = list(question = "Question", topic = "Topic"),
    extract_claims = list(
      answer_text = "Source-backed answer",
      source_context = "",
      source_ids = "",
      citation_mode = "tempest_inline"
    ),
    verify_claim_support = list(
      claim_text = "A supported claim",
      source_excerpts = "Source excerpt"
    ),
    next_question = list(
      topic = "Topic",
      perspective = "Perspective",
      answered = "",
      facts = ""
    ),
    draft_outline = list(topic = "Topic", report_title = "Report"),
    refined_outline = list(
      topic = "Topic",
      report_title = "Outline",
      draft_outline = "Draft outline",
      facts = "Verified facts"
    ),
    section_writing = list(
      section_title = "Evidence",
      section_summary = "",
      subsections = "",
      facts = "Verified facts"
    ),
    lead_section = list(
      topic = "Topic",
      title = "Report",
      article_body = "",
      facts = "Verified facts"
    )
  )
}

fake_verify_claim_supports <- function(
  workspace,
  claims,
  min_support_score = 0.7
) {
  requested <- stats::setNames(
    claims,
    vapply(claims, \(claim) claim@claim_id, character(1))
  )
  for (claim in workspace$list_proposed_claims()) {
    represented <- vapply(
      workspace$get_evidence_for_proposed_claim(claim@claim_id),
      \(span) span@source_id,
      character(1)
    )
    for (source_id in setdiff(claim@source_ids, represented)) {
      source <- workspace$get_retrieved_source(source_id)
      span_id <- workspace$add_evidence_span(tempest_evidence_span(
        source_id = source_id,
        quote = source$content_text,
        extracted_by = "test::extractor"
      ))
      workspace$link_evidence_to_proposed_claim(claim@claim_id, span_id)
    }
  }
  supports <- unlist(
    lapply(
      workspace$list_proposed_claims(),
      function(claim) {
        requested_claim <- requested[[claim@claim_id]] %||% claim
        status <- requested_claim@verification_status
        score <- requested_claim@support_score
        if (identical(status, "unverified")) {
          status <- "unverifiable"
          score <- NA_real_
        }
        lapply(
          workspace$get_evidence_for_proposed_claim(claim@claim_id),
          \(span) {
            tempest_claim_support(
              claim_id = claim@claim_id,
              evidence_span_id = span@evidence_span_id,
              source_id = span@source_id,
              verification_status = status,
              support_score = score,
              rationale = "Supported by exact source evidence."
            )
          }
        )
      }
    ),
    recursive = FALSE
  )
  workspace$verify_proposed_claims_batch(
    supports,
    verified_at = "2026-08-16T12:03:00Z",
    min_support_score = min_support_score,
    verifier = "test::verifier"
  )
  lapply(names(requested), workspace$get_proposed_claim)
}

test_program_executions <- function(
  config = tempest_config(),
  run_id = "test-program-execution"
) {
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    run_id,
    mode = "storm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  tempest:::tempest_bind_program_set(program_set, manifest)
}

test_program_set_from_program <- function(
  program,
  registry = list(),
  .local_envir = parent.frame()
) {
  stages <- tempest:::tempest_program_set_stages()
  programs <- stats::setNames(rep(list(program), length(stages)), stages)
  root <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    "program-set"
  )
  tempest_program_set(
    programs = programs,
    path = root,
    registry = registry
  )
}

test_program_reference <- function(
  stage,
  program_artifact_id = NULL,
  governed_procedure_ref = NULL
) {
  program_artifact_id <- program_artifact_id %||%
    paste0(
      "sha256:",
      digest::digest(
        paste0("test-program:", stage),
        algo = "sha256",
        serialize = FALSE
      )
    )
  list(
    stage = stage,
    contract_version = 1L,
    program_artifact_id = program_artifact_id,
    artifact_reference = list(
      type = "builtin",
      id = paste0("tempest::", stage)
    ),
    governed_procedure_ref = governed_procedure_ref,
    evaluator_id = paste0("tempest::evaluator/", stage),
    evaluator_version = "1"
  )
}
