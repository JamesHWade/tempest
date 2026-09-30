# Synthetic replay adapter for the demonstration. It overrides only documented
# Chat methods; deputy, dsprrr, and Tempest still execute their actual code.
# It deliberately makes no claims about live-model quality or latency.

TempestReplayChat <- R6::R6Class(
  "TempestReplayChat",
  inherit = ellmer::Chat,
  public = list(
    initialize = function(statement, source_id, system_prompt = NULL) {
      private$statement <- statement
      private$source_id <- source_id
      super$initialize(
        provider = ellmer::Provider(
          name = "tempest-replay",
          base_url = "https://example.invalid"
        ),
        model = ellmer::Model(name = "synthetic-replay"),
        system_prompt = system_prompt,
        echo = "none"
      )
    },
    chat_structured = function(..., type, echo = "none", convert = TRUE) {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      fields <- names(type@properties)
      statement <- private$statement
      source_id <- private$source_id
      outline <- list(
        title = "Synthetic polymer recovery pilot",
        sections = list(list(
          title = "Pilot recovery",
          summary = "Recovery measured in a synthetic pilot note.",
          subsections = list(list(
            title = "Recovery observation",
            bullets = "Inspect the exact source quote before accepting.",
            needed = "Verified pilot recovery"
          ))
        ))
      )
      value <- if ("perspectives" %in% fields) {
        list(
          title = outline$title,
          perspectives = list(list(
            name = "Recovery evidence",
            description = "Separate measured recovery from wider claims.",
            key_questions = "What recovery does the pilot note report?"
          ))
        )
      } else if ("queries" %in% fields) {
        list(queries = "synthetic polymer recovery pilot")
      } else if ("facts" %in% fields) {
        list(
          facts = list(list(
            claim = statement,
            sources = list(list(source_id = source_id, quote = statement)),
            confidence = "high"
          ))
        )
      } else if ("status" %in% fields) {
        list(
          status = "supported",
          score = 0.95,
          rationale = "The scripted claim matches the exact synthetic source quote."
        )
      } else if ("sections" %in% fields) {
        outline
      } else if ("items" %in% fields) {
        match <- regmatches(prompt, regexec("claim_id: ([^)]+)", prompt))[[1L]]
        if (length(match) != 2L) {
          stop("The replay received no grounded claim identity.")
        }
        list(
          items = list(list(
            kind = "observation",
            text = statement,
            claim_ids = list(match[[2L]])
          ))
        )
      } else {
        stop(
          "The replay has no response for structured fields: ",
          paste(fields, collapse = ", ")
        )
      }
      private$record(
        prompt,
        ellmer::ContentText(as.character(jsonlite::toJSON(
          value,
          auto_unbox = TRUE
        )))
      )
      value
    },
    chat = function(..., echo = "none") {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      response <- private$answer()
      private$record(prompt, ellmer::ContentText(response))
      response
    },
    chat_async = function(..., echo = "none") {
      promises::promise_resolve(self$chat(..., echo = echo))
    },
    chat_structured_async = function(..., type, echo = "none", convert = TRUE) {
      promises::promise_resolve(self$chat_structured(
        ...,
        type = type,
        echo = echo,
        convert = convert
      ))
    },
    stream = function(..., stream = c("text", "content"), controller = NULL) {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      response <- private$answer()
      coro::generator(function() {
        coro::yield(ellmer::ContentText(response))
        private$record(prompt, ellmer::ContentText(response))
      })()
    },
    stream_async = function(
      ...,
      stream = c("text", "content"),
      controller = NULL
    ) {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      response <- private$answer()
      coro::async_generator(function() {
        coro::yield(ellmer::ContentText(response))
        private$record(prompt, ellmer::ContentText(response))
      })()
    }
  ),
  private = list(
    statement = NULL,
    source_id = NULL,
    answer = function() {
      paste0(private$statement, " [", private$source_id, "].")
    },
    record = function(prompt, content) {
      self$add_turn(
        ellmer::UserTurn(list(ellmer::ContentText(prompt))),
        ellmer::AssistantTurn(
          list(content),
          tokens = c(0, 0, 0),
          cost = 0,
          finish_reason = "stop"
        ),
        log_tokens = FALSE
      )
    }
  )
)

tempest_demo_inputs <- function(correction = FALSE) {
  statement <- if (correction) {
    "The synthetic pilot recovered 74% of the input polymer."
  } else {
    "The synthetic pilot recovered 82% of the input polymer."
  }
  url <- paste0(
    "https://example.org/synthetic-pilot-",
    if (correction) "correction" else "initial"
  )
  resource <- tempest::tempest_resource(
    resource_kind = "web",
    locator = url,
    title = if (correction) {
      "Synthetic corrected pilot note"
    } else {
      "Synthetic pilot note"
    },
    media_type = "text/plain",
    content = paste(
      statement,
      "This is invented demonstration evidence, not an experimental result."
    ),
    retrieved_at = "2026-09-29T00:00:00Z"
  )
  config <- tempest::tempest_config(
    cache_enabled = FALSE,
    search_provider = "wikipedia",
    max_search_results = 1L,
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) {
      TempestReplayChat$new(statement, resource@resource_id, system_prompt)
    }
  )
  expert <- tempest::tempest_expert(
    name = "Pilot reviewer",
    title = "Synthetic recovery analyst",
    description = "Reviews the exact recovery reported in the synthetic note.",
    instructions = "Report only the supplied source-backed observation."
  )
  workspace <- tempest::tempest_research_workspace()
  workspace$upsert_retrieved_resource(resource)
  retriever <- list(
    workspace = workspace,
    search = function(query, k) {
      # Perspective seeding returns no URLs, so there is no ToC web request.
      if (identical(query, "Synthetic polymer recovery pilot")) {
        return(data.frame(
          title = character(),
          url = character(),
          snippet = character()
        ))
      }
      data.frame(title = resource@title, url = url, snippet = statement)
    },
    fetch = function(url) {
      if (!identical(url, resource@locator)) {
        stop("Unknown synthetic source.")
      }
      resource
    }
  )
  list(
    config = config,
    experts = list(expert),
    retriever = retriever,
    statement = statement
  )
}

tempest_demo_research <- function(
  directory,
  correction = FALSE,
  knowledge = NULL
) {
  inputs <- tempest_demo_inputs(correction)
  run_id <- if (correction) "pilot-correction" else "pilot-initial"
  if (!is.null(knowledge)) {
    run_id <- paste0(
      run_id,
      "-",
      substr(
        digest::digest(knowledge@artifact_selection, algo = "sha256"),
        1L,
        16L
      )
    )
  }
  resume <- dir.exists(file.path(directory, "runs", run_id))
  if (resume) {
    # Let the public resume path restore all persisted evidence together.
    inputs$retriever$workspace <- tempest::tempest_research_workspace()
  }
  tempest::tempest_run(
    "Synthetic polymer recovery pilot",
    config = inputs$config,
    experts = inputs$experts,
    retriever = inputs$retriever,
    max_questions_per_perspective = 1L,
    knowledge = knowledge,
    output_dir = file.path(directory, "runs"),
    run_id = run_id,
    resume = resume,
    verbose = FALSE
  )
}

# Called in an independent R process by the demo and the integration test.
tempest_demo_reopen <- function(directory, decision_id) {
  store <- graft::graft_store(file.path(directory, "evidence"))
  knowledge <- tempest::tempest_reuse_artifact_research(
    store,
    "synthetic-pilot",
    decision_id,
    "demo-briefing",
    eligible = function(event) TRUE
  )
  inputs <- tempest_demo_inputs()
  session <- tempest::tempest_session(
    "Retained pilot briefing",
    config = inputs$config,
    experts = inputs$experts,
    knowledge = knowledge
  )
  path <- tempfile("tempest-demo-session-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  tempest::tempest_session_save(session, path)
  restored <- tempest::tempest_session_resume(
    path,
    config = inputs$config,
    knowledge = knowledge
  )
  stopifnot(identical(
    tempest::tempest_sources(session),
    tempest::tempest_sources(restored)
  ))
  list(
    decision_id = decision_id,
    selection_id = knowledge@artifact_selection$selection_id,
    sources = tempest::tempest_sources(restored),
    records = length(knowledge@records),
    exact_restart = TRUE
  )
}
