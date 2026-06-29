test_that("tempest_run emits ordered STORM progress events", {
  skip_if_not_installed("ellmer")
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_extract_toc_from_url = function(url) character(),
    tempest_wiki_page_sections = function(title) character()
  )

  source <- fake_source(
    url = "https://example.org/progress",
    title = "Progress source",
    content_text = "Progress uses staged events and persisted artifacts."
  )
  source_id <- source$id
  store <- SourceStore$new()
  store$upsert_source(source)
  collector <- tempest_progress_collector(include_payload = TRUE)
  outline <- list(
    title = "Progress report",
    sections = list(list(
      title = "Workflow evidence",
      summary = "How progress events flow through STORM.",
      subsections = list(list(
        title = "Signals",
        bullets = c("Stage events", "Artifact events"),
        needed = c("Cited facts")
      ))
    ))
  )
  claim_result <- function(claim) {
    list(
      facts = list(list(
        claim = claim,
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ))
    )
  }
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) {
      if (
        identical(role, "writer") &&
          identical(system_prompt, tempest_prompt("polisher_system"))
      ) {
        return(fake_chat(
          text = list(paste0(
            "Polished report cites progress evidence [",
            source_id,
            "]."
          ))
        ))
      }
      if (identical(role, "writer")) {
        return(fake_chat(
          structured = list(
            list(queries = c("progress events")),
            outline,
            outline
          ),
          text = list(
            paste0("Section body cites events [", source_id, "]."),
            paste0("Lead body cites events [", source_id, "].")
          )
        ))
      }
      if (
        identical(role, "judge") &&
          identical(system_prompt, tempest_prompt("fact_extractor_system"))
      ) {
        return(fake_chat(
          structured = list(
            claim_result("STORM progress emits stage events."),
            claim_result("STORM progress persists artifacts.")
          )
        ))
      }
      if (identical(role, "judge")) {
        return(fake_chat(
          structured = list(
            list(status = "supported", score = 0.9, rationale = "ok"),
            list(status = "supported", score = 0.9, rationale = "ok")
          )
        ))
      }
      if (identical(role, "expert")) {
        return(fake_chat(
          text = list(paste0(
            "Expert answer cites progress evidence [",
            source_id,
            "]."
          ))
        ))
      }
      if (
        identical(role, "coordinator") &&
          identical(system_prompt, tempest_prompt("persona_generator_system"))
      ) {
        return(fake_chat(
          structured = list(list(
            personas = list(list(
              name = "Dr. Flow",
              title = "Workflow analyst",
              affiliation = "",
              background = "",
              focus_areas = c("Progress"),
              perspective = "Workflow progress",
              initial_questions = c("How should progress be reported?")
            ))
          ))
        ))
      }
      fake_chat(
        structured = list(
          list(
            title = "Progress report",
            perspectives = list(list(
              name = "Workflow",
              description = "Workflow progress perspective.",
              key_questions = c("How should progress be reported?")
            ))
          )
        )
      )
    }
  )
  retriever <- tempest_retriever(config = cfg, store = store)

  result <- tempest_run(
    "Progress events",
    config = cfg,
    retriever = retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    dsprrr_modules = list(),
    output_dir = withr::local_tempdir(),
    run_id = "progress-run",
    progress = collector$record,
    verbose = FALSE
  )

  event_data <- collector$data()
  labels <- vapply(
    event_data,
    function(event) {
      paste(
        event$event_type,
        event$stage,
        event$step,
        event$status,
        sep = ":"
      )
    },
    character(1)
  )

  expect_contains(
    labels,
    c(
      "workflow:NA:NA:started",
      "stage:perspectives:NA:started",
      "stage:perspectives:NA:succeeded",
      "stage:research:NA:started",
      "stage:research:NA:succeeded",
      "stage:outline:NA:started",
      "stage:outline:NA:succeeded",
      "stage:write:NA:started",
      "stage:write:NA:succeeded",
      "stage:polish:NA:started",
      "stage:verification:NA:started",
      "stage:verification:NA:succeeded",
      "stage:polish:NA:succeeded",
      "step:persistence:perspectives_artifacts:succeeded",
      "step:persistence:research_artifacts:succeeded",
      "step:persistence:outline_artifacts:succeeded",
      "step:persistence:write_artifacts:succeeded",
      "step:persistence:polish_artifacts:succeeded",
      "artifact:polish:report_md:available",
      "workflow:NA:NA:succeeded"
    )
  )
  expect_lt(
    match("stage:perspectives:NA:started", labels),
    match("stage:research:NA:started", labels)
  )
  expect_lt(
    match("stage:write:NA:succeeded", labels),
    match("stage:polish:NA:started", labels)
  )
  expect_lt(
    match("stage:verification:NA:succeeded", labels),
    match("stage:polish:NA:succeeded", labels)
  )
  expect_match(result$report_md, "Polished report")
  expect_equal(
    collector$data(event_type = "artifact")[[1]]$payload$artifact,
    "report_md"
  )
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "run_id")),
    "progress-run"
  )
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "workflow")),
    "storm"
  )
})

test_that("STORM research harvests OpenAI native annotations", {
  skip_if_not_installed("ellmer")

  url <- "https://example.org/storm-native-source"
  source_id <- tempest:::tempest_source_id(url)
  turn <- native_openai_json_turn(
    claim_text = "STORM native-backed claim.",
    url = url,
    title = "STORM Native Source"
  )
  expert_chat <- list(
    chat = function(prompt, ...) "STORM native-backed claim.",
    last_turn = function() turn,
    register_tools = function(...) invisible(NULL)
  )
  writer_chat <- fake_chat(
    structured = list(list(queries = "storm native evidence"))
  )
  extractor <- fake_chat(
    structured = list(list(
      facts = list(list(
        claim = "STORM native-backed claim",
        sources = list(list(source_id = source_id)),
        confidence = "high",
        support_score = 0.87
      ))
    ))
  )
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) {
      return(expert_chat)
    }
    if (
      identical(role, "judge") &&
        identical(system_prompt, tempest_prompt("fact_extractor_system"))
    ) {
      return(extractor)
    }
    if (identical(role, "writer")) {
      return(writer_chat)
    }
    fake_chat()
  })
  perspectives <- list(list(
    name = "Native evidence",
    description = "Native evidence perspective.",
    key_questions = c("What does native evidence say?")
  ))
  personas <- list(list(
    id = 1,
    name = "Dr. Native",
    title = "Researcher",
    perspective = "Native evidence"
  ))

  result <- tempest:::tempest_research_one_perspective(
    1,
    perspectives = perspectives,
    personas = personas,
    config = cfg,
    topic = "Native evidence",
    research_strategy = "key_questions",
    max_questions_per_perspective = 1,
    dsprrr_modules = list()
  )

  expect_equal(result$sources[[1]]$id, source_id)
  expect_equal(result$sources[[1]]$title, "STORM Native Source")
  result_store <- SourceStore$new()
  result_store$upsert_source(result$sources[[1]])
  sources <- tempest_sources(result_store)
  expect_match(sources$snippet[[1]], "STORM native-backed claim")
  expect_match(sources$context_text[[1]], "STORM native-backed claim")
  expect_length(result$claims, 1L)
  expect_equal(result$claims[[1]]@source_ids, source_id)
  expect_equal(result$claims[[1]]@support_score, 0.87)
})

test_that("tempest_run emits a failed verification stage event", {
  skip_if_not_installed("ellmer")
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_extract_toc_from_url = function(url) character(),
    tempest_wiki_page_sections = function(title) character(),
    tempest_run_verification = function(...) {
      stop("verification module unavailable")
    }
  )
  fixture <- storm_progress_fixture()
  collector <- tempest_progress_collector()

  expect_error(
    tempest_run(
      "Progress events",
      config = fixture$config,
      retriever = fixture$retriever,
      n_experts = 1,
      max_questions_per_perspective = 1,
      dsprrr_modules = list(),
      output_dir = withr::local_tempdir(),
      run_id = "progress-verify-fail",
      progress = collector$record,
      verbose = FALSE
    ),
    "verification module unavailable"
  )

  labels <- vapply(
    collector$data(),
    function(event) {
      paste(event$event_type, event$stage, event$status, sep = ":")
    },
    character(1)
  )
  expect_contains(
    labels,
    c(
      "stage:verification:started",
      "stage:verification:failed",
      "workflow:NA:failed"
    )
  )
})

test_that("tempest_run emits terminal progress events on failure", {
  skip_if_not_installed("ellmer")
  collector <- tempest_progress_collector()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )

  expect_error(
    tempest_run(
      "Progress failure",
      config = cfg,
      retriever = tempest_retriever(config = cfg, store = SourceStore$new()),
      steps = "write",
      dsprrr_modules = list(),
      progress = collector$record,
      verbose = FALSE
    ),
    "No outline available"
  )

  labels <- vapply(
    collector$data(),
    function(event) {
      paste(event$event_type, event$stage, event$status, sep = ":")
    },
    character(1)
  )
  expect_contains(
    labels,
    c(
      "workflow:NA:started",
      "stage:write:started",
      "stage:write:failed",
      "workflow:NA:failed"
    )
  )
})
