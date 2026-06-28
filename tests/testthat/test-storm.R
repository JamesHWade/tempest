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
  events <- list()
  collect <- function(event) {
    events[[length(events) + 1L]] <<- event
  }
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
    progress = collect,
    verbose = FALSE
  )

  event_data <- lapply(events, tempest_progress_event_data)
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
    unique(vapply(event_data, `[[`, character(1), "run_id")),
    "progress-run"
  )
  expect_equal(
    unique(vapply(event_data, `[[`, character(1), "workflow")),
    "storm"
  )
})

test_that("tempest_run emits terminal progress events on failure", {
  skip_if_not_installed("ellmer")
  events <- list()
  collect <- function(event) {
    events[[length(events) + 1L]] <<- event
  }
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
      progress = collect,
      verbose = FALSE
    ),
    "No outline available"
  )

  labels <- vapply(
    lapply(events, tempest_progress_event_data),
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
