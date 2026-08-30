# tests/testthat/helper-storm-progress.R
# Builds a fully scripted STORM fixture that runs end-to-end (through the
# verification stage) without network or API keys, for progress-event tests.

storm_progress_fixture <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_extract_toc_from_url = function(url) character(),
    tempest_wiki_page_sections = function(title) character(),
    tempest_storm_semantic_filter_facts = function(
      retriever,
      query,
      store,
      max_items = 30,
      min_support_score = 0.7
    ) {
      facts <- tempest:::tempest_supported_claims(
        store,
        min_support_score = min_support_score
      )
      utils::head(facts, max_items)
    },
    .env = .local_envir
  )
  source <- fake_source(
    url = "https://example.org/progress",
    title = "Progress source",
    content_text = "Progress uses staged events and persisted artifacts."
  )
  source_id <- source@resource_id
  store <- test_research_workspace()
  store$upsert_retrieved_resource(source)
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
        sources = list(list(
          source_id = source_id,
          quote = "Progress uses staged events and persisted artifacts."
        )),
        confidence = "high"
      ))
    )
  }
  cfg <- tempest_config(
    citation_policy = "claim_verified",
    cache_enabled = FALSE,
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "writer")) {
        return(fake_chat(
          structured = list(
            list(queries = c("progress events")),
            outline,
            outline,
            function(prompt) {
              fake_briefing_output_from_prompt(
                prompt,
                "STORM progress emits stage events."
              )
            },
            function(prompt) {
              fake_briefing_output_from_prompt(
                prompt,
                "STORM progress emits stage events."
              )
            }
          ),
          text = list(
            paste0(
              "STORM progress emits stage events [",
              source_id,
              "]."
            ),
            paste0(
              "STORM progress emits stage events [",
              source_id,
              "]."
            )
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
              affiliation = "Independent",
              background = "Studies observable workflow execution.",
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
  list(
    config = cfg,
    store = store,
    retriever = tempest_retriever(config = cfg, workspace = store),
    source_id = source_id,
    outline = outline
  )
}
