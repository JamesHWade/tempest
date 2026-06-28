# tests/testthat/helper-storm-progress.R
# Builds a fully scripted STORM fixture that runs end-to-end (through the
# verification stage) without network or API keys, for progress-event tests.

storm_progress_fixture <- function() {
  source <- fake_source(
    url = "https://example.org/progress",
    title = "Progress source",
    content_text = "Progress uses staged events and persisted artifacts."
  )
  source_id <- source$id
  store <- SourceStore$new()
  store$upsert_source(source)
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
  list(
    config = cfg,
    store = store,
    retriever = tempest_retriever(config = cfg, store = store),
    source_id = source_id
  )
}
