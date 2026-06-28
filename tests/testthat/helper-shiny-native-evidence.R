native_evidence_session <- function(claim_text = "native-backed app claim") {
  url <- "https://example.org/native-app-source"
  source_id <- tempest:::tempest_source_id(url)
  SearchResponse <- getFromNamespace("ContentToolResponseSearch", "ellmer")
  turn <- ellmer::AssistantTurn(
    contents = list(
      SearchResponse(
        urls = url,
        json = list(
          results = list(list(
            title = "Native app source",
            url = url,
            snippet = "Native app source snippet."
          ))
        )
      ),
      ellmer::ContentText(claim_text)
    )
  )
  extractor <- list(
    chat_structured = function(prompt, type = NULL, ...) {
      if (!grepl(source_id, prompt, fixed = TRUE)) {
        return(list(facts = list()))
      }
      list(
        facts = list(list(
          claim = claim_text,
          sources = list(list(source_id = source_id)),
          confidence = "high"
        ))
      )
    }
  )
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    if (
      identical(role, "judge") &&
        identical(system_prompt, tempest_prompt("fact_extractor_system"))
    ) {
      return(extractor)
    }
    fake_chat()
  })
  ses <- tempest_session(
    "Native evidence topic",
    config = cfg,
    personas = list(tempest_expert(
      name = "Dr. Native",
      title = "Researcher",
      perspective = "Native source evidence"
    ))
  )
  list(
    session = ses,
    turn = turn,
    source_id = source_id,
    claim_text = claim_text
  )
}
