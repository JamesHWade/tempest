native_openai_json_turn <- function(
  claim_text = "native-backed app claim",
  url = "https://example.org/native-app-source",
  title = "Native app source"
) {
  ellmer::AssistantTurn(
    contents = list(
      ellmer::ContentText(claim_text)
    ),
    json = list(
      output = list(
        list(
          type = "web_search_call",
          action = list(type = "search")
        ),
        list(
          type = "message",
          content = list(list(
            type = "output_text",
            text = claim_text,
            annotations = list(list(
              type = "url_citation",
              title = title,
              url = url
            ))
          ))
        )
      )
    )
  )
}

native_evidence_session <- function(claim_text = "native-backed app claim") {
  url <- "https://example.org/native-app-source"
  source_id <- tempest:::tempest_source_id(url)
  turn <- native_openai_json_turn(
    claim_text = claim_text,
    url = url,
    title = "Native app source"
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
