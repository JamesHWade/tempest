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

test_expert <- function(
  expert_id = "expert.test",
  name = "Dr. Test",
  title = "Research specialist",
  description = "A test expert profile.",
  instructions = "Answer the assigned question with explicit evidence.",
  initial_questions = character(),
  required_capability_ids = character(),
  optional_capability_ids = character(),
  state = "active",
  metadata = list()
) {
  tempest_expert(
    expert_id = expert_id,
    name = name,
    title = title,
    description = description,
    instructions = instructions,
    initial_questions = initial_questions,
    required_capability_ids = required_capability_ids,
    optional_capability_ids = optional_capability_ids,
    state = state,
    metadata = metadata
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
    experts = list(tempest_expert(
      expert_id = "expert.native",
      name = "Dr. Native",
      title = "Researcher",
      description = "Native source evidence",
      instructions = "Inspect and cite provider-native source evidence."
    ))
  )
  list(
    session = ses,
    turn = turn,
    source_id = source_id,
    claim_text = claim_text
  )
}
