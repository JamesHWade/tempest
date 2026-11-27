# Deterministic stage fallbacks

tempest_stage_fallback_perspectives <- function(chat, inputs, context) {
  prompt <- paste0(
    "You are planning a comprehensive research report.\n",
    "Topic: ",
    inputs$topic,
    "\n\n",
    inputs$seed_context,
    "\n\n",
    "Propose exactly ",
    inputs$n_experts,
    paste0(
      " distinct perspectives to cover the topic. Each perspective ",
      "should have 3-6 research questions.\n"
    ),
    "Return structured data."
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_perspectives(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_personas <- function(chat, inputs, context) {
  tempest_personas_ellmer_fallback(chat, inputs, context)
}

tempest_stage_fallback_query_decomposition <- function(chat, inputs, context) {
  list(queries = inputs$question)
}

tempest_stage_fallback_next_question <- function(chat, inputs, context) {
  prompt <- paste0(
    "You are interviewing an expert to build a factual knowledge base.\n\n",
    "Topic: ",
    inputs$topic,
    "\nPerspective: ",
    inputs$perspective,
    "\n\nAnswered Q&A so far:\n",
    inputs$answered,
    "\n\nCurrent fact notes summary:\n",
    inputs$facts,
    "\n\nPropose the single most useful next question to ask.\n",
    "If this perspective is sufficiently covered, set done = true.\n",
    "Return structured data."
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_next_question(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_draft_outline <- function(chat, inputs, context) {
  prompt <- paste0(
    "Create a draft outline for a report based on your own knowledge.\n\n",
    "Topic: ",
    inputs$topic,
    "\nDesired report title: ",
    inputs$report_title,
    "\n\nRequirements:\n",
    "- Organize into 4-6 sections based on what you know about the topic.\n",
    "- Include subsections with bullet points.\n",
    "- This is a preliminary outline; it will be refined with research findings.\n",
    "- Return structured data.\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_outline(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_refined_outline <- function(chat, inputs, context) {
  prompt <- paste0(
    "Refine this draft outline using verified research findings.\n\n",
    "Topic: ",
    inputs$topic,
    "\nDesired report title: ",
    inputs$report_title,
    "\n\nDraft outline sections:\n",
    inputs$draft_outline,
    "\n\nAvailable verified fact notes (each includes citations):\n",
    context$verified_facts,
    "\n\nRequirements:\n",
    "- Adjust sections based on available evidence.\n",
    "- Add, merge, or remove sections as needed.\n",
    "- Exclude introductions, summaries, overviews, conclusions, executive summaries, and at-a-glance sections.\n",
    "- Remove angles without verified facts; one detail section is valid when evidence is sparse.\n",
    "- Ensure each section has supporting facts.\n",
    "- Return structured data.\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_outline(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_section_writing <- function(chat, inputs, context) {
  prompt <- paste0(
    "Prepare typed briefing items for one report section.\n\n",
    "Section title: ",
    inputs$section_title,
    "\nSection intent: ",
    inputs$section_summary,
    "\n\nPlanned subsections:\n",
    inputs$subsections,
    "\n\nVerified fact notes you MUST use (do not invent facts or fetch additional sources):\n",
    context$verified_facts,
    "\n\nRules:\n",
    "- Each fact ends with its status against accepted knowledge: new, or already accepted.\n",
    "- Return up to three observation items by copying the exact text of facts whose status is new; at least one is required whenever any fact is new, and none are allowed when no fact is new.\n",
    "- Add at most two assessment prompts using exactly: Assess the decision implications of: CLAIM_TEXT.\n",
    "- Add at most two review-action prompts using exactly: Review before deciding: CLAIM_TEXT.\n",
    "- For multiple claims, sort by claim_id and join exact claim text with ' | '.\n",
    "- Add at most one no-change item by copying the exact text of one fact whose status is already accepted; never use a new fact.\n",
    "- When no fact is new, return exactly one no-change item.\n",
    "- Omit no-change when no fact is already accepted; missing or unresolved evidence is not a no-change signal.\n",
    "- Bind every item to exactly the supplied claim_ids copied into its text.\n",
    "- Confidence is required only for assessments and no-change items.\n",
    "- Use ONLY the facts above. Do NOT call tools or fetch sources.\n",
    "- Return structured data, not Markdown.\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_briefing_items(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_lead_section <- function(chat, inputs, context) {
  prompt <- paste0(
    "Create the at-a-glance decision brief for this report.\n\n",
    "Topic: ",
    inputs$topic,
    "\nTitle: ",
    inputs$title,
    "\n\nArticle body (for context):\n",
    inputs$article_body,
    "\n\nKey facts:\n",
    context$verified_facts,
    "\n\nRules:\n",
    "- Each fact ends with its status against accepted knowledge: new, or already accepted.\n",
    "- Return up to three observations by copying the exact text of facts whose status is new; at least one is required whenever any fact is new, and none are allowed when no fact is new.\n",
    "- Add at most one assessment prompt using exactly: Assess the decision implications of: CLAIM_TEXT.\n",
    "- Add at most one review-action prompt using exactly: Review before deciding: CLAIM_TEXT.\n",
    "- For multiple claims, sort by claim_id and join exact claim text with ' | '.\n",
    "- Add at most one no-change item by copying the exact text of one fact whose status is already accepted; never use a new fact.\n",
    "- When no fact is new, return exactly one no-change item.\n",
    "- Omit no-change when no fact is already accepted; missing or unresolved evidence is not a no-change signal.\n",
    "- Bind every item to exactly the supplied claim_ids copied into its text.\n",
    "- Confidence is required only for assessments and no-change items.\n",
    "- Return structured data, not Markdown.\n"
  )
  chat$chat_structured(
    prompt,
    type = tempest_type_briefing_items(),
    echo = "none",
    convert = FALSE
  )
}

tempest_stage_fallback_registry <- function() {
  list(
    perspectives = tempest_stage_fallback_perspectives,
    personas = tempest_stage_fallback_personas,
    query_decomposition = tempest_stage_fallback_query_decomposition,
    next_question = tempest_stage_fallback_next_question,
    draft_outline = tempest_stage_fallback_draft_outline,
    refined_outline = tempest_stage_fallback_refined_outline,
    section_writing = tempest_stage_fallback_section_writing,
    lead_section = tempest_stage_fallback_lead_section
  )
}

tempest_stage_fallback_resolve <- function(stage) {
  policy <- tempest_stage_policy(stage)
  fallback <- tempest_stage_fallback_registry()[[stage]] %||% NULL
  if (identical(policy$fallback_policy, "fail_closed")) {
    if (!is.null(fallback)) {
      tempest_stage_evaluator_abort(
        "Fail-closed stage unexpectedly has a fallback implementation."
      )
    }
    return(NULL)
  }
  if (!is.function(fallback)) {
    tempest_stage_evaluator_abort(
      "Fallback-eligible stage lacks its package-owned implementation."
    )
  }
  fallback
}

tempest_stage_failure_messages <- function() {
  c(
    execution = "Primary stage execution failed.",
    validation = "Primary stage output failed validation.",
    fallback = "Primary execution failed and configured fallback also failed.",
    commit = "Validated stage output could not be committed.",
    cancelled = "Stage execution was cancelled."
  )
}
