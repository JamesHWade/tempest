# STORM ellmer type definitions

#' @keywords internal
tempest_type_perspectives <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    title = ellmer::type_string("Suggested title for the final report."),
    perspectives = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string("Perspective name (short)"),
        description = ellmer::type_string("What this perspective covers"),
        key_questions = ellmer::type_array(ellmer::type_string(
          "A research question"
        ))
      )
    )
  )
}

#' @keywords internal
tempest_type_personas <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    personas = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string(
          "Full name of the persona (e.g., 'Dr. Sarah Chen')"
        ),
        title = ellmer::type_string(
          "Professional title or role (e.g., 'Climate Scientist')"
        ),
        affiliation = ellmer::type_string(
          "Organization or institution (e.g., 'Arctic Research Institute')"
        ),
        background = ellmer::type_string(
          "2-3 sentence background: expertise, experience, what shaped their perspective"
        ),
        focus_areas = ellmer::type_array(ellmer::type_string(
          "Specific focus area or subtopic"
        )),
        perspective = ellmer::type_string(
          "How this persona uniquely approaches the topic - their angle or lens"
        ),
        initial_questions = ellmer::type_array(ellmer::type_string(
          "Questions this persona would naturally ask"
        ))
      )
    )
  )
}

#' @keywords internal
tempest_type_outline <- function() {
  tempest_require("ellmer")
  subsection <- ellmer::type_object(
    title = ellmer::type_string("Subsection title"),
    bullets = ellmer::type_array(ellmer::type_string(
      "Bullet: a specific point to cover"
    )),
    needed = ellmer::type_array(
      ellmer::type_string("If needed: unanswered question"),
      required = FALSE
    )
  )
  section <- ellmer::type_object(
    title = ellmer::type_string("Section title"),
    summary = ellmer::type_string("1-2 sentence summary"),
    subsections = ellmer::type_array(subsection)
  )
  ellmer::type_object(
    title = ellmer::type_string("Report title"),
    sections = ellmer::type_array(section)
  )
}

#' @keywords internal
tempest_type_fact_extract <- function() {
  tempest_require("ellmer")
  src <- ellmer::type_object(
    source_id = ellmer::type_string("Source id like Sxxxxxxxxxxxx"),
    url = ellmer::type_string("URL if available", required = FALSE),
    quote = ellmer::type_string(
      "Short supporting quote (<=20 words) if available",
      required = FALSE
    )
  )
  fact <- ellmer::type_object(
    claim = ellmer::type_string("Atomic factual claim"),
    sources = ellmer::type_array(src),
    confidence = ellmer::type_enum(
      c("low", "medium", "high"),
      "Confidence",
      required = FALSE
    ),
    support_score = ellmer::type_number(
      "Degree in [0,1] to which cited sources support this claim.",
      required = FALSE
    ),
    note = ellmer::type_string(
      "Nuance, scope conditions, or caveats.",
      required = FALSE
    )
  )
  ellmer::type_object(facts = ellmer::type_array(fact))
}

#' @keywords internal
tempest_type_next_question <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    question = ellmer::type_string("Next research question to ask the expert."),
    done = ellmer::type_boolean(
      "Whether research for this perspective is complete.",
      required = FALSE
    )
  )
}

#' @keywords internal
tempest_type_query_decomposition <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    queries = ellmer::type_array(
      ellmer::type_string("A targeted search query")
    )
  )
}
