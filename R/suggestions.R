# Suggested follow-up questions for the tempest Chat tab.

#' @keywords internal
tempest_type_suggested_questions <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    questions = ellmer::type_array(
      ellmer::type_string(
        "A concise, self-contained question the user could ask next."
      )
    )
  )
}

#' @keywords internal
tempest_suggest_questions_prompt <- function(topic, context = NULL, n = 4) {
  parts <- c(paste0("Topic: ", topic), "")
  if (!is.null(context) && nzchar(context)) {
    parts <- c(
      parts,
      "Conversation so far:",
      context,
      "",
      paste0("Suggest ", n, " follow-up questions the user could ask next.")
    )
  } else {
    parts <- c(
      parts,
      paste0(
        "Suggest ",
        n,
        " questions a curious newcomer could ask to start exploring this topic."
      )
    )
  }
  parts <- c(
    parts,
    "",
    "These will be rendered as clickable suggestion cards, separate from the assistant's prose answer.",
    "Focus on topic-specific research questions that fill evidence gaps, inspect uncertainty, or expand the mind map.",
    "Do not suggest generic artifacts or transformations such as diagrams, slide decks, outlines, or gap-analysis tables unless the user explicitly asked for that artifact."
  )
  paste(parts, collapse = "\n")
}

#' Suggest follow-up research questions for a topic
#'
#' Generates a short list of concise, user-facing questions to ask next about a
#' topic, optionally conditioned on the conversation so far. The tempest Shiny
#' app uses this to render clickable suggestion cards. The call is a single
#' tool-free structured completion: any error yields an empty result.
#'
#' @param topic The research topic.
#' @param context Optional character string with the recent conversation. When
#'   `NULL`, questions are generated from the topic alone.
#' @param n Maximum number of questions to return.
#' @param chat Optional ellmer Chat to use. When `NULL`, a chat is created from
#'   `config` using the coordinator model; the structured call does not invoke
#'   tools.
#' @param config A `TempestConfig` object.
#' @return A character vector of at most `n` questions (possibly empty).
#' @examples
#' \dontrun{
#' tempest_suggest_questions("History of jazz", n = 4)
#' }
#' @export
tempest_suggest_questions <- function(
  topic,
  context = NULL,
  n = 4,
  chat = NULL,
  config = tempest_config()
) {
  topic <- tempest_trim(topic %||% "")
  if (length(topic) != 1L || is.na(topic) || !nzchar(topic)) {
    return(character())
  }
  n <- suppressWarnings(as.integer(n)[1])
  if (length(n) == 0L || is.na(n) || n < 1L) {
    n <- 4L
  }
  tryCatch(
    {
      if (is.null(chat)) {
        tempest_require("ellmer", "Question suggestions require ellmer.")
        chat <- tempest_make_chat(
          config,
          "coordinator",
          system_prompt = tempest_prompt("question_suggester_system"),
          echo = "none"
        )
      }
      result <- chat$chat_structured(
        tempest_suggest_questions_prompt(topic, context, n),
        type = tempest_type_suggested_questions(),
        echo = "none",
        convert = FALSE
      )
      qs <- tempest_as_character_vector(result$questions %||% character())
      utils::head(qs, n)
    },
    error = function(e) character()
  )
}
