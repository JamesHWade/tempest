# Deterministic suggested follow-up questions for the Tempest Chat tab.

tempest_suggest_questions_projection <- function(topic, context, n) {
  discussed <- !is.null(context) && nzchar(tempest_trim(context))
  first <- if (discussed) {
    paste0(
      "What evidence is still missing from the current discussion of ",
      topic,
      "?"
    )
  } else {
    paste0("What evidence best establishes the key claims about ", topic, "?")
  }
  questions <- c(
    first,
    paste0(
      "Which uncertainty or tradeoff matters most for understanding ",
      topic,
      "?"
    ),
    paste0(
      "What contrasting perspective could change the view of ",
      topic,
      "?"
    ),
    paste0(
      "How could the strongest claim about ",
      topic,
      " be independently verified?"
    )
  )
  utils::head(questions, n)
}

#' Suggest follow-up research questions for a topic
#'
#' Projects a short, deterministic list of user-facing research questions from
#' a topic and whether prior conversation context is present. Session-bound
#' suggestions use the session's authoritative `next_question` program instead.
#'
#' @param topic The research topic.
#' @param context Optional character string with the recent conversation. When
#'   nonempty, the first question asks about evidence missing from the current
#'   discussion.
#' @param n Maximum number of questions to return.
#' @return A character vector of at most `n` questions (possibly empty).
#' @examples
#' tempest_suggest_questions("History of jazz", n = 4)
#' @export
tempest_suggest_questions <- function(topic, context = NULL, n = 4) {
  topic <- tempest_trim(topic %||% "")
  if (length(topic) != 1L || is.na(topic) || !nzchar(topic)) {
    return(character())
  }
  n <- tempest_config_count(n, "n")
  tempest_suggest_questions_projection(topic, context, n)
}

#' @keywords internal
tempest_suggest_questions_async <- function(topic, context = NULL, n = 4) {
  tempest_require("promises", "Async question suggestions require promises.")
  promises::promise_resolve(tempest_suggest_questions(topic, context, n))
}
