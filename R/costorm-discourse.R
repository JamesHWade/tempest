# Co-STORM discourse management

#' Structured type for discourse turn decisions
#' @keywords internal
tempest_type_turn_policy <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    action = ellmer::type_enum(
      c(
        "expert_speaks",
        "moderator_probes",
        "add_expert",
        "retire_expert",
        "surface_unseen",
        "end_round"
      ),
      "What action to take next."
    ),
    expert_id = ellmer::type_string(
      "Stable expert id to act (for expert_speaks/retire_expert).",
      required = FALSE
    ),
    instruction = ellmer::type_string(
      "Instruction or question for the chosen agent."
    ),
    rationale = ellmer::type_string(
      "Short reason for this decision.",
      required = FALSE
    )
  )
}

#' DiscourseManager
#'
#' Retired generic discourse manager for Co-STORM sessions.
#'
#' @field config A `TempestConfig` object.
#'
#' @keywords internal
DiscourseManager <- R6::R6Class(
  "DiscourseManager",
  public = list(
    config = NULL,

    #' @description
    #' Create a new DiscourseManager.
    #' @param config A `TempestConfig` object.
    initialize = function(config) {
      tempest_costorm_session_abort(
        paste0(
          "The generic discourse manager is unavailable; Co-STORM accepts ",
          "only explicit moderator turns through its completion boundary."
        )
      )
    },

    #' @description
    #' Decide the next turn action.
    #' @param topic The research topic.
    #' @param transcript_md Recent transcript as markdown.
    #' @param mindmap_md Mind map as markdown.
    #' @param expert_descriptions Formatted expert descriptions with stable ids.
    #' @param unseen_sources Character vector of undiscussed source IDs.
    #' @return A turn decision list with action, expert_id, instruction, rationale.
    decide_next_turn = function(
      topic,
      transcript_md,
      mindmap_md,
      expert_descriptions,
      unseen_sources = character()
    ) {
      tempest_costorm_session_abort(
        paste0(
          "Automatic discourse decisions are unavailable; submit an explicit ",
          "moderator turn through the session completion boundary."
        )
      )
    }
  )
)
