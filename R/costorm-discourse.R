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
    agent_name = ellmer::type_string(
      "Name of the agent to act (for expert_speaks/retire_expert).",
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
#' LLM-driven discourse management for Co-STORM sessions.
#' Decides which agent should speak next and what action to take.
#'
#' @field chat An ellmer chat object for making decisions.
#' @field config A `TempestConfig` object.
#'
#' @keywords internal
DiscourseManager <- R6::R6Class(
  "DiscourseManager",
  public = list(
    chat = NULL,
    config = NULL,

    #' @description
    #' Create a new DiscourseManager.
    #' @param config A `TempestConfig` object.
    initialize = function(config) {
      self$config <- config
      self$chat <- tempest_make_chat(
        config,
        "coordinator",
        system_prompt = tempest_prompt("discourse_manager_system"),
        echo = "none"
      )
      invisible(self)
    },

    #' @description
    #' Decide the next turn action.
    #' @param topic The research topic.
    #' @param transcript_md Recent transcript as markdown.
    #' @param mindmap_md Mind map as markdown.
    #' @param persona_descriptions Formatted persona descriptions.
    #' @param unseen_sources Character vector of undiscussed source IDs.
    #' @return A turn decision list with action, agent_name, instruction, rationale.
    decide_next_turn = function(
      topic,
      transcript_md,
      mindmap_md,
      persona_descriptions,
      unseen_sources = character()
    ) {
      type <- tempest_type_turn_policy()

      unseen_txt <- if (length(unseen_sources) > 0) {
        paste0(
          "\n\nUndiscussed sources (",
          length(unseen_sources),
          " total):\n",
          paste0("- ", head(unseen_sources, 10), collapse = "\n")
        )
      } else {
        ""
      }

      prompt <- paste0(
        "Topic: ",
        topic,
        "\n\n",
        "Expert panel:\n",
        persona_descriptions,
        "\n\n",
        "Mind map:\n",
        mindmap_md,
        "\n\n",
        "Recent conversation:\n",
        transcript_md,
        "\n",
        unseen_txt,
        "\n\n",
        "Decide the next action for the conversation.\n",
        "Return structured data."
      )

      tryCatch(
        self$chat$chat_structured(
          prompt,
          type = type,
          echo = "none",
          convert = FALSE
        ),
        error = function(e) {
          tempest_warn(
            "Discourse manager decision failed: {conditionMessage(e)}"
          )
          list(
            action = "moderator_probes",
            agent_name = "",
            instruction = "Continue exploring the topic.",
            rationale = "Fallback due to discourse manager error."
          )
        }
      )
    }
  )
)
