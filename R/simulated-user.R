# Simulated user for Co-STORM evaluation

#' SimulatedUser
#'
#' A simulated curious researcher for automated Co-STORM evaluation.
#' Generates natural research questions and tracks turn counts.
#'
#' @field topic The research topic.
#' @field chat An ellmer chat object for generating questions.
#' @field turn_count Number of turns completed.
#' @field max_turns Maximum turns before stopping.
#'
#' @export
SimulatedUser <- R6::R6Class(
  "SimulatedUser",
  public = list(
    topic = NULL,
    chat = NULL,
    turn_count = 0L,
    max_turns = 10L,

    #' @description
    #' Create a new SimulatedUser.
    #' @param topic The research topic.
    #' @param config A `TempestConfig` object.
    #' @param max_turns Maximum number of turns.
    initialize = function(topic, config = tempest_config(), max_turns = 10L) {
      tempest_require("ellmer", "SimulatedUser requires ellmer.")
      self$topic <- tempest_trim(topic)
      self$max_turns <- as.integer(max_turns)
      self$turn_count <- 0L
      self$chat <- config$make_chat(
        "coordinator",
        system_prompt = tempest_prompt("simulated_user_system"),
        echo = "none"
      )
      invisible(self)
    },

    #' @description
    #' Generate the next question based on session state.
    #' @param transcript_md Recent transcript as markdown.
    #' @param mindmap_md Mind map as markdown.
    #' @return A question string, or NULL if done.
    generate_question = function(
      transcript_md = "(no dialog yet)",
      mindmap_md = "(empty)"
    ) {
      if (self$turn_count >= self$max_turns) {
        return(NULL)
      }

      prompt <- paste0(
        "Topic: ",
        self$topic,
        "\n\n",
        "Mind map of current knowledge:\n",
        mindmap_md,
        "\n\n",
        "Conversation so far:\n",
        transcript_md,
        "\n\n",
        "You are turn ",
        self$turn_count + 1L,
        " of ",
        self$max_turns,
        ".\n\n",
        "Ask your next research question. If the topic is well-covered, respond with exactly: DONE"
      )

      response <- tryCatch(
        tempest_trim(self$chat$chat(prompt, echo = "none")),
        error = function(e) {
          tempest_warn(
            "SimulatedUser question generation failed at turn {self$turn_count}: {conditionMessage(e)}"
          )
          NULL
        }
      )
      if (is.null(response)) {
        return(NULL)
      }

      if (grepl("^DONE$", response, ignore.case = TRUE)) {
        return(NULL)
      }

      self$turn_count <- self$turn_count + 1L
      response
    },

    #' @description
    #' Run a full automated session.
    #' @param session A `TempestSession` object.
    #' @param warmup Whether to run warmup first.
    #' @param verbose Print progress.
    #' @return The session object (invisibly).
    run_session = function(session, warmup = TRUE, verbose = TRUE) {
      stopifnot(inherits(session, "TempestSession"))

      if (warmup) {
        if (verbose) {
          tempest_inform("SimulatedUser: running warmup")
        }
        session$warmup(verbose = verbose)
      }

      while (self$turn_count < self$max_turns) {
        transcript_md <- session$transcript_markdown(max_turns = 30)
        mindmap_md <- session$mindmap_markdown()

        question <- self$generate_question(transcript_md, mindmap_md)
        if (is.null(question)) {
          if (verbose) {
            tempest_inform(
              "SimulatedUser: research complete at turn {self$turn_count}"
            )
          }
          break
        }

        if (verbose) {
          tempest_inform("SimulatedUser turn {self$turn_count}: {question}")
        }
        session$step(question)
      }

      if (verbose) {
        total_facts <- length(session$store$list_facts())
        total_sources <- length(session$store$list_sources())
        tempest_inform(
          "SimulatedUser: {self$turn_count} turns, {total_facts} facts, {total_sources} sources"
        )
      }

      invisible(session)
    }
  )
)
