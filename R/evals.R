# vitals evaluation helpers

#' Load an included evaluation dataset
#'
#' @param name Dataset name. Currently supports "qa".
#' @return A tibble with columns `input` and `target`.
#' @keywords internal
tempest_eval_dataset <- function(name = c("qa")) {
  name <- match.arg(name)
  path <- system.file(
    "extdata",
    "evals",
    paste0(name, ".csv"),
    package = "tempest"
  )
  if (identical(path, "")) {
    tempest_abort("Eval dataset not found in installed package.")
  }
  read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

#' A vitals solver that answers questions using retrieval + citations
#'
#' This solver is designed to evaluate the end-to-end behavior of the tempest
#' retrieval + citation discipline. It is intentionally lighter than a full
#' `tempest_run()` call.
#'
#' @param input Character vector of questions.
#' @param config A `TempestConfig`.
#' @param solver_chat Optional (ignored). Present to match vitals conventions.
#' @return A list with `result` and `solver_chat` (and `solver_metadata`).
#' @keywords internal
tempest_solver_cited_answer <- function(
  input,
  config = tempest_config(),
  solver_chat = NULL
) {
  tempest_require("ellmer", "tempest_solver_cited_answer() requires ellmer.")
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  meta <- vector("list", n)

  for (i in seq_len(n)) {
    store <- SourceStore$new()
    retriever <- tempest_retriever(config = config, store = store)
    chat <- tempest_make_chat(
      config,
      "expert",
      system_prompt = tempest_prompt("qa_solver_system"),
      echo = "none"
    )
    tempest_register_default_tools(
      chat,
      retriever,
      model = config@models[["expert"]],
      search_provider = config@search_provider
    )

    q <- input[[i]]
    prompt <- paste0(
      "Answer the question using tools.\n\n",
      "Question: ",
      q,
      "\n\n",
      "Rules:\n",
      "- Use web_search + fetch_url.\n",
      "- Cite sources using [Sxxxxxxxxxxxx] after factual claims.\n",
      "- If you are uncertain, say so.\n",
      "- Keep the answer under 200 words.\n"
    )
    ans <- chat$chat(prompt, echo = "none")

    results[[i]] <- ans
    chats[[i]] <- chat
    meta[[i]] <- list(
      sources = store$to_tibbles()$sources,
      claims = store$to_tibbles()$claims
    )
  }

  list(result = results, solver_chat = chats, solver_metadata = meta)
}

#' Create a vitals Task for tempest
#'
#' @param dataset Which built-in dataset to use. Currently "qa".
#' @param solver A vitals-compatible solver. Defaults to `tempest_solver_cited_answer`.
#' @param scorer A vitals scorer. If `NULL`, defaults to `vitals::model_graded_qa()`.
#' @param scorer_chat Chat used by the scorer (required for model-graded scoring).
#' @param config A `TempestConfig` passed to the solver.
#' @param ... Passed to `vitals::Task$new()`.
#' @return A `vitals::Task`.
#' @examples
#' \dontrun{
#' scorer_chat <- ellmer::chat("openai/gpt-5-mini")
#' task <- tempest_task(scorer_chat = scorer_chat, config = tempest_config())
#' task$eval()
#' }
#' @export
tempest_task <- function(
  dataset = c("qa"),
  solver = tempest_solver_cited_answer,
  scorer = NULL,
  scorer_chat = NULL,
  config = tempest_config(),
  ...
) {
  tempest_require("vitals", "tempest_task() requires vitals.")
  tempest_require("ellmer", "tempest_task() requires ellmer (solver + scorer).")
  dataset <- match.arg(dataset)
  ds <- tempest_eval_dataset(dataset)

  if (is.null(scorer)) {
    if (is.null(scorer_chat)) {
      tempest_abort(
        "Provide scorer_chat (an ellmer Chat) or set scorer explicitly."
      )
    }
    scorer <- vitals::model_graded_qa(
      partial_credit = TRUE,
      scorer_chat = scorer_chat
    )
  }

  vitals::Task$new(
    dataset = ds,
    solver = function(input, ...) solver(input = input, config = config, ...),
    scorer = scorer,
    name = paste0("tempest-", dataset),
    ...
  )
}

#' Create a Co-STORM evaluation task using SimulatedUser
#'
#' Runs automated Co-STORM sessions with a simulated user for evaluation.
#'
#' @param dataset Which built-in dataset to use. Currently "qa".
#' @param config A `TempestConfig`.
#' @param max_turns Maximum turns per simulated session.
#' @param ... Passed to `vitals::Task$new()`.
#' @return A `vitals::Task`.
#' @examples
#' \dontrun{
#' task <- tempest_costorm_task(config = tempest_config(), max_turns = 5)
#' task$eval()
#' }
#' @export
tempest_costorm_task <- function(
  dataset = c("qa"),
  config = tempest_config(),
  max_turns = 5L,
  ...
) {
  tempest_require("vitals", "tempest_costorm_task() requires vitals.")
  tempest_require("ellmer", "tempest_costorm_task() requires ellmer.")
  dataset <- match.arg(dataset)
  ds <- tempest_eval_dataset(dataset)

  solver <- function(input, ...) {
    n <- length(input)
    results <- character(n)
    chats <- vector("list", n)
    meta <- vector("list", n)

    for (i in seq_len(n)) {
      topic <- input[[i]]
      session <- tempest_session(topic, config = config, n_experts = 2)
      sim_user <- SimulatedUser$new(
        topic,
        config = config,
        max_turns = max_turns
      )
      sim_user$run_session(session, warmup = FALSE, verbose = FALSE)

      report <- session$report(style = "technical", include_references = FALSE)
      results[[i]] <- report
      chats[[i]] <- session$chats$moderator
      meta[[i]] <- list(
        turns = sim_user$turn_count,
        sources = session$store$to_tibbles()$sources,
        claims = session$store$to_tibbles()$claims
      )
    }

    list(result = results, solver_chat = chats, solver_metadata = meta)
  }

  scorer_chat <- tempest_make_chat(config, "judge", echo = "none")
  scorer <- vitals::model_graded_qa(
    partial_credit = TRUE,
    scorer_chat = scorer_chat
  )

  vitals::Task$new(
    dataset = ds,
    solver = solver,
    scorer = scorer,
    name = paste0("tempest-costorm-", dataset),
    ...
  )
}
