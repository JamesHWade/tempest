# vitals evaluation helpers

#' Load an included evaluation dataset
#'
#' @param name Dataset name. Currently supports "qa".
#' @return A tibble with columns `input` and `target`.
#' @keywords internal
tempest_eval_dataset <- function(name = c("qa")) {
  name <- match.arg(name)
  path <- system.file("extdata", "evals", paste0(name, ".csv"), package = "tempest")
  if (identical(path, "")) tempest_abort("Eval dataset not found in installed package.")
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
tempest_solver_cited_answer <- function(input, config = tempest_config(), solver_chat = NULL) {
  tempest_require("ellmer", "tempest_solver_cited_answer() requires ellmer.")
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  meta <- vector("list", n)

  for (i in seq_len(n)) {
    store <- SourceStore$new()
    retriever <- tempest_retriever(config = config, store = store)
    chat <- config$make_chat("expert", system_prompt = tempest_prompt("qa_solver_system"), echo = "none")
    tempest_register_default_tools(
      chat,
      retriever,
      model = config$models[["expert"]],
      search_provider = config$search_provider
    )

    q <- input[[i]]
    prompt <- paste0(
      "Answer the question using tools.\n\n",
      "Question: ", q, "\n\n",
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
      facts = store$to_tibbles()$facts
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
      tempest_abort("Provide scorer_chat (an ellmer Chat) or set scorer explicitly.")
    }
    scorer <- vitals::model_graded_qa(partial_credit = TRUE, scorer_chat = scorer_chat)
  }

  vitals::Task$new(
    dataset = ds,
    solver = function(input, ...) solver(input = input, config = config, ...),
    scorer = scorer,
    name = paste0("tempest-", dataset),
    ...
  )
}
