# vitals evaluation helpers

#' Load an included evaluation dataset
#'
#' @param name Dataset name. Currently supports "qa".
#' @return A tibble with columns `input` and `target`.
#' @keywords internal
stormr_eval_dataset <- function(name = c("qa")) {
  name <- match.arg(name)
  path <- system.file("extdata", "evals", paste0(name, ".csv"), package = "stormr")
  if (identical(path, "")) stormr_abort("Eval dataset not found in installed package.")
  read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

#' A vitals solver that answers questions using retrieval + citations
#'
#' This solver is designed to evaluate the end-to-end behavior of the stormr
#' retrieval + citation discipline. It is intentionally lighter than a full
#' `storm_run()` call.
#'
#' @param input Character vector of questions.
#' @param config A `StormConfig`.
#' @param solver_chat Optional (ignored). Present to match vitals conventions.
#' @return A list with `result` and `solver_chat` (and `solver_metadata`).
#' @keywords internal
stormr_solver_cited_answer <- function(input, config = storm_config(), solver_chat = NULL) {
  stormr_require("ellmer", "stormr_solver_cited_answer() requires ellmer.")
  n <- length(input)
  results <- character(n)
  chats <- vector("list", n)
  meta <- vector("list", n)

  for (i in seq_len(n)) {
    store <- SourceStore$new()
    retriever <- storm_retriever(config = config, store = store)
    chat <- config$make_chat("expert", system_prompt = storm_prompt("qa_solver_system"), echo = "none")
    storm_register_default_tools(
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

#' Create a vitals Task for stormr
#'
#' @param dataset Which built-in dataset to use. Currently "qa".
#' @param solver A vitals-compatible solver. Defaults to `stormr_solver_cited_answer`.
#' @param scorer A vitals scorer. If `NULL`, defaults to `vitals::model_graded_qa()`.
#' @param scorer_chat Chat used by the scorer (required for model-graded scoring).
#' @param config A `StormConfig` passed to the solver.
#' @param ... Passed to `vitals::Task$new()`.
#' @return A `vitals::Task`.
#' @export
stormr_task <- function(
  dataset = c("qa"),
  solver = stormr_solver_cited_answer,
  scorer = NULL,
  scorer_chat = NULL,
  config = storm_config(),
  ...
) {
  stormr_require("vitals", "stormr_task() requires vitals.")
  stormr_require("ellmer", "stormr_task() requires ellmer (solver + scorer).")
  dataset <- match.arg(dataset)
  ds <- stormr_eval_dataset(dataset)

  if (is.null(scorer)) {
    if (is.null(scorer_chat)) {
      stormr_abort("Provide scorer_chat (an ellmer Chat) or set scorer explicitly.")
    }
    scorer <- vitals::model_graded_qa(partial_credit = TRUE, scorer_chat = scorer_chat)
  }

  vitals::Task$new(
    dataset = ds,
    solver = function(input, ...) solver(input = input, config = config, ...),
    scorer = scorer,
    name = paste0("stormr-", dataset),
    ...
  )
}
