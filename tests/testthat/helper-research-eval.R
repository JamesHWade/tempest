research_eval_test_context <- local({
  context <- NULL

  function() {
    if (!is.null(context)) {
      return(context)
    }
    root <- tryCatch(
      normalizePath(testthat::test_path("..", ".."), mustWork = TRUE),
      error = function(error) normalizePath(getwd(), mustWork = TRUE)
    )
    script <- file.path(root, "tools", "research-eval.R")
    if (!file.exists(script)) {
      return(NULL)
    }
    env <- new.env(parent = globalenv())
    sys.source(script, envir = env)
    context <<- list(
      eval = env,
      corpus_path = file.path(
        root,
        "inst",
        "extdata",
        "research-eval",
        "corpus.json"
      )
    )
    context
  }
})

research_eval_chat_class <- R6::R6Class(
  "Chat",
  public = list(
    turns = NULL,
    chat_structured_fn = NULL,
    initialize = function(chat_structured, model) {
      self$chat_structured_fn <- chat_structured
      self$turns <- list()
      private$model <- ellmer::Model(name = model, extra_args = list())
      private$provider <- ellmer::Provider(name = "offline-test", base_url = "")
    },
    chat_structured = function(prompt, type = NULL, ...) {
      self$chat_structured_fn(prompt, type, ...)
    },
    chat = function(...) "offline test response",
    stream = function(...) character(),
    get_turns = function(...) self$turns,
    set_turns = function(turns, ...) {
      self$turns <- turns
      invisible(self)
    },
    last_turn = function(...) {
      if (length(self$turns) == 0L) NULL else self$turns[[length(self$turns)]]
    },
    get_model = function() private$model@name,
    get_provider = function() private$provider,
    get_model_object = function() private$model,
    get_tokens = function() {
      data.frame(
        input = numeric(),
        output = numeric(),
        cached_input = numeric(),
        cost = numeric()
      )
    }
  ),
  private = list(
    model = NULL,
    provider = NULL
  ),
  lock_objects = FALSE,
  parent_env = globalenv()
)

research_eval_fake_chat <- function(chat_structured, model) {
  research_eval_chat_class$new(chat_structured, model)
}
