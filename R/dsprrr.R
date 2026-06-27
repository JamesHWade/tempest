# dsprrr module factory & optimization

#' @keywords internal
tempest_run_dsprrr_module <- function(module, chat, inputs, step) {
  if (is.null(module)) {
    return(NULL)
  }

  tryCatch(
    do.call(
      dsprrr::run,
      c(
        list(module = module),
        inputs,
        list(.llm = chat, .return_format = "simple", .progress = FALSE)
      )
    ),
    error = function(e) {
      tempest_warn(
        "dsprrr {step} failed, falling back to ellmer: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

#' Create dsprrr modules for structured steps
#'
#' Creates dsprrr modules for STORM structured extraction/generation steps.
#'
#' @param config A `TempestConfig` object.
#' @return A named list of dsprrr modules, or `NULL` if module creation fails.
#' @keywords internal
tempest_make_dsprrr_modules <- function(config) {
  if (!tempest_has("ellmer")) {
    return(NULL)
  }

  tryCatch(
    {
      query_type <- tempest_type_query_decomposition()
      personas_type <- tempest_type_personas()
      perspectives_type <- tempest_type_perspectives()
      facts_type <- tempest_type_fact_extract()
      outline_type <- tempest_type_outline()

      modules <- list(
        perspectives = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("seed_context", "string"),
              dsprrr::input("n_experts", "integer")
            ),
            output_type = perspectives_type,
            instructions = paste(
              "Plan a comprehensive STORM research report.",
              "Use seed sources and table-of-contents hints to discover distinct perspectives.",
              "Return a title and exactly n_experts perspectives.",
              "Each perspective needs 3-6 specific research questions.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        personas = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("n_experts", "integer"),
              dsprrr::input("requirements", "string")
            ),
            output_type = personas_type,
            instructions = paste(
              "Generate diverse expert personas for STORM multi-perspective research.",
              "The personas must be complementary and have non-overlapping focus areas.",
              "Return exactly n_experts personas.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        query_decomposition = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("question", "string"),
              dsprrr::input("topic", "string")
            ),
            output_type = query_type,
            instructions = paste(
              "Decompose the research question into 2-3 targeted web search queries.",
              "Queries should cover different aspects of the question and stay anchored to the topic.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        fact_extraction = dsprrr::module(
          dsprrr::signature(
            inputs = list(dsprrr::input("answer_text", "string")),
            output_type = facts_type,
            instructions = paste(
              "Extract atomic factual claims from the answer.",
              "Only extract claims explicitly supported by citations like [Sxxxxxxxxxxxx].",
              "Do not infer or invent facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        next_question = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("perspective", "string"),
              dsprrr::input("answered", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = tempest_type_next_question(),
            instructions = paste(
              "Choose the single most useful next question for this perspective.",
              "Set done to true only when the perspective is sufficiently covered.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        draft_outline = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("title", "string")
            ),
            output_type = outline_type,
            instructions = paste(
              "Create a preliminary STORM outline from parametric knowledge.",
              "Organize into 4-6 sections with subsections and bullet points.",
              "The outline will later be refined using verified facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        refined_outline = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("title", "string"),
              dsprrr::input("draft_outline", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = outline_type,
            instructions = paste(
              "Refine the draft outline using verified fact notes.",
              "Adjust, merge, add, or remove sections based on available evidence.",
              "Ensure sections are supportable by cited facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        section_writing = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("section_title", "string"),
              dsprrr::input("section_summary", "string"),
              dsprrr::input("subsections", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = ellmer::type_object(
              section_text = ellmer::type_string(
                "Markdown section text with citations"
              )
            ),
            instructions = paste(
              "Write one concise Markdown report section.",
              "Use only the provided verified facts.",
              "Every factual claim must include source citations like [Sxxxxxxxxxxxx].",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        lead_section = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("topic", "string"),
              dsprrr::input("title", "string"),
              dsprrr::input("article_body", "string"),
              dsprrr::input("facts", "string")
            ),
            output_type = ellmer::type_object(
              lead_section = ellmer::type_string(
                "A 2-3 paragraph lead section with citations"
              )
            ),
            instructions = paste(
              "Write a Wikipedia-style lead section for the article.",
              "It must be self-contained, summarize the most important points, and preserve citations.",
              sep = "\n"
            )
          ),
          type = "predict"
        )
      )
      modules
    },
    error = function(e) {
      tempest_warn("Failed to create dsprrr modules: {conditionMessage(e)}")
      NULL
    }
  )
}

#' @keywords internal
tempest_default_dsprrr_teleprompter <- function(
  trainset,
  k = 4L,
  seed = 123L
) {
  tempest_require("dsprrr", "dsprrr optimization requires dsprrr.")
  k <- as.integer(min(k, nrow(trainset)))
  if (is.na(k) || k < 1L) {
    k <- 1L
  }
  dsprrr::LabeledFewShot(k = k, seed = as.integer(seed))
}

#' @keywords internal
tempest_call_dsprrr_teleprompter_factory <- function(
  factory,
  module_name,
  module,
  trainset
) {
  args <- list(
    module_name = module_name,
    name = module_name,
    module = module,
    trainset = trainset
  )
  fmls <- names(formals(factory))
  if ("..." %in% fmls) {
    return(do.call(factory, args))
  }
  do.call(factory, args[intersect(names(args), fmls)])
}

#' @keywords internal
tempest_select_dsprrr_teleprompter <- function(
  teleprompter,
  module_name,
  module,
  trainset,
  k,
  seed
) {
  if (is.null(teleprompter)) {
    return(tempest_default_dsprrr_teleprompter(
      trainset,
      k = k,
      seed = seed
    ))
  }

  if (is.function(teleprompter)) {
    return(tempest_call_dsprrr_teleprompter_factory(
      teleprompter,
      module_name = module_name,
      module = module,
      trainset = trainset
    ))
  }

  if (
    is.list(teleprompter) && !inherits(teleprompter, "dsprrr::Teleprompter")
  ) {
    selected <- teleprompter[[module_name]] %||% teleprompter$default
    if (is.null(selected)) {
      return(tempest_default_dsprrr_teleprompter(
        trainset,
        k = k,
        seed = seed
      ))
    }
    if (is.function(selected)) {
      return(tempest_call_dsprrr_teleprompter_factory(
        selected,
        module_name = module_name,
        module = module,
        trainset = trainset
      ))
    }
    return(selected)
  }

  teleprompter
}

#' @keywords internal
tempest_dsprrr_modules_path <- function(path) {
  if (!rlang::is_string(path)) {
    tempest_abort(
      "{.arg path} must be a single string, not {.obj_type_friendly {path}}."
    )
  }
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(path)
  }
  file.path(path, "dsprrr-modules.rds")
}

#' Save compiled dsprrr modules
#'
#' @param modules A named list of dsprrr modules.
#' @param path File path ending in `.rds`, or a directory where
#'   `dsprrr-modules.rds` will be written.
#' @return Invisibly returns the RDS path.
#' @examples
#' \dontrun{
#' modules <- tempest_optimize_dsprrr_modules(
#'   trainsets = list(query_decomposition = trainset)
#' )
#' path <- tempest_save_dsprrr_modules(modules, tempfile(fileext = ".rds"))
#' }
#' @export
tempest_save_dsprrr_modules <- function(modules, path) {
  rds_path <- tempest_dsprrr_modules_path(path)
  dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(modules, rds_path)
  invisible(rds_path)
}

#' Load compiled dsprrr modules
#'
#' @param path File path ending in `.rds`, or a directory containing
#'   `dsprrr-modules.rds`.
#' @return A named list of dsprrr modules.
#' @examples
#' \dontrun{
#' modules <- tempest_load_dsprrr_modules("path/to/dsprrr-modules.rds")
#' result <- tempest_run("History of jazz", dsprrr_modules = modules)
#' }
#' @export
tempest_load_dsprrr_modules <- function(path) {
  rds_path <- tempest_dsprrr_modules_path(path)
  if (!file.exists(rds_path)) {
    tempest_abort(
      "Compiled dsprrr module file does not exist: {.path {rds_path}}"
    )
  }
  readRDS(rds_path)
}

#' Optimize STORM dsprrr modules
#'
#' Compiles selected STORM dsprrr modules against user-provided training data.
#' This is an explicit optimization step: run it with labeled examples, then
#' pass the returned module list to [tempest_run()] via `dsprrr_modules`.
#'
#' @param trainsets Named list of training data frames. Names must match module
#'   names from `tempest_make_dsprrr_modules()`, such as
#'   `"query_decomposition"`, `"fact_extraction"`, `"section_writing"`, or
#'   `"lead_section"`.
#' @param modules Optional named list of modules to optimize. Defaults to fresh
#'   modules from `tempest_make_dsprrr_modules(config)`.
#' @param config A `TempestConfig` object used when `modules` is `NULL`.
#' @param teleprompter Optional dsprrr teleprompter, named list of
#'   teleprompters, or factory function. Defaults to
#'   `dsprrr::LabeledFewShot()`.
#' @param valsets Optional named list of validation data frames.
#' @param .llm Optional ellmer chat object passed to dsprrr compilation.
#' @param save_dir Optional directory or `.rds` path for the optimized module
#'   list.
#' @param k Number of examples for the default `LabeledFewShot` teleprompter.
#' @param seed Random seed for the default teleprompter.
#' @param strict If `TRUE`, abort on the first compile failure. If `FALSE`,
#'   warn and keep the unoptimized module for that step.
#' @param verbose If `TRUE`, print optimization progress.
#' @return A named list of dsprrr modules.
#' @examples
#' \dontrun{
#' trainset <- data.frame(
#'   question = "What are battery recycling bottlenecks?",
#'   topic = "lithium batteries"
#' )
#' trainset$queries <- list(c("battery recycling", "EV battery capacity"))
#' modules <- tempest_optimize_dsprrr_modules(
#'   trainsets = list(query_decomposition = trainset),
#'   config = tempest_config()
#' )
#' }
#' @export
tempest_optimize_dsprrr_modules <- function(
  trainsets,
  modules = NULL,
  config = tempest_config(),
  teleprompter = NULL,
  valsets = NULL,
  .llm = NULL,
  save_dir = NULL,
  k = 4L,
  seed = 123L,
  strict = TRUE,
  verbose = TRUE
) {
  tempest_require("dsprrr", "Optimizing STORM modules requires dsprrr.")

  if (!is.list(trainsets) || is.null(names(trainsets))) {
    tempest_abort("{.arg trainsets} must be a named list of data frames.")
  }

  modules <- modules %||% tempest_make_dsprrr_modules(config)
  if (is.null(modules) || !is.list(modules) || is.null(names(modules))) {
    tempest_abort("No dsprrr modules are available to optimize.")
  }

  optimized <- modules
  compiled <- list()

  for (module_name in names(trainsets)) {
    if (!module_name %in% names(modules)) {
      msg <- c(
        "No dsprrr module named {.val {module_name}}.",
        i = "Available modules: {.val {names(modules)}}"
      )
      if (isTRUE(strict)) {
        tempest_abort(msg)
      }
      tempest_warn(msg)
      next
    }

    trainset <- trainsets[[module_name]]
    if (!is.data.frame(trainset)) {
      trainset <- tryCatch(
        as.data.frame(trainset),
        error = function(e) {
          tempest_abort(c(
            "Trainset for {.val {module_name}} must be a data frame.",
            x = conditionMessage(e)
          ))
        }
      )
    }
    if (nrow(trainset) == 0L) {
      tempest_warn(
        "Skipping {.val {module_name}} optimization: empty trainset."
      )
      next
    }

    tp <- tempest_select_dsprrr_teleprompter(
      teleprompter,
      module_name = module_name,
      module = modules[[module_name]],
      trainset = trainset,
      k = k,
      seed = seed
    )
    valset <- valsets[[module_name]] %||% NULL

    if (isTRUE(verbose)) {
      tempest_inform(
        "Optimizing dsprrr module {.val {module_name}} with {nrow(trainset)} examples"
      )
    }

    optimized_module <- tryCatch(
      dsprrr::compile_module(
        modules[[module_name]],
        tp,
        trainset,
        valset = valset,
        .llm = .llm
      ),
      error = function(e) {
        if (isTRUE(strict)) {
          tempest_abort(c(
            "Failed to optimize dsprrr module {.val {module_name}}.",
            x = conditionMessage(e)
          ))
        }
        tempest_warn(
          "Failed to optimize dsprrr module {.val {module_name}}; keeping original module: {conditionMessage(e)}"
        )
        modules[[module_name]]
      }
    )

    optimized[[module_name]] <- optimized_module
    compiled[[module_name]] <- list(
      n_train = nrow(trainset),
      teleprompter = class(tp)[1] %||% NA_character_,
      compiled = inherits(optimized_module, "Module") &&
        isTRUE(optimized_module$is_compiled())
    )
  }

  attr(optimized, "tempest_dsprrr_optimization") <- compiled
  if (!is.null(save_dir)) {
    tempfile_path <- tempest_save_dsprrr_modules(optimized, save_dir)
    attr(optimized, "tempest_dsprrr_modules_path") <- tempfile_path
  }

  optimized
}
