# dsprrr module factory & optimization

tempest_dsprrr_run <- function(...) {
  dsprrr::run(...)
}

#' @keywords internal
tempest_run_dsprrr_module <- function(module, chat, inputs, step) {
  result <- tempest_run_dsprrr_module_structured(module, chat, inputs, step)
  if (is.null(result)) {
    return(NULL)
  }
  result$output
}

#' @keywords internal
tempest_run_dsprrr_module_structured <- function(module, chat, inputs, step) {
  if (is.null(module)) {
    return(NULL)
  }

  execution <- if (inherits(module, "tempest_dsprrr_execution")) {
    module
  } else {
    tempest_dsprrr_execution(
      program = module,
      program_artifact_id = tempest_program_reference(
        module
      )$program_artifact_id,
      trace_context = list()
    )
  }
  program <- execution$program
  trace_context <- execution$trace_context

  result <- tryCatch(
    do.call(
      tempest_dsprrr_run,
      c(
        list(module = program),
        inputs,
        list(
          .llm = chat,
          .return_format = "structured",
          .progress = FALSE,
          .trace_context = trace_context
        )
      )
    ),
    error = function(e) {
      if (tempest_dsprrr_contract_condition(e)) {
        stop(e)
      }
      tempest_warn(
        "dsprrr {step} failed, falling back to ellmer: {conditionMessage(e)}"
      )
      NULL
    }
  )
  if (is.null(result)) {
    return(NULL)
  }
  if (!inherits(result, "dsprrr_result") || !is.list(result$metadata)) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution did not return structured verification metadata."
    )
  }

  expected_program_artifact_id <- execution$program_artifact_id
  actual_program_artifact_id <- result$metadata$program_artifact_id %||% NULL
  if (!identical(actual_program_artifact_id, expected_program_artifact_id)) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution metadata does not match the bound program artifact."
    )
  }
  if (!identical(result$metadata$trace_context %||% list(), trace_context)) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution metadata does not match the bound Tempest trace context."
    )
  }
  result
}

tempest_dsprrr_contract_condition <- function(condition) {
  classes <- class(condition)
  inherits(condition, "tempest_ecosystem_contract_error") ||
    inherits(condition, "dsprrr_trace_context_error") ||
    inherits(condition, "dsprrr_trace_contract_error") ||
    inherits(condition, "dsprrr_program_trace_contract_error") ||
    any(grepl("^dsprrr_(artifact_|program_artifact_)", classes))
}

#' Create dsprrr modules for structured steps
#'
#' Creates dsprrr modules for STORM structured extraction/generation steps.
#'
#' @param config A `TempestConfig` object.
#' @return A named list of dsprrr modules, or `NULL` if module creation fails.
#' @keywords internal
tempest_make_dsprrr_modules <- function(config) {
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
        extract_claims = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("answer_text", "string"),
              dsprrr::input(
                "source_context",
                "string",
                "Known source ids, titles, and URLs available to cite."
              ),
              dsprrr::input(
                "source_ids",
                "string",
                "Source ids attached to this answer turn, one per line."
              ),
              dsprrr::input(
                "citation_mode",
                "string",
                "Citation mode: tempest_inline, provider_native, url, or mixed."
              )
            ),
            output_type = facts_type,
            instructions = paste(
              "Extract atomic factual claims from the answer.",
              "Only extract claims explicitly supported by citations or source annotations.",
              "When source_context is empty, only use explicit citations like [Sxxxxxxxxxxxx].",
              "When source_context is present, return only source_id values listed there.",
              "Use source_ids as the set of provider-native sources attached to this turn.",
              "Do not use a known source unless the answer text or provider-native turn context supports the claim.",
              "Include support_score in [0,1] when source support is clear; omit it when unscored.",
              "Do not infer or invent facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        verify_claim_support = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("claim_text", "string"),
              dsprrr::input("source_excerpts", "string")
            ),
            output_type = tempest_type_verification(),
            instructions = paste(
              "Judge whether the cited source excerpts support the claim.",
              "Return a status, a support score in [0,1], and a short rationale.",
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
              dsprrr::input("report_title", "string")
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
              dsprrr::input("report_title", "string"),
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
    selected <- teleprompter[[module_name]] %||% teleprompter$.default
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
tempest_validate_dsprrr_teleprompter <- function(teleprompter, module_names) {
  if (
    is.null(teleprompter) ||
      is.function(teleprompter) ||
      inherits(teleprompter, "dsprrr::Teleprompter")
  ) {
    return(teleprompter)
  }
  valid <- is.list(teleprompter) &&
    !is.data.frame(teleprompter) &&
    (length(teleprompter) == 0L ||
      (!is.null(names(teleprompter)) &&
        !anyNA(names(teleprompter)) &&
        all(nzchar(names(teleprompter))) &&
        !anyDuplicated(names(teleprompter))))
  if (!valid) {
    tempest_dsprrr_abort(
      "{.arg teleprompter} must be a dsprrr teleprompter, factory, or named list.",
      class = "tempest_dsprrr_optimization_error"
    )
  }

  unknown <- setdiff(names(teleprompter), c(".default", module_names))
  if (length(unknown) > 0L) {
    tempest_dsprrr_abort(
      c(
        "{.arg teleprompter} contains unknown module names.",
        x = "Unknown names: {.val {unknown}}."
      ),
      class = "tempest_dsprrr_optimization_error"
    )
  }
  invalid <- names(teleprompter)[
    !vapply(
      teleprompter,
      function(value) {
        is.function(value) || inherits(value, "dsprrr::Teleprompter")
      },
      logical(1)
    )
  ]
  if (length(invalid) > 0L) {
    tempest_dsprrr_abort(
      c(
        "{.arg teleprompter} entries must be teleprompters or factories.",
        x = "Invalid entries: {.val {invalid}}."
      ),
      class = "tempest_dsprrr_optimization_error"
    )
  }
  teleprompter
}

#' @keywords internal
tempest_validate_dsprrr_compile_arg_set <- function(args) {
  if (
    !is.list(args) ||
      is.data.frame(args) ||
      (length(args) > 0L &&
        (is.null(names(args)) ||
          anyNA(names(args)) ||
          any(!nzchar(names(args))) ||
          anyDuplicated(names(args))))
  ) {
    tempest_dsprrr_abort(
      "Each {.arg compile_args} entry must be a named list.",
      class = "tempest_dsprrr_optimization_error"
    )
  }

  reserved <- c("program", "teleprompter", "trainset", "valset", ".llm")
  conflicts <- intersect(names(args), reserved)
  if (length(conflicts) > 0L) {
    tempest_dsprrr_abort(
      c(
        "{.arg compile_args} cannot replace Tempest-owned compilation arguments.",
        x = "Reserved arguments: {.val {conflicts}}."
      ),
      class = "tempest_dsprrr_optimization_error"
    )
  }
  args
}

#' @keywords internal
tempest_validate_dsprrr_compile_args <- function(compile_args, module_names) {
  if (is.null(compile_args)) {
    return(list())
  }
  valid <- is.list(compile_args) &&
    !is.data.frame(compile_args) &&
    (length(compile_args) == 0L ||
      (!is.null(names(compile_args)) &&
        !anyNA(names(compile_args)) &&
        all(nzchar(names(compile_args))) &&
        !anyDuplicated(names(compile_args))))
  if (!valid) {
    tempest_dsprrr_abort(
      "{.arg compile_args} must be a named list keyed by {.val .default} or module name.",
      class = "tempest_dsprrr_optimization_error"
    )
  }

  unknown <- setdiff(names(compile_args), c(".default", module_names))
  if (length(unknown) > 0L) {
    tempest_dsprrr_abort(
      c(
        "{.arg compile_args} contains unknown module names.",
        x = "Unknown names: {.val {unknown}}.",
        i = "Available modules: {.val {module_names}}."
      ),
      class = "tempest_dsprrr_optimization_error"
    )
  }
  lapply(compile_args, tempest_validate_dsprrr_compile_arg_set)
}

#' @keywords internal
tempest_select_dsprrr_compile_args <- function(compile_args, module_name) {
  if (is.null(compile_args)) {
    return(list())
  }
  if (!is.list(compile_args) || is.data.frame(compile_args)) {
    tempest_dsprrr_abort(
      "{.arg compile_args} must be a named list.",
      class = "tempest_dsprrr_optimization_error"
    )
  }
  default <- tempest_validate_dsprrr_compile_arg_set(
    compile_args$.default %||% list()
  )
  specific <- tempest_validate_dsprrr_compile_arg_set(
    compile_args[[module_name]] %||% list()
  )
  c(default[setdiff(names(default), names(specific))], specific)
}

#' @keywords internal
tempest_normalize_dsprrr_dataset <- function(data) {
  for (name in names(data)) {
    if (inherits(data[[name]], "AsIs")) {
      data[[name]] <- unclass(data[[name]])
    }
  }
  data
}

#' @keywords internal
tempest_dsprrr_error_class <- function(specific = character()) {
  unique(c(specific, "tempest_dsprrr_error", "tempest_error"))
}

#' @keywords internal
tempest_dsprrr_bundle_error_class <- function(specific = character()) {
  unique(c(
    specific,
    "tempest_dsprrr_bundle_error",
    "tempest_dsprrr_error",
    "tempest_persistence_error",
    "tempest_error"
  ))
}

#' @keywords internal
tempest_dsprrr_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    ...,
    class = tempest_dsprrr_error_class(class),
    parent = parent,
    .envir = .envir
  )
}

#' @keywords internal
tempest_dsprrr_bundle_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL,
  .envir = rlang::caller_env()
) {
  tempest_abort(
    message,
    ...,
    class = tempest_dsprrr_bundle_error_class(class),
    parent = parent,
    .envir = .envir
  )
}

#' @keywords internal
tempest_require_dsprrr_artifacts <- function() {
  required <- c("program_artifact", "restore_module_config")
  missing <- setdiff(required, getNamespaceExports("dsprrr"))
  if (length(missing) > 0L) {
    tempest_dsprrr_abort(c(
      "The installed dsprrr is too old for Tempest program bundles.",
      i = "Install the dsprrr revision declared in Tempest's {.file DESCRIPTION}.",
      x = "Missing APIs: {.val {missing}}."
    ))
  }
  invisible(TRUE)
}

#' @keywords internal
tempest_validate_dsprrr_modules <- function(modules) {
  valid_names <- is.list(modules) &&
    !is.data.frame(modules) &&
    length(modules) > 0L &&
    !is.null(names(modules)) &&
    !anyNA(names(modules)) &&
    all(nzchar(names(modules))) &&
    !anyDuplicated(names(modules))
  if (!valid_names) {
    tempest_dsprrr_abort(
      "{.arg modules} must be a non-empty named list with unique names."
    )
  }

  invalid <- names(modules)[
    !vapply(
      modules,
      inherits,
      logical(1),
      what = "Module"
    )
  ]
  if (length(invalid) > 0L) {
    tempest_dsprrr_abort(c(
      "Every entry in {.arg modules} must be a dsprrr Module.",
      x = "Invalid entries: {.val {invalid}}."
    ))
  }
  modules
}

#' @keywords internal
tempest_dsprrr_bundle_path <- function(path) {
  if (!rlang::is_string(path) || !nzchar(path)) {
    tempest_dsprrr_bundle_abort(
      "{.arg path} must be a single non-empty path string."
    )
  }
  if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
    tempest_dsprrr_bundle_abort(c(
      "{.arg path} must end in {.file .rds}.",
      i = "Choose a bundle path such as {.path storm-programs.rds}."
    ))
  }
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

#' @keywords internal
tempest_dsprrr_bundle_digest <- function(bundle) {
  payload <- unclass(bundle)
  payload$integrity <- NULL
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

#' @keywords internal
tempest_validate_dsprrr_bundle <- function(bundle) {
  expected_fields <- c(
    "bundle_type",
    "schema_version",
    "dsprrr_version",
    "created_at",
    "programs",
    "integrity"
  )
  if (
    !is.list(bundle) ||
      is.data.frame(bundle) ||
      !identical(names(bundle), expected_fields)
  ) {
    tempest_dsprrr_bundle_abort(
      "Tempest dsprrr bundle is malformed or unsupported."
    )
  }
  programs <- bundle$programs
  valid_programs <- is.list(programs) &&
    !is.data.frame(programs) &&
    length(programs) > 0L &&
    !is.null(names(programs)) &&
    !anyNA(names(programs)) &&
    all(nzchar(names(programs))) &&
    !anyDuplicated(names(programs)) &&
    all(vapply(
      programs,
      inherits,
      logical(1),
      what = "dsprrr_program_artifact"
    ))
  valid <- inherits(bundle, "tempest_dsprrr_program_bundle") &&
    identical(bundle$bundle_type %||% "", "tempest_dsprrr_programs") &&
    identical(bundle$schema_version, 1L) &&
    rlang::is_string(bundle$dsprrr_version) &&
    rlang::is_string(bundle$created_at) &&
    rlang::is_string(bundle$integrity) &&
    grepl("^[a-f0-9]{64}$", bundle$integrity) &&
    valid_programs
  if (!valid) {
    tempest_dsprrr_bundle_abort(
      "Tempest dsprrr bundle is malformed or unsupported."
    )
  }
  actual <- tempest_dsprrr_bundle_digest(bundle)
  if (!identical(actual, bundle$integrity)) {
    tempest_dsprrr_bundle_abort(
      "Tempest dsprrr bundle checksum validation failed."
    )
  }
  invisible(bundle)
}

#' @keywords internal
tempest_read_dsprrr_bundle <- function(path) {
  if (!file.exists(path)) {
    tempest_dsprrr_bundle_abort(
      "Tempest dsprrr bundle does not exist: {.path {path}}."
    )
  }
  bundle <- tryCatch(
    readRDS(path),
    error = function(error) {
      tempest_dsprrr_bundle_abort(
        "Could not read Tempest dsprrr bundle: {.path {path}}.",
        parent = error
      )
    }
  )
  tempest_validate_dsprrr_bundle(bundle)
  bundle
}

#' @keywords internal
tempest_atomic_save_dsprrr_bundle <- function(bundle, path, overwrite) {
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    tempest_dsprrr_bundle_abort("{.arg overwrite} must be TRUE or FALSE.")
  }
  if (dir.exists(path)) {
    tempest_dsprrr_bundle_abort(
      "Tempest dsprrr bundle path is a directory: {.path {path}}."
    )
  }
  if (file.exists(path)) {
    if (!isTRUE(overwrite)) {
      tempest_dsprrr_bundle_abort(c(
        "Tempest dsprrr bundle already exists.",
        i = "Use {.code overwrite = TRUE} to replace it.",
        x = "Path: {.path {path}}."
      ))
    }
    tempest_read_dsprrr_bundle(path)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile(
    pattern = paste0(".", basename(path), "-staging-"),
    tmpdir = dirname(path)
  )
  installed <- FALSE
  on.exit(
    if (!installed && file.exists(staging)) {
      unlink(staging)
    },
    add = TRUE
  )
  saveRDS(bundle, staging, version = 3L)
  tempest_read_dsprrr_bundle(staging)

  backup <- NULL
  if (file.exists(path)) {
    backup <- tempfile(
      pattern = paste0(".", basename(path), "-backup-"),
      tmpdir = dirname(path)
    )
    if (!file.rename(path, backup)) {
      tempest_dsprrr_bundle_abort(
        "Could not stage the previous dsprrr bundle for replacement."
      )
    }
  }

  if (!file.rename(staging, path)) {
    if (!is.null(backup)) {
      file.rename(backup, path)
    }
    tempest_dsprrr_bundle_abort(
      "Could not atomically install the completed dsprrr bundle."
    )
  }
  if (!is.null(backup)) {
    unlink(backup)
  }
  installed <- TRUE
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Save compiled dsprrr modules
#'
#' `r lifecycle::badge("experimental")`
#'
#' Saves every module as a dsprrr versioned program artifact inside one
#' checksummed Tempest bundle. Runtime chats, credentials, caches, and execution
#' history are not persisted. The completed bundle is installed atomically.
#'
#' @param modules A non-empty named list of dsprrr modules.
#' @param path Program bundle file ending in `.rds`.
#' @param registry Named runtime registry passed to
#'   [dsprrr::program_artifact()].
#' @param trusted Whether dsprrr may embed trusted runtime values. The safer
#'   default is `FALSE`; prefer stable registry IDs.
#' @param overwrite Whether to replace an existing Tempest dsprrr bundle.
#' @return Invisibly returns the normalized bundle path.
#' @examples
#' \dontrun{
#' modules <- tempest_optimize_dsprrr_modules(
#'   trainsets = list(query_decomposition = trainset)
#' )
#' path <- tempest_save_dsprrr_modules(modules, "storm-programs.rds")
#' }
#' @export
tempest_save_dsprrr_modules <- function(
  modules,
  path,
  registry = list(),
  trusted = FALSE,
  overwrite = FALSE
) {
  tempest_require_dsprrr_artifacts()
  modules <- tempest_validate_dsprrr_modules(modules)
  path <- tempest_dsprrr_bundle_path(path)
  programs <- lapply(names(modules), function(name) {
    tryCatch(
      dsprrr::program_artifact(
        modules[[name]],
        registry = registry,
        trusted = trusted
      ),
      error = function(error) {
        tempest_dsprrr_bundle_abort(
          "Could not serialize dsprrr program {.val {name}}.",
          parent = error
        )
      }
    )
  })
  names(programs) <- names(modules)
  bundle <- structure(
    list(
      bundle_type = "tempest_dsprrr_programs",
      schema_version = 1L,
      dsprrr_version = as.character(utils::packageVersion("dsprrr")),
      created_at = tempest_now_utc(),
      programs = programs
    ),
    class = c("tempest_dsprrr_program_bundle", "list")
  )
  bundle$integrity <- tempest_dsprrr_bundle_digest(bundle)
  tempest_atomic_save_dsprrr_bundle(bundle, path, overwrite = overwrite)
}

#' Load compiled dsprrr modules
#'
#' `r lifecycle::badge("experimental")`
#'
#' Validates the bundle checksum before asking dsprrr to validate and restore
#' each versioned program artifact.
#'
#' @param path Program bundle file created by
#'   [tempest_save_dsprrr_modules()].
#' @param registry Named runtime registry passed to
#'   [dsprrr::restore_module_config()].
#' @param trusted Whether dsprrr may restore embedded trusted runtime values.
#' @return A named list of dsprrr modules.
#' @examples
#' \dontrun{
#' modules <- tempest_load_dsprrr_modules("path/to/storm-programs.rds")
#' result <- tempest_run("History of jazz", dsprrr_modules = modules)
#' }
#' @export
tempest_load_dsprrr_modules <- function(
  path,
  registry = list(),
  trusted = FALSE
) {
  tempest_require_dsprrr_artifacts()
  path <- tempest_dsprrr_bundle_path(path)
  bundle <- tempest_read_dsprrr_bundle(path)
  programs <- bundle$programs
  modules <- lapply(names(programs), function(name) {
    tryCatch(
      suppressMessages(dsprrr::restore_module_config(
        programs[[name]],
        registry = registry,
        trusted = trusted
      )),
      error = function(error) {
        tempest_dsprrr_bundle_abort(
          "Could not load dsprrr program {.val {name}}.",
          parent = error
        )
      }
    )
  })
  names(modules) <- names(programs)
  tempest_validate_dsprrr_modules(modules)
}

#' Optimize STORM dsprrr modules
#'
#' `r lifecycle::badge("experimental")`
#'
#' Compiles selected STORM dsprrr modules against user-provided training data.
#' This is an explicit optimization step: run it with labeled examples, then
#' pass the returned module list to [tempest_run()] via `dsprrr_modules`.
#'
#' @param trainsets Named list of training data frames. Names must match module
#'   names from `tempest_make_dsprrr_modules()`, such as
#'   `"query_decomposition"`, `"extract_claims"`, `"section_writing"`, or
#'   `"lead_section"`.
#' @param modules Optional named list of modules to optimize. Defaults to fresh
#'   modules from `tempest_make_dsprrr_modules(config)`.
#' @param config A `TempestConfig` object used when `modules` is `NULL`.
#' @param teleprompter Optional dsprrr teleprompter, factory function, or named
#'   list keyed by module name with an optional `.default`. Defaults to
#'   `dsprrr::LabeledFewShot()`; an empty list also uses this default.
#' @param valsets Optional named list of validation data frames.
#' @param .llm Optional ellmer chat object passed to dsprrr compilation.
#' @param compile_args Named list of additional arguments for dsprrr
#'   compilation. Use `.default` for arguments shared by every module and a
#'   module name for overrides. This exposes dsprrr optimizer controls,
#'   checkpoints, agent chats, and sandbox runners without Tempest duplicating
#'   their contracts.
#' @param save_path Optional `.rds` path for an atomic, versioned program bundle.
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
#'   config = tempest_config(),
#'   compile_args = list(
#'     .default = list(
#'       control = dsprrr::optimizer_control(max_provider_calls = 100L)
#'     )
#'   ),
#'   save_path = "storm-programs.rds"
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
  compile_args = list(),
  save_path = NULL,
  k = 4L,
  seed = 123L,
  strict = TRUE,
  verbose = TRUE
) {
  tempest_require("dsprrr", "Optimizing STORM modules requires dsprrr.")

  valid_trainsets <- is.list(trainsets) &&
    !is.data.frame(trainsets) &&
    length(trainsets) > 0L &&
    !is.null(names(trainsets)) &&
    !anyNA(names(trainsets)) &&
    all(nzchar(names(trainsets))) &&
    !anyDuplicated(names(trainsets))
  if (!valid_trainsets) {
    tempest_dsprrr_abort(
      "{.arg trainsets} must be a non-empty named list with unique names.",
      class = "tempest_dsprrr_optimization_error"
    )
  }

  modules <- modules %||% tempest_make_dsprrr_modules(config)
  if (is.null(modules) || !is.list(modules) || is.null(names(modules))) {
    tempest_abort("No dsprrr modules are available to optimize.")
  }
  modules <- tempest_validate_dsprrr_modules(modules)
  teleprompter <- tempest_validate_dsprrr_teleprompter(
    teleprompter,
    names(modules)
  )
  compile_args <- tempest_validate_dsprrr_compile_args(
    compile_args,
    names(modules)
  )
  if (
    !is.null(valsets) &&
      (!is.list(valsets) ||
        is.data.frame(valsets) ||
        is.null(names(valsets)) ||
        anyNA(names(valsets)) ||
        any(!nzchar(names(valsets))) ||
        anyDuplicated(names(valsets)))
  ) {
    tempest_dsprrr_abort(
      "{.arg valsets} must be NULL or a named list.",
      class = "tempest_dsprrr_optimization_error"
    )
  }
  unknown_valsets <- setdiff(names(valsets), names(modules))
  if (length(unknown_valsets) > 0L) {
    tempest_dsprrr_abort(
      c(
        "{.arg valsets} contains unknown module names.",
        x = "Unknown names: {.val {unknown_valsets}}."
      ),
      class = "tempest_dsprrr_optimization_error"
    )
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
    trainset <- tempest_normalize_dsprrr_dataset(trainset)

    tp <- tempest_select_dsprrr_teleprompter(
      teleprompter,
      module_name = module_name,
      module = modules[[module_name]],
      trainset = trainset,
      k = k,
      seed = seed
    )
    valset <- valsets[[module_name]] %||% NULL
    if (!is.null(valset)) {
      if (!is.data.frame(valset)) {
        valset <- tryCatch(
          as.data.frame(valset),
          error = function(error) {
            tempest_dsprrr_abort(
              c(
                "Validation set for {.val {module_name}} must be a data frame.",
                x = conditionMessage(error)
              ),
              class = "tempest_dsprrr_optimization_error"
            )
          }
        )
      }
      valset <- tempest_normalize_dsprrr_dataset(valset)
    }
    module_compile_args <- tempest_select_dsprrr_compile_args(
      compile_args,
      module_name
    )

    if (isTRUE(verbose)) {
      tempest_inform(
        "Optimizing dsprrr module {.val {module_name}} with {nrow(trainset)} examples"
      )
    }

    optimized_module <- tryCatch(
      do.call(
        dsprrr::compile_module,
        c(
          list(
            program = modules[[module_name]],
            teleprompter = tp,
            trainset = trainset,
            valset = valset,
            .llm = .llm
          ),
          module_compile_args
        )
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
        isTRUE(optimized_module$is_compiled()),
      compile_args = names(module_compile_args),
      summary = tryCatch(
        dsprrr::optimization_summary(optimized_module),
        error = function(error) NULL
      )
    )
  }

  attr(optimized, "tempest_dsprrr_optimization") <- compiled
  if (!is.null(save_path)) {
    tempfile_path <- tempest_save_dsprrr_modules(optimized, save_path)
    attr(optimized, "tempest_dsprrr_modules_path") <- tempfile_path
  }

  optimized
}
