# dsprrr stage execution and builtin program factory

tempest_dsprrr_run <- function(...) {
  dsprrr::run(...)
}

tempest_dsprrr_run_async <- function(...) {
  dsprrr::run_async(...)
}

tempest_program_set_execution <- function(
  program_set,
  stage,
  trace_context = list()
) {
  if (!S7::S7_inherits(program_set, TempestProgramSet)) {
    tempest_ecosystem_contract_abort(
      "{.arg program_set} must be created by {.fn tempest_program_set}."
    )
  }
  stages <- tempest_program_set_stages()
  if (!rlang::is_string(stage) || !stage %in% stages) {
    tempest_ecosystem_contract_abort(
      "{.arg stage} must identify an exact Tempest ProgramSet stage."
    )
  }
  if ("program_artifact_id" %in% names(trace_context)) {
    tempest_ecosystem_contract_abort(
      paste0(
        "{.field program_artifact_id} is reserved by dsprrr and must not ",
        "appear in {.arg trace_context}."
      )
    )
  }
  state <- tempest_program_set_stage_state(program_set, stage)
  declared_reference <- state$entry
  program <- tempest_program_set_verify_program(
    program_set,
    stage,
    state = state
  )
  declared_program_artifact_id <- declared_reference$program_artifact_id
  tempest_dsprrr_execution(
    program = program,
    program_artifact_id = declared_program_artifact_id,
    trace_context = trace_context,
    stage = stage,
    contract_version = declared_reference$contract_version,
    evaluator_id = declared_reference$evaluator_id,
    evaluator_version = declared_reference$evaluator_version,
    governed_procedure_ref = declared_reference$governed_procedure_ref
  )
}

tempest_standalone_dsprrr_trace_context <- function(
  stage,
  knowledge_snapshot_id = NULL
) {
  context <- list(
    product = "tempest",
    research_run_id = tempest_uuid("standalone-program"),
    role = "program",
    stage = stage
  )
  if (!is.null(knowledge_snapshot_id)) {
    context$knowledge_snapshot_id <- knowledge_snapshot_id
  }
  tempest_research_manifest_canonical_value(context, "trace_context")
}

tempest_bind_program_set <- function(program_set, manifest) {
  programs <- tempest_program_set_programs(program_set)
  declared_references <- tempest_research_manifest_programs(
    tempest_program_set_entries(program_set)
  )
  if (
    !tempest_program_set_identity_equal(declared_references, manifest@programs)
  ) {
    tempest_ecosystem_contract_abort(
      "The ProgramSet entries do not match the research manifest."
    )
  }
  stages <- tempest_program_set_stages()
  stats::setNames(
    lapply(
      stages,
      function(stage) {
        reference <- declared_references[[stage]]
        tempest_dsprrr_execution(
          program = programs[[stage]],
          program_artifact_id = reference$program_artifact_id,
          trace_context = tempest_dsprrr_trace_context(
            manifest,
            stage,
            reference$program_artifact_id
          ),
          stage = stage,
          contract_version = reference$contract_version,
          evaluator_id = reference$evaluator_id,
          evaluator_version = reference$evaluator_version,
          governed_procedure_ref = reference$governed_procedure_ref
        )
      }
    ),
    stages
  )
}

#' @keywords internal
tempest_run_dsprrr_module <- function(module, chat, inputs, step) {
  result <- tempest_run_dsprrr_module_structured(module, chat, inputs, step)
  result$output
}

tempest_dsprrr_execution_require <- function(module, step) {
  if (!inherits(module, "tempest_dsprrr_execution")) {
    tempest_ecosystem_contract_abort(
      paste0(
        "The dsprrr program for {.val {step}} was not resolved from a ",
        "TempestProgramSet before execution."
      )
    )
  }
  module
}

tempest_dsprrr_execution_verify <- function(module, step) {
  execution <- tempest_dsprrr_execution_require(module, step)
  if (!identical(execution$stage, step)) {
    tempest_ecosystem_contract_abort(
      "The bound dsprrr stage does not match the requested execution stage."
    )
  }
  actual_program_artifact_id <- tempest_program_set_program_id(
    execution$program,
    execution$stage
  )
  if (!identical(actual_program_artifact_id, execution$program_artifact_id)) {
    tempest_program_set_abort(
      paste0(
        "The dsprrr program for {.val {step}} changed after ProgramSet ",
        "binding."
      ),
      class = "tempest_program_set_verification_error"
    )
  }
  execution
}

tempest_dsprrr_execution_governance_preflight <- function(
  execution,
  knowledge_view
) {
  execution <- tempest_dsprrr_execution_verify(
    execution,
    execution$stage %||% "unknown"
  )
  reference <- execution$governed_procedure_ref
  if (is.null(reference)) {
    return(NULL)
  }
  if (is.null(knowledge_view)) {
    tempest_governed_procedure_abort(
      "Governed execution requires its exact pinned {.arg knowledge_view}."
    )
  }
  tempest_governed_procedure_preflight(
    reference = reference,
    knowledge_view = knowledge_view,
    stage = execution$stage,
    program_artifact_id = execution$program_artifact_id,
    contract_version = execution$contract_version,
    evaluator_id = execution$evaluator_id,
    evaluator_version = execution$evaluator_version
  )
}

tempest_dsprrr_execution_governance_trace <- function(execution) {
  execution <- tempest_dsprrr_execution_require(
    execution,
    execution$stage %||% "unknown"
  )
  if (is.null(execution$governed_procedure_ref)) {
    return(NULL)
  }
  tempest_governed_procedure_trace_binding(
    execution$governed_procedure_ref
  )
}

tempest_dsprrr_execution_metadata_validate <- function(execution, metadata) {
  if (!is.list(metadata)) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution did not return structured verification metadata."
    )
  }
  expected_program_artifact_id <- execution$program_artifact_id
  actual_program_artifact_id <- metadata$program_artifact_id %||% NULL
  if (!identical(actual_program_artifact_id, expected_program_artifact_id)) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution metadata does not match the bound program artifact."
    )
  }
  if (!identical(metadata$trace_context %||% list(), execution$trace_context)) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution metadata does not match the bound Tempest trace context."
    )
  }
  invisible(metadata)
}

tempest_dsprrr_structured_missing <- function(value) {
  is.atomic(value) && length(value) == 1L && is.na(value)
}

tempest_dsprrr_structured_output <- function(value) {
  if (is.factor(value)) {
    return(unname(as.character(value)))
  }
  if (is.data.frame(value)) {
    rows <- lapply(seq_len(nrow(value)), function(index) {
      row <- lapply(value, function(column) {
        cell <- if (is.list(column)) column[[index]] else column[index]
        tempest_dsprrr_structured_output(cell)
      })
      row[!vapply(row, tempest_dsprrr_structured_missing, logical(1))]
    })
    return(unname(rows))
  }
  if (is.list(value) && is.null(attr(value, "class", exact = TRUE))) {
    return(lapply(value, tempest_dsprrr_structured_output))
  }
  value
}

#' @keywords internal
tempest_run_dsprrr_module_structured <- function(module, chat, inputs, step) {
  execution <- tempest_dsprrr_execution_verify(module, step)
  program <- execution$program
  trace_context <- execution$trace_context

  result <- do.call(
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
  )
  if (!inherits(result, "dsprrr_result")) {
    tempest_ecosystem_contract_abort(
      "dsprrr execution did not return structured verification metadata."
    )
  }
  tempest_dsprrr_execution_metadata_validate(execution, result$metadata)
  result$output <- tempest_dsprrr_structured_output(result$output)
  result
}

tempest_run_dsprrr_module_async <- function(module, chat, inputs, step) {
  execution <- tempest_dsprrr_execution_verify(module, step)
  request <- do.call(
    tempest_dsprrr_run_async,
    c(
      list(module = execution$program),
      inputs,
      list(
        .llm = chat,
        .trace_context = execution$trace_context
      )
    )
  )
  metadata <- attr(request, "dsprrr_trace_context", exact = TRUE)
  tempest_dsprrr_execution_metadata_validate(execution, metadata)
  normalized <- promises::then(
    request,
    tempest_dsprrr_structured_output
  )
  attr(normalized, "dsprrr_trace_context") <- metadata
  normalized
}

tempest_dsprrr_contract_condition <- function(condition) {
  classes <- class(condition)
  inherits(condition, "tempest_ecosystem_contract_error") ||
    inherits(condition, "tempest_program_set_error") ||
    inherits(condition, "dsprrr_trace_context_error") ||
    inherits(condition, "dsprrr_trace_contract_error") ||
    inherits(condition, "dsprrr_program_trace_contract_error") ||
    any(grepl("^dsprrr_(artifact_|program_artifact_)", classes))
}

tempest_rethrow_dsprrr_contract <- function(condition) {
  if (tempest_dsprrr_contract_condition(condition)) {
    stop(condition)
  }
  invisible(condition)
}

#' Create dsprrr modules for structured steps
#'
#' Creates dsprrr modules for STORM structured extraction/generation steps.
#'
#' @param config A `TempestConfig` object.
#' @return A named list containing every fixed-stage dsprrr module.
#' @keywords internal
tempest_make_dsprrr_modules <- function(config) {
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
      )
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
      )
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
      )
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
          "Every quote must be a verbatim contiguous substring of captured source context, including exact capitalization and Markdown.",
          "Never quote answer-only prose; omit the quote when no captured source excerpt contains it.",
          "Never add formatting or ellipses to a quote; omit it when no exact quote is available.",
          "Include support_score in [0,1] when source support is clear; omit it when unscored.",
          "Do not infer or invent facts.",
          sep = "\n"
        )
      )
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
      )
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
      )
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
          "Do not include an introduction, summary, overview, conclusion, executive summary, or at-a-glance section; Tempest generates the decision lead separately.",
          "The outline will later be refined using verified facts.",
          sep = "\n"
        )
      )
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
          "Exclude introduction, summary, overview, conclusion, executive summary, and at-a-glance sections; Tempest generates the decision lead separately.",
          "Remove requested angles that lack verified facts; a one-section outline is valid and preferred when evidence is sparse.",
          "Ensure sections are supportable by cited facts.",
          sep = "\n"
        )
      )
    ),
    section_writing = dsprrr::module(
      dsprrr::signature(
        inputs = list(
          dsprrr::input("section_title", "string"),
          dsprrr::input("section_summary", "string"),
          dsprrr::input("subsections", "string"),
          dsprrr::input("facts", "string")
        ),
        output_type = tempest_type_briefing_items(),
        instructions = paste(
          "Prepare one concise, decision-useful section as typed briefing items.",
          "Use only the supplied verified facts and their exact claim_ids.",
          "Each fact ends with its status against accepted knowledge: new, or already accepted.",
          "Return up to three observations by copying the exact text of facts whose status is new; return no observations when no fact is new.",
          "Add up to two assessment prompts using only: Assess the decision implications of: CLAIM_TEXT.",
          "Add up to two review_action prompts using only: Review before deciding: CLAIM_TEXT.",
          "For multiple bound claims, sort by claim_id and join their exact claim text with ' | ' in either prompt.",
          "Add at most one no_change item by copying the exact text of one fact whose status is already accepted; never use a new fact for no_change.",
          "When no fact is new, return exactly one no_change item so the section still reports its verified finding.",
          "Omit no_change when no fact is already accepted; missing or unresolved evidence is not a no-change signal.",
          "Every non-observation item must bind exactly the claim_ids copied into its text.",
          "Assessments and no_change items require calibrated confidence; observations and review actions omit it.",
          "Do not put source citations, Markdown, or provenance prose in item text.",
          sep = "\n"
        )
      )
    ),
    lead_section = dsprrr::module(
      dsprrr::signature(
        inputs = list(
          dsprrr::input("topic", "string"),
          dsprrr::input("title", "string"),
          dsprrr::input("article_body", "string"),
          dsprrr::input("facts", "string")
        ),
        output_type = tempest_type_briefing_items(),
        instructions = paste(
          "Prepare a compact at-a-glance decision brief as typed items.",
          "Use only the supplied verified facts and their exact claim_ids.",
          "Each fact ends with its status against accepted knowledge: new, or already accepted.",
          "Select up to three observations by copying the exact text of facts whose status is new; select none when no fact is new.",
          "Add no more than one assessment prompt using only: Assess the decision implications of: CLAIM_TEXT.",
          "Add no more than one review_action prompt using only: Review before deciding: CLAIM_TEXT.",
          "For multiple bound claims, sort by claim_id and join their exact claim text with ' | ' in either prompt.",
          "A no_change item must copy the exact text of one fact whose status is already accepted; omit it when no fact is already accepted.",
          "When no fact is new, return exactly one no_change item so the brief still reports its verified finding.",
          "Every non-observation item must bind exactly the verified claim_ids copied into its text.",
          "Assessments and no_change items require calibrated confidence; observations and review actions omit it.",
          "Do not put source citations, Markdown, or provenance prose in item text.",
          sep = "\n"
        )
      )
    )
  )
  modules
}
