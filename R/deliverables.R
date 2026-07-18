# Shared deliverable lifecycle

tempest_deliverable_abort <- function(
  message,
  ...,
  operation_id = NULL,
  phase = NULL,
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_deliverable_execution_error", "tempest_error"),
    parent = parent,
    operation_id = operation_id,
    phase = phase,
    .envir = rlang::caller_env()
  )
}

tempest_call_operation <- function(operation, arguments) {
  if (is.primitive(operation)) {
    return(do.call(operation, arguments))
  }
  formal_names <- names(formals(operation))
  if ("..." %in% formal_names) {
    return(do.call(operation, arguments))
  }
  if (length(formal_names) == 0L) {
    return(do.call(operation, list()))
  }
  do.call(operation, arguments[intersect(names(arguments), formal_names)])
}

tempest_deliverable_operation_version <- function(deliverable, id) {
  versions <- deliverable@operation_versions
  if (
    length(versions) == 0L ||
      is.null(names(versions)) ||
      !id %in% names(versions)
  ) {
    return(NULL)
  }
  unname(versions[[id]])
}

tempest_deliverable_resolve <- function(
  registry,
  deliverable,
  id,
  kind
) {
  version <- tempest_deliverable_operation_version(deliverable, id)
  list(
    id = id,
    version = version,
    kind = kind,
    implementation = registry$resolve(
      id,
      version = version,
      kind = kind
    ),
    descriptor = registry$describe(
      id,
      version = version,
      kind = kind
    )
  )
}

tempest_deliverable_plan <- function(
  deliverable,
  context = list(),
  registry = NULL,
  catalog = NULL,
  runtime = list(),
  provenance = list()
) {
  if (!S7::S7_inherits(deliverable, TempestDeliverableSpec)) {
    tempest_deliverable_abort(
      "{.arg deliverable} must be created by {.fn tempest_deliverable_spec}."
    )
  }
  context <- tempest_workflow_list(context, "context")
  runtime <- tempest_workflow_list(runtime, "runtime")
  provenance <- tempest_workflow_list(provenance, "provenance")
  registry <- registry %||% tempest_builtin_operation_registry()
  if (!inherits(registry, "TempestOperationRegistry")) {
    tempest_deliverable_abort(
      "{.arg registry} must be created by {.fn tempest_operation_registry}."
    )
  }
  catalog <- catalog %||% tempest_artifact_catalog()
  if (!inherits(catalog, "TempestArtifactCatalog")) {
    tempest_deliverable_abort(
      "{.arg catalog} must be created by {.fn tempest_artifact_catalog}."
    )
  }

  resolve_many <- function(ids, kind) {
    stats::setNames(
      lapply(
        ids,
        function(id) {
          tempest_deliverable_resolve(
            registry,
            deliverable,
            id,
            kind
          )
        }
      ),
      ids
    )
  }

  structure(
    list(
      deliverable = deliverable,
      context = context,
      registry = registry,
      catalog = catalog,
      runtime = runtime,
      provenance = provenance,
      generator = tempest_deliverable_resolve(
        registry,
        deliverable,
        deliverable@generator_id,
        "generator"
      ),
      validators = resolve_many(deliverable@validator_ids, "validator"),
      renderers = resolve_many(deliverable@renderer_ids, "renderer"),
      exporters = resolve_many(deliverable@exporter_ids, "exporter")
    ),
    class = "tempest_deliverable_plan"
  )
}

tempest_deliverable_run_operation <- function(
  operation,
  phase,
  arguments
) {
  tryCatch(
    tempest_call_operation(operation$implementation, arguments),
    error = function(error) {
      if (inherits(error, "tempest_deliverable_execution_error")) {
        stop(error)
      }
      tempest_deliverable_abort(
        "Deliverable operation {.val {operation$id}} failed during {.val {phase}}.",
        operation_id = operation$id,
        phase = phase,
        parent = error
      )
    }
  )
}

tempest_deliverable_generate <- function(plan) {
  stopifnot(inherits(plan, "tempest_deliverable_plan"))
  tempest_deliverable_run_operation(
    plan$generator,
    "generation",
    list(
      deliverable = plan$deliverable,
      context = plan$context,
      runtime = plan$runtime
    )
  )
}

tempest_deliverable_normalize_validation <- function(result, operation) {
  if (S7::S7_inherits(result, TempestValidationResult)) {
    result <- list(result)
  }
  if (
    !is.list(result) ||
      length(result) == 0L ||
      any(
        !vapply(
          result,
          function(value) {
            S7::S7_inherits(value, TempestValidationResult)
          },
          logical(1)
        )
      )
  ) {
    tempest_deliverable_abort(
      "Validator {.val {operation$id}} returned an invalid result.",
      operation_id = operation$id,
      phase = "validation"
    )
  }
  if (
    any(
      !vapply(
        result,
        function(value) identical(value@validator_id, operation$id),
        logical(1)
      )
    )
  ) {
    tempest_deliverable_abort(
      "Validator {.val {operation$id}} returned a result for another operation.",
      operation_id = operation$id,
      phase = "validation"
    )
  }
  result
}

#' Describe one renderer-produced artifact representation
#'
#' `r lifecycle::badge("experimental")`
#'
#' Custom renderer operations return this lightweight runtime value. The
#' deliverable lifecycle adds specification identity, validation results,
#' provenance, checksums, and lifecycle status when it creates the final typed
#' artifact.
#'
#' @param content Inline representation content.
#' @param storage_ref Optional external storage reference.
#' @param artifact_kind Role within the deliverable.
#' @param media_type Optional media type. Defaults to renderer metadata or the
#'   deliverable specification.
#' @param resource_ids,claim_ids,evidence_span_ids Evidence identifiers.
#' @param parent_artifact_ids Parent artifact identifiers.
#' @param metadata Serializable representation metadata.
#' @return A runtime `tempest_artifact_representation`.
#' @export
tempest_artifact_representation <- function(
  content = NULL,
  storage_ref = NA_character_,
  artifact_kind = "primary",
  media_type = NULL,
  resource_ids = character(),
  claim_ids = character(),
  evidence_span_ids = character(),
  parent_artifact_ids = character(),
  metadata = list()
) {
  storage_ref <- if (is.null(storage_ref)) NA_character_ else storage_ref
  if (
    !is.character(storage_ref) ||
      length(storage_ref) != 1L ||
      (!is.na(storage_ref) && !nzchar(tempest_trim(storage_ref)))
  ) {
    tempest_deliverable_abort(
      "{.arg storage_ref} must be a non-empty string or `NA`."
    )
  }
  if (is.null(content) && is.na(storage_ref)) {
    tempest_deliverable_abort(
      "A representation must contain {.arg content} or a {.arg storage_ref}."
    )
  }
  if (!is.null(media_type)) {
    media_type <- tempest_workflow_scalar(media_type, "media_type")
  }
  structure(
    list(
      content = content,
      storage_ref = storage_ref,
      artifact_kind = tempest_workflow_scalar(
        artifact_kind,
        "artifact_kind"
      ),
      media_type = media_type,
      resource_ids = tempest_workflow_character(
        resource_ids,
        "resource_ids"
      ),
      claim_ids = tempest_workflow_character(claim_ids, "claim_ids"),
      evidence_span_ids = tempest_workflow_character(
        evidence_span_ids,
        "evidence_span_ids"
      ),
      parent_artifact_ids = tempest_workflow_character(
        parent_artifact_ids,
        "parent_artifact_ids"
      ),
      metadata = tempest_workflow_list(metadata, "metadata")
    ),
    class = "tempest_artifact_representation"
  )
}

tempest_deliverable_representations <- function(
  value,
  operation,
  deliverable,
  renderer_index
) {
  if (inherits(value, "tempest_artifact_representation")) {
    return(list(value))
  }
  if (
    is.list(value) &&
      length(value) > 0L &&
      all(
        vapply(
          value,
          inherits,
          logical(1),
          what = "tempest_artifact_representation"
        )
      )
  ) {
    return(value)
  }
  media_type <- operation$descriptor$metadata$media_type %||%
    deliverable@media_types[[
      min(renderer_index, length(deliverable@media_types))
    ]]
  artifact_kind <- operation$descriptor$metadata$artifact_kind %||%
    if (renderer_index == 1L) "primary" else operation$id
  list(tempest_artifact_representation(
    content = value,
    artifact_kind = artifact_kind,
    media_type = media_type
  ))
}

tempest_deliverable_provenance_value <- function(
  provenance,
  name,
  default
) {
  provenance[[name]] %||% default
}

tempest_deliverable_artifact_id <- function(
  provenance,
  operation_id,
  artifact_index
) {
  artifact_ids <- provenance$artifact_ids %||% character()
  if (
    length(artifact_ids) > 0L &&
      !is.null(names(artifact_ids)) &&
      operation_id %in% names(artifact_ids)
  ) {
    return(unname(artifact_ids[[operation_id]]))
  }
  if (artifact_index == 1L && !is.null(provenance$artifact_id)) {
    return(provenance$artifact_id)
  }
  NULL
}

tempest_deliverable_finalize <- function(plan, canonical_content) {
  stopifnot(inherits(plan, "tempest_deliverable_plan"))
  deliverable <- plan$deliverable
  validation_results <- unlist(
    lapply(plan$validators, function(operation) {
      value <- tempest_deliverable_run_operation(
        operation,
        "validation",
        list(
          content = canonical_content,
          deliverable = deliverable,
          context = plan$context,
          runtime = plan$runtime
        )
      )
      tempest_deliverable_normalize_validation(value, operation)
    }),
    recursive = FALSE
  )
  validation_results <- validation_results %||% list()
  has_failed_validation <- any(
    vapply(
      validation_results,
      function(result) identical(result@status, "failed"),
      logical(1)
    )
  )
  artifact_status <- if (has_failed_validation) {
    "invalid"
  } else if (deliverable@requires_approval) {
    "awaiting_approval"
  } else {
    "valid"
  }

  artifact_index <- 0L
  artifacts <- unlist(
    lapply(seq_along(plan$renderers), function(renderer_index) {
      operation <- plan$renderers[[renderer_index]]
      rendered <- tempest_deliverable_run_operation(
        operation,
        "rendering",
        list(
          content = canonical_content,
          deliverable = deliverable,
          context = plan$context,
          runtime = plan$runtime,
          validation_results = validation_results
        )
      )
      representations <- tempest_deliverable_representations(
        rendered,
        operation,
        deliverable,
        renderer_index
      )
      lapply(representations, function(representation) {
        artifact_index <<- artifact_index + 1L
        tempest_artifact(
          deliverable,
          content = representation$content,
          storage_ref = representation$storage_ref,
          artifact_id = tempest_deliverable_artifact_id(
            plan$provenance,
            operation$id,
            artifact_index
          ),
          artifact_kind = representation$artifact_kind,
          media_type = representation$media_type,
          producer_operation_id = operation$id,
          run_id = tempest_deliverable_provenance_value(
            plan$provenance,
            "run_id",
            NA_character_
          ),
          step_id = tempest_deliverable_provenance_value(
            plan$provenance,
            "step_id",
            NA_character_
          ),
          expert_id = tempest_deliverable_provenance_value(
            plan$provenance,
            "expert_id",
            NA_character_
          ),
          resource_ids = unique(c(
            tempest_deliverable_provenance_value(
              plan$provenance,
              "resource_ids",
              character()
            ),
            representation$resource_ids
          )),
          claim_ids = unique(c(
            tempest_deliverable_provenance_value(
              plan$provenance,
              "claim_ids",
              character()
            ),
            representation$claim_ids
          )),
          evidence_span_ids = unique(c(
            tempest_deliverable_provenance_value(
              plan$provenance,
              "evidence_span_ids",
              character()
            ),
            representation$evidence_span_ids
          )),
          parent_artifact_ids = unique(c(
            tempest_deliverable_provenance_value(
              plan$provenance,
              "parent_artifact_ids",
              character()
            ),
            representation$parent_artifact_ids
          )),
          validation_results = validation_results,
          status = artifact_status,
          metadata = utils::modifyList(
            tempest_deliverable_provenance_value(
              plan$provenance,
              "metadata",
              list()
            ),
            representation$metadata
          )
        )
      })
    }),
    recursive = FALSE
  )

  for (exporter in plan$exporters) {
    artifacts <- lapply(artifacts, function(artifact) {
      exported <- tempest_deliverable_run_operation(
        exporter,
        "export",
        list(
          artifact = artifact,
          deliverable = deliverable,
          context = plan$context,
          runtime = plan$runtime,
          validation_results = validation_results
        )
      )
      if (is.null(exported)) {
        return(artifact)
      }
      if (!S7::S7_inherits(exported, TempestArtifact)) {
        tempest_deliverable_abort(
          "Exporter {.val {exporter$id}} must return a typed artifact or `NULL`.",
          operation_id = exporter$id,
          phase = "export"
        )
      }
      if (
        !identical(exported@artifact_id, artifact@artifact_id) ||
          !identical(
            exported@spec_fingerprint,
            artifact@spec_fingerprint
          )
      ) {
        tempest_deliverable_abort(
          "Exporter {.val {exporter$id}} changed artifact identity.",
          operation_id = exporter$id,
          phase = "export"
        )
      }
      exported
    })
  }
  plan$catalog$add_many(artifacts)

  resolved_operations <- c(
    list(plan$generator),
    plan$validators,
    plan$renderers,
    plan$exporters
  )
  result <- structure(
    list(
      deliverable = deliverable,
      canonical_content = canonical_content,
      validation_results = validation_results,
      artifacts = artifacts,
      catalog = plan$catalog,
      resolved_operations = lapply(
        resolved_operations,
        function(operation) operation$descriptor
      )
    ),
    class = "tempest_deliverable_result"
  )
  result
}

#' Generate and finalize a Tempest deliverable
#'
#' `r lifecycle::badge("experimental")`
#'
#' This is the application-neutral output lifecycle used by built-in and
#' host-defined workflows. It resolves all operations before generation, runs
#' validators, renders typed artifacts, invokes exporters, and publishes the
#' artifacts to a catalog. Failed validation produces inspectable invalid
#' artifacts rather than dropping output.
#'
#' Generator, validator, renderer, and exporter operations receive named
#' arguments and may declare only those they use. See
#' [tempest_artifact_representation()] for the renderer return contract.
#'
#' @param deliverable A `tempest_deliverable_spec`.
#' @param context Serializable generation and rendering context.
#' @param registry Runtime operation registry. Defaults to the built-in
#'   registry.
#' @param catalog Typed artifact catalog. A new in-memory catalog is created by
#'   default.
#' @param runtime Runtime-only clients and callbacks.
#' @param provenance Run, step, expert, evidence, and artifact identifiers.
#' @return A `tempest_deliverable_result` containing canonical content,
#'   validation results, typed artifacts, the catalog, and resolved operation
#'   metadata.
#' @examples
#' registry <- tempest_operation_registry(list(
#'   generate = list(
#'     kind = "generator",
#'     implementation = function(context) context$text
#'   ),
#'   render = list(
#'     kind = "renderer",
#'     implementation = function(content) content
#'   )
#' ))
#' spec <- tempest_deliverable_spec(
#'   "answer",
#'   title = "Answer",
#'   purpose = "Answer the request",
#'   instructions = "Be concise.",
#'   generator_id = "generate",
#'   renderer_ids = "render"
#' )
#' result <- tempest_generate_deliverable(
#'   spec,
#'   context = list(text = "Done"),
#'   registry = registry
#' )
#' result$artifacts[[1]]@content
#' @export
tempest_generate_deliverable <- function(
  deliverable,
  context = list(),
  registry = NULL,
  catalog = NULL,
  runtime = list(),
  provenance = list()
) {
  plan <- tempest_deliverable_plan(
    deliverable = deliverable,
    context = context,
    registry = registry,
    catalog = catalog,
    runtime = runtime,
    provenance = provenance
  )
  canonical_content <- tempest_deliverable_generate(plan)
  tempest_deliverable_finalize(plan, canonical_content)
}

tempest_markdown_report_prompt <- function(deliverable, context) {
  if (!is.null(context$prompt)) {
    return(tempest_workflow_scalar(context$prompt, "context$prompt"))
  }
  objective <- context$objective %||% NULL
  objective_text <- if (S7::S7_inherits(objective, TempestObjective)) {
    paste0(
      "\n\nObjective:\n",
      objective@description,
      if (length(objective@constraints) > 0L) {
        paste0(
          "\n\nConstraints:\n- ",
          paste(objective@constraints, collapse = "\n- ")
        )
      } else {
        ""
      },
      if (length(objective@acceptance_criteria) > 0L) {
        paste0(
          "\n\nAcceptance criteria:\n- ",
          paste(objective@acceptance_criteria, collapse = "\n- ")
        )
      } else {
        ""
      }
    )
  } else {
    ""
  }
  source_material <- context$source_material %||% ""
  source_text <- if (
    is.character(source_material) &&
      length(source_material) == 1L &&
      nzchar(tempest_trim(source_material))
  ) {
    paste0("\n\nSource material:\n", source_material)
  } else {
    ""
  }
  required_text <- if (length(deliverable@required_fields) > 0L) {
    paste0(
      "\n\nRequired sections or fields:\n- ",
      paste(deliverable@required_fields, collapse = "\n- ")
    )
  } else {
    ""
  }
  paste0(
    "Create the requested deliverable in Markdown.\n\n",
    "Purpose: ",
    deliverable@purpose,
    "\n\nInstructions:\n",
    deliverable@instructions,
    objective_text,
    source_text,
    required_text
  )
}

tempest_storm_report_spec <- function(
  title,
  config,
  remove_duplicate = FALSE
) {
  tempest_deliverable_spec(
    "storm-report",
    title = title,
    purpose = "Produce the final evidence-backed STORM report.",
    instructions = tempest_polish_rules(remove_duplicate = remove_duplicate),
    evidence_policy = config@citation_policy,
    generator_id = "tempest.generator.markdown_report",
    validator_ids = "tempest.validator.required_fields",
    renderer_ids = "tempest.renderer.markdown_report",
    operation_versions = c(
      "tempest.generator.markdown_report" = "1",
      "tempest.validator.required_fields" = "1",
      "tempest.renderer.markdown_report" = "1"
    ),
    filename_policy = list(filename = "report.md"),
    metadata = list(workflow = "storm")
  )
}

tempest_storm_report_prompt <- function(draft_md, remove_duplicate) {
  paste0(
    "Polish the following Markdown report.\n\n",
    "Rules:\n",
    tempest_polish_rules(remove_duplicate = remove_duplicate),
    "\n\n",
    "<draft>\n",
    draft_md,
    "\n</draft>\n"
  )
}

tempest_storm_report_plan <- function(
  title,
  draft_md,
  store,
  config,
  remove_duplicate,
  catalog,
  run_id,
  generate_text
) {
  tempest_deliverable_plan(
    deliverable = tempest_storm_report_spec(
      title,
      config,
      remove_duplicate
    ),
    context = list(
      prompt = tempest_storm_report_prompt(
        draft_md,
        remove_duplicate
      ),
      title = title,
      store = store,
      include_references = TRUE,
      citation_policy = config@citation_policy,
      on_unsupported_claim = config@on_unsupported_claim,
      min_support_score = config@min_support_score
    ),
    catalog = catalog,
    runtime = list(generate_text = generate_text),
    provenance = list(
      artifact_id = "report_md",
      run_id = run_id,
      step_id = "polish",
      metadata = list(topic = title)
    )
  )
}

tempest_storm_restore_report_artifact <- function(
  report_md,
  title,
  config,
  remove_duplicate,
  catalog,
  run_id
) {
  artifact <- tempest_artifact(
    tempest_storm_report_spec(title, config, remove_duplicate),
    content = report_md,
    artifact_id = "report_md",
    producer_operation_id = "tempest.renderer.markdown_report",
    run_id = run_id,
    step_id = "polish",
    status = "valid",
    metadata = list(topic = title, restored = TRUE)
  )
  catalog$add(artifact)
  artifact
}

tempest_costorm_report_spec <- function(session) {
  tempest_deliverable_spec(
    "costorm-report",
    title = session$title %||% session$topic %||% "Co-STORM Report",
    purpose = "Synthesize the Co-STORM session into an evidence-backed report.",
    instructions = paste(
      "Use only verified facts, preserve Tempest source citations,",
      "and do not invent facts."
    ),
    evidence_policy = session$config@citation_policy,
    generator_id = "tempest.generator.markdown_report",
    validator_ids = "tempest.validator.required_fields",
    renderer_ids = "tempest.renderer.markdown_report",
    operation_versions = c(
      "tempest.generator.markdown_report" = "1",
      "tempest.validator.required_fields" = "1",
      "tempest.renderer.markdown_report" = "1"
    ),
    filename_policy = list(filename = "report.md"),
    metadata = list(workflow = "costorm")
  )
}

tempest_costorm_report_prompt <- function(session, style) {
  paste0(
    "Write a comprehensive report based on the session.\n\n",
    "Topic: ",
    session$topic,
    "\n\n",
    "Mind map:\n",
    tempest_mindmap_to_markdown(session$mindmap),
    "\n\n",
    "Verified facts:\n",
    tempest_summarize_facts_for_prompt(session$store, max_items = 120),
    "\n\n",
    "Conversation (summary):\n",
    session$transcript_markdown(max_turns = 80),
    "\n\n",
    "Style: ",
    style,
    "\n\n",
    "Rules:\n",
    "- Use only verified facts (with citations).\n",
    "- Preserve citations like [Sxxxxxxxxxxxx].\n",
    "- Do not invent facts.\n\n",
    "Write the report body in Markdown (no title)."
  )
}

tempest_costorm_report_context <- function(
  session,
  style,
  include_references
) {
  list(
    prompt = tempest_costorm_report_prompt(session, style),
    title = session$title %||% session$topic,
    store = session$store,
    include_references = include_references,
    citation_policy = session$config@citation_policy,
    on_unsupported_claim = session$config@on_unsupported_claim,
    min_support_score = session$config@min_support_score,
    style = style
  )
}

tempest_costorm_artifact_catalog <- function(session) {
  catalog <- session$artifact_catalog %||% NULL
  if (!is.null(catalog)) {
    return(catalog)
  }
  tempest_artifact_catalog(store = session$config@artifact_store)
}

tempest_costorm_report_plan <- function(
  session,
  style,
  include_references,
  generate_text
) {
  tempest_deliverable_plan(
    deliverable = tempest_costorm_report_spec(session),
    context = tempest_costorm_report_context(
      session,
      style,
      include_references
    ),
    catalog = tempest_costorm_artifact_catalog(session),
    runtime = list(generate_text = generate_text),
    provenance = list(
      artifact_id = "report_md",
      run_id = session$session_id %||% NA_character_,
      step_id = "report",
      metadata = list(topic = session$topic, style = style)
    )
  )
}

tempest_deliverable_primary_artifact <- function(result) {
  primary <- Filter(
    function(artifact) identical(artifact@artifact_kind, "primary"),
    result$artifacts
  )
  if (length(primary) == 0L) {
    primary <- result$artifacts
  }
  if (length(primary) == 0L) {
    tempest_deliverable_abort(
      "The deliverable did not produce an artifact.",
      phase = "rendering"
    )
  }
  primary[[1]]
}

tempest_builtin_markdown_report_generator <- function(
  deliverable,
  context,
  runtime
) {
  generate_text <- runtime$generate_text %||% NULL
  if (
    is.null(generate_text) &&
      !is.null(runtime$chat) &&
      is.function(runtime$chat$chat)
  ) {
    generate_text <- function(prompt) {
      runtime$chat$chat(prompt, echo = "none")
    }
  }
  if (!is.function(generate_text)) {
    tempest_deliverable_abort(
      "The Markdown report generator requires {.field runtime$generate_text}.",
      operation_id = "tempest.generator.markdown_report",
      phase = "generation"
    )
  }
  tempest_call_operation(
    generate_text,
    list(
      prompt = tempest_markdown_report_prompt(deliverable, context),
      deliverable = deliverable,
      context = context,
      runtime = runtime
    )
  )
}

tempest_builtin_provided_content_generator <- function(context) {
  if (!"content" %in% names(context)) {
    tempest_deliverable_abort(
      "The provided-content generator requires {.field context$content}.",
      operation_id = "tempest.generator.provided_content",
      phase = "generation"
    )
  }
  context$content
}

tempest_builtin_required_fields_validator <- function(
  content,
  deliverable
) {
  required <- deliverable@required_fields
  if (length(required) == 0L) {
    return(tempest_validation_result(
      "tempest.validator.required_fields",
      message = "The specification has no required fields."
    ))
  }
  present <- if (
    is.list(content) &&
      !is.null(names(content))
  ) {
    vapply(
      required,
      function(field) {
        field %in% names(content) && !is.null(content[[field]])
      },
      logical(1)
    )
  } else if (is.character(content) && length(content) == 1L) {
    vapply(
      required,
      function(field) {
        pattern <- paste0(
          "(?im)^#{1,6}\\s+",
          "\\Q",
          field,
          "\\E",
          "\\s*$"
        )
        grepl(pattern, content, perl = TRUE)
      },
      logical(1)
    )
  } else {
    rep(FALSE, length(required))
  }
  missing <- required[!present]
  if (length(missing) > 0L) {
    return(tempest_validation_result(
      "tempest.validator.required_fields",
      status = "failed",
      message = paste0(
        "Missing required fields or sections: ",
        paste(missing, collapse = ", "),
        "."
      ),
      details = list(missing = missing)
    ))
  }
  tempest_validation_result(
    "tempest.validator.required_fields",
    message = "All required fields or sections are present."
  )
}

tempest_builtin_markdown_renderer <- function(content) {
  tempest_artifact_representation(
    content = content,
    media_type = "text/markdown"
  )
}

tempest_builtin_markdown_report_renderer <- function(
  content,
  deliverable,
  context
) {
  include_references <- context$include_references %||% TRUE
  include_references <- tempest_workflow_flag(
    include_references,
    "context$include_references"
  )
  if (!include_references) {
    return(tempest_artifact_representation(
      content = content,
      media_type = "text/markdown"
    ))
  }
  store <- context$store %||% NULL
  if (!inherits(store, "SourceStore") && !inherits(store, "TempestRetriever")) {
    tempest_deliverable_abort(
      "The Markdown report renderer requires a SourceStore when references are included.",
      operation_id = "tempest.renderer.markdown_report",
      phase = "rendering"
    )
  }
  rendered <- tempest_report_md(
    title = context$title %||% deliverable@title,
    body = content,
    store = store,
    citation_policy = context$citation_policy %||% deliverable@evidence_policy,
    on_unsupported_claim = context$on_unsupported_claim %||% "flag",
    min_support_score = context$min_support_score %||% 0.7
  )
  tempest_artifact_representation(
    content = rendered,
    media_type = "text/markdown"
  )
}

tempest_builtin_markdown_exporter <- function(
  artifact,
  deliverable,
  runtime
) {
  policy <- deliverable@filename_policy
  filename <- policy$filename %||% NULL
  if (is.null(filename)) {
    stem <- policy$stem %||%
      tempest_topic_slug(deliverable@deliverable_id)
    extension <- policy$extension %||% "md"
    filename <- paste0(stem, ".", sub("^\\.", "", extension))
  }
  filename <- basename(tempest_workflow_scalar(filename, "filename"))
  artifact@metadata <- utils::modifyList(
    artifact@metadata,
    list(filename = filename)
  )
  if (is.null(runtime$output_dir)) {
    return(artifact)
  }
  output_dir <- tempest_workflow_scalar(
    runtime$output_dir,
    "runtime$output_dir"
  )
  if (!is.character(artifact@content) || length(artifact@content) != 1L) {
    tempest_deliverable_abort(
      "The Markdown exporter requires a single text artifact.",
      operation_id = "tempest.exporter.markdown",
      phase = "export"
    )
  }
  path <- file.path(output_dir, filename)
  tempest_write_text(path, artifact@content)
  artifact@storage_ref <- fs::path_abs(path)
  artifact@updated_at <- tempest_now_utc()
  artifact
}

#' Create a registry containing Tempest's built-in deliverable operations
#'
#' `r lifecycle::badge("experimental")`
#'
#' Hosts can register additional operations or explicitly replace a built-in
#' operation on the returned registry.
#'
#' @return A `TempestOperationRegistry`.
#' @export
tempest_builtin_operation_registry <- function() {
  tempest_operation_registry(list(
    "tempest.generator.markdown_report" = list(
      version = "1",
      kind = "generator",
      implementation = tempest_builtin_markdown_report_generator
    ),
    "tempest.generator.provided_content" = list(
      version = "1",
      kind = "generator",
      implementation = tempest_builtin_provided_content_generator
    ),
    "tempest.validator.required_fields" = list(
      version = "1",
      kind = "validator",
      implementation = tempest_builtin_required_fields_validator
    ),
    "tempest.renderer.markdown" = list(
      version = "1",
      kind = "renderer",
      implementation = tempest_builtin_markdown_renderer,
      metadata = list(
        media_type = "text/markdown",
        artifact_kind = "primary"
      )
    ),
    "tempest.renderer.markdown_report" = list(
      version = "1",
      kind = "renderer",
      implementation = tempest_builtin_markdown_report_renderer,
      metadata = list(
        media_type = "text/markdown",
        artifact_kind = "primary"
      )
    ),
    "tempest.exporter.markdown" = list(
      version = "1",
      kind = "exporter",
      implementation = tempest_builtin_markdown_exporter,
      metadata = list(media_type = "text/markdown")
    )
  ))
}
