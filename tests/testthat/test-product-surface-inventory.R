test_that("product entry points have exact source owners", {
  context <- test_source_inventory_context()
  entry_points <- c(
    run_app = "app.R",
    tempest_run = "storm.R",
    tempest_session = "costorm.R",
    tempest_report_md = "product-report.R",
    tempest_session_report_md = "costorm-report.R",
    tempest_product_report_reference = "product-report.R",
    tempest_product_report_reference_validate = "product-report.R",
    tempest_task = "evals.R",
    tempest_trajectory_review = "trajectory-review.R",
    tempest_costorm_task = "evals.R",
    tempest_promotion_bundle = "promotion-types.R",
    tempest_research_workspace_snapshot = "research-workspace-persistence.R",
    tempest_research_workspace_restore = "research-workspace-persistence.R",
    tempest_session_snapshot = "costorm-persistence.R",
    tempest_session_restore = "costorm-persistence.R",
    tempest_session_save = "costorm-persistence.R",
    tempest_session_resume = "costorm-persistence.R",
    tempest_costorm_archive_read = "costorm-persistence.R",
    tempest_storm_load_artifacts = "storm-persistence.R",
    tempest_storm_save_artifacts = "storm-persistence.R",
    tempest_shiny_store = "shiny-adapter.R",
    tempest_shiny_ui = "shiny-adapter.R",
    tempest_shiny_server = "shiny-adapter.R"
  )
  if (identical(context$mode, "source")) {
    definitions <- test_source_inventory_definitions(context)
    counts <- vapply(
      names(entry_points),
      function(name) sum(definitions$name == name),
      integer(1)
    )

    expect_identical(unname(counts), rep(1L, length(entry_points)))
    rows <- match(names(entry_points), definitions$name)
    expect_identical(definitions$owner[rows], unname(entry_points))
  } else {
    namespace <- asNamespace("tempest")
    bindings <- vapply(
      names(entry_points),
      function(name) {
        exists(name, envir = namespace, inherits = FALSE) &&
          is.function(get(name, envir = namespace, inherits = FALSE))
      },
      logical(1)
    )
    expected_formals <- list(
      run_app = "...",
      tempest_run = c(
        "topic",
        "config",
        "retriever",
        "knowledge_view",
        "n_experts",
        "experts",
        "research_strategy",
        "max_rounds",
        "max_questions_per_perspective",
        "parallel_writing",
        "program_set",
        "steps",
        "output_dir",
        "resume",
        "run_id",
        "progress",
        "verbose"
      ),
      tempest_session = c(
        "topic",
        "config",
        "n_experts",
        "experts",
        "retriever",
        "progress",
        "session_id",
        "program_set",
        "knowledge_view"
      ),
      tempest_report_md = c(
        "title",
        "body",
        "workspace",
        "citation_policy",
        "on_unsupported_claim",
        "min_support_score"
      ),
      tempest_session_report_md = "session",
      tempest_product_report_reference = "value",
      tempest_product_report_reference_validate = c("reference", "value"),
      tempest_task = c(
        "dataset",
        "solver",
        "scorer",
        "scorer_chat",
        "config",
        "program_set",
        "knowledge_view",
        "..."
      ),
      tempest_trajectory_review = c(
        "research",
        "promotion_bundle",
        "promotion_receipt"
      ),
      tempest_costorm_task = c(
        "dataset",
        "config",
        "max_turns",
        "solver",
        "scorer",
        "scorer_chat",
        "program_set",
        "knowledge_view",
        "..."
      ),
      tempest_promotion_bundle = c("research", "claim_ids"),
      tempest_research_workspace_snapshot = "workspace",
      tempest_research_workspace_restore = c(
        "snapshot",
        "workspace",
        "graft_snapshot"
      ),
      tempest_session_snapshot = "session",
      tempest_session_restore = c(
        "snapshot",
        "config",
        "progress",
        "program_set",
        "knowledge_view"
      ),
      tempest_session_save = c("session", "path", "overwrite"),
      tempest_session_resume = c(
        "path",
        "config",
        "progress",
        "program_set",
        "knowledge_view"
      ),
      tempest_costorm_archive_read = "path",
      tempest_storm_load_artifacts = c(
        "run_dir",
        "workspace",
        "config",
        "program_set",
        "run_id"
      ),
      tempest_storm_save_artifacts = c(
        "run_dir",
        "workspace",
        "state",
        "research_manifest",
        "program_set",
        "config",
        "steps"
      ),
      tempest_shiny_store = character(),
      tempest_shiny_ui = c("id", "panels", "show_config"),
      tempest_shiny_server = c(
        "id",
        "config",
        "store",
        "panels",
        "experts",
        "session_id",
        "program_set",
        "knowledge_view"
      )
    )
    actual_formals <- lapply(
      names(expected_formals),
      function(name) {
        formal_names <- names(formals(get(
          name,
          envir = namespace,
          inherits = FALSE
        )))
        if (is.null(formal_names)) character() else formal_names
      }
    )
    names(actual_formals) <- names(expected_formals)

    expect_identical(unname(bindings), rep(TRUE, length(entry_points)))
    expect_identical(names(expected_formals), names(entry_points))
    expect_identical(actual_formals, expected_formals)
  }
})

test_that("T9 retired source seams remain absent", {
  context <- test_source_inventory_context()
  persistence_owners <- list(
    "product-persistence.R" = c(
      "^tempest_product_",
      "^tempest_persistence_",
      "^tempest_session_persistence_error_class$"
    ),
    "research-workspace-persistence.R" = "^tempest_research_workspace_",
    "costorm-persistence.R" = c(
      "^tempest_costorm_",
      "^tempest_session_",
      "^tempest_expert_session_records_from_json$"
    ),
    "storm-persistence.R" = "^tempest_storm_"
  )
  retired_definitions <- c(
    "tempest_storm_report_prompt",
    "tempest_costorm_report_prompt",
    "tempest_persistence_report_reference",
    "tempest_persistence_validate_report_reference",
    "tempest_persistence_execution_review_candidates",
    "tempest_persistence_report_without_execution_review",
    "tempest_persistence_report_for_records",
    "tempest_persistence_stage_manifest_traces",
    "tempest_persistence_deputy_manifest_traces",
    "tempest_persistence_expert_session_trace_bindings",
    "tempest_persistence_manifest_runtime_from_traces",
    "tempest_persistence_authoritative_extraction_attempt_ids",
    "tempest_persistence_manifest_validate_trace_ids",
    "tempest_persistence_manifest_existing_traces",
    "tempest_persistence_manifest_bind_stage_records",
    "tempest_persistence_manifest_validate_stage_records",
    "tempest_persistence_manifest_bind_report",
    "tempest_persistence_manifest_validate_report",
    "tempest_persistence_report_inline_citations",
    "tempest_persistence_validate_report_policy",
    "tempest_run_section_job",
    "tempest_parallel_workers",
    "tempest_setup_daemons",
    "tempest_collect_parallel",
    "tempest_parallel_records_import",
    "tempest_semantic_filter_facts",
    "tempest_shinychat_input_part_text",
    "tempest_shinychat_input_text",
    "tempest_shinychat_turn_text"
  )
  allowed_run_definitions <- sort(
    c(
      "tempest_run_async",
      "tempest_run_cancel",
      "tempest_run_dsprrr_module",
      "tempest_run_dsprrr_module_async",
      "tempest_run_dsprrr_module_structured",
      "tempest_run_internal",
      "tempest_run_verification"
    ),
    method = "radix"
  )
  if (identical(context$mode, "source")) {
    definitions <- test_source_inventory_definitions(context)
    invalid_persistence_names <- unlist(
      lapply(
        names(persistence_owners),
        function(owner) {
          definition_names <- definitions$name[definitions$owner == owner]
          allowed <- Reduce(
            `|`,
            lapply(
              persistence_owners[[owner]],
              grepl,
              x = definition_names
            )
          )
          definition_names[!allowed]
        }
      ),
      use.names = FALSE
    )
    run_definitions <- sort(
      grep("^tempest_run_", definitions$name, value = TRUE),
      method = "radix"
    )

    expect_identical(invalid_persistence_names, character())
    expect_disjoint(definitions$name, retired_definitions)
    expect_identical(run_definitions, allowed_run_definitions)

    r_dir <- file.path(context$root, "R")
    persistence_files <- file.path(r_dir, names(persistence_owners))
    expect_identical(file.exists(persistence_files), rep(TRUE, 4L))
    expect_identical(
      file.exists(file.path(r_dir, "run-persistence.R")),
      FALSE
    )
    persistence_source <- paste(
      unlist(lapply(persistence_files, readLines, warn = FALSE)),
      collapse = "\n"
    )
    forbidden_persistence_tokens <- c(
      "tempest.session_write_hook",
      "tempest.run_write_hook",
      "tempest.session_partial_recovery",
      "getOption(",
      "options("
    )
    present_persistence_tokens <- forbidden_persistence_tokens[vapply(
      forbidden_persistence_tokens,
      function(token) grepl(token, persistence_source, fixed = TRUE),
      logical(1)
    )]
    expect_identical(present_persistence_tokens, character())
  } else {
    namespace <- asNamespace("tempest")
    retired_present <- vapply(
      retired_definitions,
      exists,
      logical(1),
      envir = namespace,
      inherits = FALSE
    )
    run_definitions <- sort(
      test_source_inventory_namespace_functions("^tempest_run_"),
      method = "radix"
    )
    store <- tempest_shiny_store()
    store_members <- c(
      "peek_costorm_session",
      "costorm_session",
      "costorm_workspace",
      "set_costorm_session",
      "touch_costorm_session",
      "save_costorm_session",
      "resume_costorm_session",
      "costorm_persistence_status",
      "report_md",
      "report_workspace",
      "report_topic",
      "publish_costorm_report",
      "publish_storm_report"
    )
    adapter <- get(
      "TempestShinyChatAdapter",
      envir = namespace,
      inherits = FALSE
    )
    initialize_formals <- names(formals(adapter$public_methods$initialize))

    expect_identical(
      unname(retired_present),
      rep(FALSE, length(retired_definitions))
    )
    expect_identical(run_definitions, allowed_run_definitions)
    expect_named(store, store_members)
    expect_identical(
      initialize_formals,
      c(
        "id",
        "initial_client",
        "session",
        "on_turn",
        "workspace",
        "render_message",
        "on_dispose",
        "backend"
      )
    )
  }

  retired_prompt_names <- c(
    "polisher_system.md",
    "reporter_system.md",
    "qa_solver_system.md"
  )
  if (identical(context$mode, "source")) {
    retired_prompts <- file.path(
      context$root,
      "inst",
      "prompts",
      retired_prompt_names
    )
    ui_files <- c(
      file.path(
        context$root,
        "R",
        c("app.R", "shiny-adapter.R", "shinychat-adapter.R")
      ),
      file.path(context$root, "inst", "shiny", "app.R"),
      list.files(
        file.path(context$root, "inst", "shiny", "R"),
        pattern = "[.]R$",
        full.names = TRUE
      ),
      file.path(context$root, "inst", "examples", "shiny-host", "app.R")
    )
  } else {
    retired_prompts <- vapply(
      retired_prompt_names,
      function(name) system.file("prompts", name, package = "tempest"),
      character(1)
    )
    shiny_root <- system.file("shiny", package = "tempest")
    ui_files <- c(
      file.path(shiny_root, "app.R"),
      list.files(
        file.path(shiny_root, "R"),
        pattern = "[.]R$",
        full.names = TRUE
      ),
      system.file("examples", "shiny-host", "app.R", package = "tempest")
    )
  }
  expect_identical(unname(file.exists(retired_prompts)), rep(FALSE, 3L))
  expect_identical(file.exists(ui_files), rep(TRUE, length(ui_files)))
  ui_source <- paste(
    unlist(lapply(ui_files, readLines, warn = FALSE)),
    collapse = "\n"
  )
  retired_ui_tokens <- c(
    "source_store",
    "report_store",
    "autosave_trigger",
    "autosave_session",
    "report_ready",
    "set_persistence",
    "set_session_report",
    "set_storm_result"
  )
  present_ui_tokens <- retired_ui_tokens[vapply(
    retired_ui_tokens,
    function(token) grepl(token, ui_source, fixed = TRUE),
    logical(1)
  )]
  old_store_member <- grepl(
    paste0(
      "\\b(shared_store|store)\\$",
      "(peek|get|set|touch|save|restore|evidence_store|report_store|",
      "persistence|set_persistence|report|set_session_report|",
      "set_storm_result)\\b"
    ),
    ui_source,
    perl = TRUE
  )

  expect_identical(present_ui_tokens, character())
  expect_identical(old_store_member, FALSE)
  expect_no_match(ui_source, "tempest_run\\s*=\\s*NULL")
  expect_no_match(ui_source, "parallel_research\\s*=\\s*TRUE")
})
