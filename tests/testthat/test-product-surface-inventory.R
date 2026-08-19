test_that("T9 product entry points have exact source owners", {
  r_dir <- testthat::test_path("..", "..", "R")
  r_files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  definitions <- do.call(
    rbind,
    lapply(r_files, function(path) {
      expressions <- parse(path)
      names <- vapply(
        expressions,
        function(expression) {
          if (
            is.call(expression) &&
              identical(expression[[1L]], as.name("<-")) &&
              is.call(expression[[3L]]) &&
              identical(expression[[3L]][[1L]], as.name("function"))
          ) {
            return(as.character(expression[[2L]]))
          }
          NA_character_
        },
        character(1)
      )
      data.frame(
        name = names[!is.na(names)],
        owner = rep(basename(path), sum(!is.na(names))),
        stringsAsFactors = FALSE
      )
    })
  )
  entry_points <- c(
    run_app = "app.R",
    tempest_run = "storm.R",
    tempest_session = "costorm.R",
    tempest_report_md = "product-report.R",
    tempest_session_report_md = "costorm-report.R",
    tempest_product_report_reference = "product-report.R",
    tempest_product_report_reference_validate = "product-report.R",
    tempest_task = "evals.R",
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
  counts <- vapply(
    names(entry_points),
    function(name) sum(definitions$name == name),
    integer(1)
  )

  expect_identical(unname(counts), rep(1L, length(entry_points)))
  rows <- match(names(entry_points), definitions$name)
  expect_identical(definitions$owner[rows], unname(entry_points))
})

test_that("T9 retired source seams remain absent", {
  root <- testthat::test_path("..", "..")
  r_dir <- file.path(root, "R")
  r_files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  definitions <- do.call(
    rbind,
    lapply(r_files, function(path) {
      expressions <- parse(path)
      names <- vapply(
        expressions,
        function(expression) {
          if (
            is.call(expression) &&
              identical(expression[[1L]], as.name("<-")) &&
              is.call(expression[[3L]]) &&
              identical(expression[[3L]][[1L]], as.name("function"))
          ) {
            return(as.character(expression[[2L]]))
          }
          NA_character_
        },
        character(1)
      )
      data.frame(
        name = names[!is.na(names)],
        owner = rep(basename(path), sum(!is.na(names))),
        stringsAsFactors = FALSE
      )
    })
  )
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
  invalid_persistence_names <- unlist(
    lapply(
      names(persistence_owners),
      function(owner) {
        names <- definitions$name[definitions$owner == owner]
        allowed <- Reduce(
          `|`,
          lapply(persistence_owners[[owner]], grepl, x = names)
        )
        names[!allowed]
      }
    ),
    use.names = FALSE
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
    "tempest_semantic_filter_facts"
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
  run_definitions <- sort(
    grep("^tempest_run_", definitions$name, value = TRUE),
    method = "radix"
  )

  expect_identical(invalid_persistence_names, character())
  expect_disjoint(definitions$name, retired_definitions)
  expect_identical(run_definitions, allowed_run_definitions)

  persistence_files <- file.path(r_dir, names(persistence_owners))
  expect_identical(file.exists(persistence_files), rep(TRUE, 4L))
  expect_identical(file.exists(file.path(r_dir, "run-persistence.R")), FALSE)
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

  retired_prompts <- file.path(
    root,
    "inst",
    "prompts",
    c(
      "polisher_system.md",
      "reporter_system.md",
      "qa_solver_system.md"
    )
  )
  expect_identical(file.exists(retired_prompts), rep(FALSE, 3L))

  ui_files <- c(
    file.path(r_dir, c("app.R", "shiny-adapter.R")),
    list.files(
      file.path(root, "inst", "shiny", "R"),
      pattern = "[.]R$",
      full.names = TRUE
    ),
    file.path(root, "inst", "examples", "shiny-host", "app.R")
  )
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
