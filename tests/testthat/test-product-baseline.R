test_that("default dsprrr STORM semantic outcomes are frozen", {
  baseline_local_ids()
  fixture <- storm_product_baseline_fixture()
  semantics <- baseline_storm_semantics(fixture)
  definition_ids <- vapply(
    semantics$citations$definitions,
    `[[`,
    character(1),
    "citation_id"
  )

  expect_identical(semantics$citations$uses, semantics$source_ids)
  expect_identical(definition_ids, semantics$source_ids)
  expect_equal(
    intersect(
      names(fixture$result),
      c("store", "artifact_catalog", "workflow_run")
    ),
    character()
  )
  expect_identical(fixture$result$workspace, fixture$store)
  expect_identical(
    fixture$result$manifest@research_run_id,
    "storm-product-baseline"
  )
  expect_identical(fixture$result$manifest@status, "succeeded")
  expect_snapshot(baseline_snapshot_json(semantics))
})

test_that("scripted STORM resumes its immutable full-run request", {
  baseline_local_ids()
  uninterrupted <- baseline_storm_semantics(
    storm_product_baseline_fixture()
  )
  baseline_local_ids()
  fixture <- storm_resume_baseline_fixture()
  resumed <- baseline_storm_semantics(list(
    result = fixture$restored,
    store = fixture$restored_store,
    events = fixture$restored_events,
    program_stages = fixture$program_stages
  ))
  outcome_fields <- c(
    "program_stages",
    "source_ids",
    "claims",
    "citations",
    "outline_sections",
    "outline_subsections",
    "report_sections",
    "terminal_status"
  )

  expect_equal(resumed[outcome_fields], uninterrupted[outcome_fields])
  expect_identical(fixture$restored$workspace, fixture$restored_store)
  expect_equal(
    intersect(
      names(fixture$restored),
      c("store", "artifact_catalog", "workflow_run")
    ),
    character()
  )
  expect_identical(
    fixture$restored$retriever$workspace,
    fixture$restored$workspace
  )
  resumed_source <- fake_source(
    url = "https://example.org/resumed-workspace-alias",
    title = "Resumed workspace alias"
  )
  expect_error(
    fixture$restored$retriever$workspace$upsert_retrieved_resource(
      resumed_source
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_null(fixture$restored$workspace$get_retrieved_source(
    resumed_source$id
  ))
  expect_identical(
    fixture$first$manifest@research_run_id,
    fixture$restored$manifest@research_run_id
  )
  expect_identical(fixture$first$manifest@status, "running")
  expect_identical(fixture$restored$manifest@status, "succeeded")

  expect_snapshot(baseline_snapshot_json(
    list(
      initial_completed_stages = baseline_succeeded_stages(
        fixture$first_events
      ),
      resumed_program_stages = resumed$program_stages,
      resumed_source_ids = resumed$source_ids,
      resumed_claims = resumed$claims,
      resumed_citations = resumed$citations,
      resumed_outline_sections = resumed$outline_sections,
      resumed_report_sections = resumed$report_sections,
      terminal_status = resumed$terminal_status,
      resumed_event_sequence = resumed$event_sequence
    )
  ))
})

test_that("Co-STORM warmup and one moderator turn are frozen", {
  baseline_local_ids()
  fixture <- costorm_product_baseline_fixture()
  semantics <- baseline_costorm_semantics(fixture)
  definition_ids <- vapply(
    semantics$report_citations$definitions,
    `[[`,
    character(1),
    "citation_id"
  )

  expect_identical(semantics$report_citations$uses, semantics$source_ids)
  expect_identical(definition_ids, semantics$source_ids)
  expect_snapshot(baseline_snapshot_json(semantics))
})

test_that("a resumed published Co-STORM session is read-only", {
  baseline_local_ids()
  fixture <- costorm_product_baseline_fixture()
  bundle <- file.path(withr::local_tempdir(), "costorm-session")
  original <- baseline_costorm_durable_state(
    fixture$session,
    fixture$report
  )
  tempest_session_save(fixture$session, bundle)

  restored <- tempest_session_resume(
    bundle,
    config = fixture$resume_runtime$config
  )
  before <- baseline_costorm_durable_state(
    restored,
    tempest_session_report_md(restored)
  )

  expect_equal(before, original)
  continuation_error <- expect_error(
    restored$step("What else should we inspect?"),
    class = "tempest_session_error",
    regexp = "product has been finalized"
  )
  expect_length(fixture$resume_runtime$moderator_calls(), 0L)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(restored$workspace),
    "sealed"
  )
  expect_snapshot(baseline_snapshot_json(
    list(
      restored = before,
      restored_status = restored$manifest@status,
      workspace_state = tempest:::tempest_research_workspace_mutation_state(
        restored$workspace
      ),
      continuation_error_class = class(continuation_error),
      moderator_call_count = length(
        fixture$resume_runtime$moderator_calls()
      )
    )
  ))
})

test_that("STORM cancellation is terminal and publishes no report", {
  baseline_local_ids()
  fixture <- storm_product_fixture()
  output_root <- withr::local_tempdir()
  collector <- tempest_progress_collector(include_payload = TRUE)
  progress <- function(event) {
    collector$record(event)
    event <- tempest_progress_event_data(event)
    if (
      identical(event$event_type, "stage") &&
        identical(event$stage, "research") &&
        identical(event$status, "started")
    ) {
      stop(structure(
        list(message = "Cancel the product baseline."),
        class = c("interrupt", "condition")
      ))
    }
  }

  condition <- tryCatch(
    tempest_run(
      "Cancelled product baseline",
      config = fixture$config,
      retriever = fixture$retriever,
      n_experts = 1,
      max_questions_per_perspective = 1,
      output_dir = output_root,
      run_id = "storm-cancel-baseline",
      progress = progress,
      verbose = FALSE
    ),
    interrupt = \(condition) condition
  )
  events <- collector$data()
  state <- tempest_progress_state(events)
  persisted <- tempest:::tempest_read_json_strict(file.path(
    output_root,
    "storm-cancel-baseline",
    "run_config.json"
  ))

  expect_identical(state$status, "cancelled")
  expect_identical(state$terminal, TRUE)
  expect_identical(
    persisted$research_manifest$status,
    "running"
  )
  expect_null(persisted$research_manifest$deliverables$report_md)
  expect_equal("report.md" %in% unlist(persisted$files), FALSE)
  expect_equal("research" %in% unlist(persisted$completed_stages), FALSE)
  expect_identical(
    fixture$program_stages(),
    c("perspectives", "personas")
  )

  expect_snapshot(baseline_snapshot_json(
    list(
      condition_class = class(condition),
      completed_stages = baseline_succeeded_stages(events),
      terminal_status = state$status,
      terminal = state$terminal,
      report_published = !is.null(
        persisted$research_manifest$deliverables$report_md
      ),
      program_stages = fixture$program_stages(),
      event_sequence = baseline_event_labels(events)
    )
  ))
})
