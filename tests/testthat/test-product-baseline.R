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
  expect_identical(fixture$result$workspace, fixture$result$store)
  expect_identical(fixture$result$workspace, fixture$store)
  expect_identical(
    fixture$result$manifest@research_run_id,
    "storm-product-baseline"
  )
  expect_identical(fixture$result$manifest@status, "succeeded")
  expect_snapshot(baseline_snapshot_json(semantics))
})

test_that("scripted STORM resumes through the public product path", {
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
  expect_identical(fixture$restored$workspace, fixture$restored$store)
  expect_identical(
    fixture$restored$retriever$workspace,
    fixture$restored$workspace
  )
  expect_identical(
    fixture$restored$retriever$store,
    fixture$restored$workspace
  )
  resumed_source <- fake_source(
    url = "https://example.org/resumed-workspace-alias",
    title = "Resumed workspace alias"
  )
  fixture$restored$retriever$workspace$upsert_source(resumed_source)
  expect_identical(
    fixture$restored$workspace$get_source(resumed_source$id)$id,
    resumed_source$id
  )
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

test_that("a resumed Co-STORM session can continue product dialogue", {
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

  restored$step("What else should we inspect?")
  restored_events <- tempest_execution_events(restored)
  continued <- baseline_costorm_durable_state(
    restored,
    tempest_session_report_md(restored)
  )

  expect_identical(
    continued$claims[[3L]]$claim_text,
    "Continued moderator research is preserved."
  )
  moderator_calls <- fixture$resume_runtime$moderator_calls()
  expect_length(moderator_calls, 1L)
  expect_identical(moderator_calls[[1L]]$kind, "text")
  expect_match(
    moderator_calls[[1L]]$prompt,
    "Expert orientation cites evidence",
    fixed = TRUE
  )
  expect_match(
    moderator_calls[[1L]]$prompt,
    "Moderator research cites evidence",
    fixed = TRUE
  )
  expect_match(
    moderator_calls[[1L]]$prompt,
    "What should we inspect?",
    fixed = TRUE
  )
  expect_snapshot(baseline_snapshot_json(
    list(
      restored = before,
      continued = continued,
      continued_status = tempest_progress_state(restored_events)$status,
      continued_event_sequence = baseline_event_labels(restored_events)
    )
  ))
})

test_that("STORM cancellation is terminal and publishes no report", {
  baseline_local_ids()
  fixture <- storm_product_fixture()
  artifacts <- tempest_memory_artifact_store()
  fixture$config@artifact_store <- artifacts
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
    is.null(fixture$store$get_artifact("report_md")),
    TRUE
  )
  expect_identical(artifacts$exists("report_md"), FALSE)
  expect_identical(
    persisted$research_manifest$status,
    "cancelled"
  )
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
      workspace_report_published = !is.null(
        fixture$store$get_artifact("report_md")
      ),
      catalog_report_published = artifacts$exists("report_md"),
      program_stages = fixture$program_stages(),
      event_sequence = baseline_event_labels(events)
    )
  ))
})
