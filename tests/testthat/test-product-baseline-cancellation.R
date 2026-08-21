test_that("STORM cancellation is terminal and publishes no report", {
  baseline_local_ids()
  fixture <- storm_product_fixture()
  output_root <- withr::local_tempdir()
  collector <- tempest_progress_collector(include_payload = TRUE)
  progress <- function(event) {
    collector$record(event)
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
  persisted <- tempest:::tempest_product_read_json(file.path(
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
