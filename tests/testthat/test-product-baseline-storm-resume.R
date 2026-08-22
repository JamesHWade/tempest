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
  expect_identical(fixture$restored@workspace, fixture$restored_store)
  expect_equal(
    intersect(
      names(tempest:::TempestResult@properties),
      c("store", "artifact_catalog", "workflow_run")
    ),
    character()
  )
  expect_identical(
    fixture$restored@retriever$workspace,
    fixture$restored@workspace
  )
  resumed_source <- fake_source(
    url = "https://example.org/resumed-workspace-alias",
    title = "Resumed workspace alias"
  )
  expect_error(
    fixture$restored@retriever$workspace$upsert_retrieved_resource(
      resumed_source
    ),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_null(fixture$restored@workspace$get_retrieved_source(
    resumed_source@resource_id
  ))
  expect_identical(
    fixture$first@manifest@research_run_id,
    fixture$restored@manifest@research_run_id
  )
  expect_identical(fixture$first@manifest@status, "running")
  expect_identical(fixture$restored@manifest@status, "succeeded")

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
