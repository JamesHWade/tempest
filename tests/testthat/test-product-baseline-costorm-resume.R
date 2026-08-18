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
