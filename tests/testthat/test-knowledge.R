test_that("tempest_knowledge pins one immutable Graft view", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()

  knowledge <- tempest_knowledge(fixture$view)

  expect_s7_class(knowledge, tempest:::TempestKnowledge)
  expect_identical(knowledge@record_ids, character())
  expect_identical(knowledge@records, list())
  expect_identical(knowledge@governed_procedures, list())
  expect_identical(
    knowledge@reference,
    tempest:::tempest_snapshot_reference(
      tempest:::tempest_governed_procedure_view_snapshot(fixture$view)
    )
  )
})

test_that("tempest_knowledge rejects an invalid pinned view", {
  expect_error(
    tempest_knowledge(list(not = "a view")),
    class = "tempest_knowledge_error"
  )
})

test_that("tempest_knowledge enforces the accepted record allowlist", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()

  expect_error(
    tempest_knowledge(
      fixture$view,
      record_ids = "org:knowledge-view-fixture"
    ),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_knowledge(fixture$view, record_ids = "graft-record:missing"),
    class = "tempest_knowledge_error"
  )
})

test_that("tempest_knowledge requires an exact bounded record allowlist", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()

  expect_error(
    tempest_knowledge(fixture$view, record_ids = c("record:a", "record:a")),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_knowledge(
      fixture$view,
      record_ids = paste0(
        "record:",
        seq_len(tempest:::tempest_knowledge_max_records() + 1L)
      )
    ),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_knowledge(fixture$view, record_ids = list("record:a")),
    class = "tempest_error"
  )
})

test_that("tempest_knowledge separates evidence reads from executable authority", {
  expect_setequal(
    tempest:::tempest_knowledge_record_allowlist(),
    c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
  )
  expect_setequal(
    intersect(
      tempest:::tempest_knowledge_record_allowlist(),
      c("GovernedProcedure", "ProgramArtifact")
    ),
    character()
  )
})

test_that("tempest_knowledge materializes complete canonical records", {
  expect_error(
    tempest:::tempest_knowledge_record_text(
      list(claim_text = list(function() "not data")),
      "record:a"
    ),
    class = "tempest_knowledge_error"
  )
  expect_identical(
    tempest:::tempest_knowledge_record_text(
      list(claim_id = "C1", claim_text = "Evidence text", page = 4L),
      "record:a"
    ),
    "claim_id: C1\nclaim_text: Evidence text\npage: 4"
  )
  expect_identical(
    tempest:::tempest_knowledge_record_text(
      list(
        claim_id = "C1",
        about = list("entity:z", "entity:a"),
        scores = c(0.8, 0.9),
        asserted_at = as.POSIXct(
          "2026-08-27T12:34:56Z",
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
      ),
      "record:a"
    ),
    paste(
      "claim_id: C1",
      'about: ["entity:z","entity:a"]',
      "scores: [0.8,0.9]",
      "asserted_at: 2026-08-27T12:34:56.000000Z",
      sep = "\n"
    )
  )
})

test_that("tempest_knowledge validates governed-procedure stage bindings", {
  skip_if_not_installed("graft")
  fixture <- test_knowledge_view()

  expect_error(
    tempest_knowledge(
      fixture$view,
      governed_procedures = list(not_a_stage = "record:a")
    ),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_knowledge(
      fixture$view,
      governed_procedures = list(personas = 42L)
    ),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_knowledge(fixture$view, governed_procedures = list("record:a")),
    class = "tempest_knowledge_error"
  )
})

test_that("product entry points accept only the validated knowledge value", {
  expect_error(
    tempest_run("Topic", knowledge = list(view = "raw")),
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_session("Topic", knowledge = "raw-view"),
    class = "tempest_knowledge_error"
  )
  expect_identical(
    "knowledge_view" %in% names(formals(tempest_run)),
    FALSE
  )
  expect_identical(
    "program_set" %in% names(formals(tempest_run)),
    FALSE
  )
  expect_identical(
    "knowledge_view" %in% names(formals(tempest_session)),
    FALSE
  )
  expect_identical(
    "program_set" %in% names(formals(tempest_session)),
    FALSE
  )
})

test_that("an absent knowledge value resolves the builtin program set", {
  resolved <- tempest:::tempest_knowledge_argument(NULL)

  expect_null(resolved$value)
  expect_null(resolved$view)
  expect_identical(resolved$records, list())
  expect_s7_class(resolved$program_set, tempest:::TempestProgramSet)
})
