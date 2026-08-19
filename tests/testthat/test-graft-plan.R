test_that("promotion planning uses a deterministic private seed plan", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))

  first <- tempest_graft_plan(store, fixture$bundle)
  second <- tempest_graft_plan(store, fixture$bundle)

  expect_s7_class(first, getFromNamespace("GraftCommitPlan", "graft"))
  expect_identical(first@plan_id, second@plan_id)
  expect_identical(first@plan_digest, second@plan_digest)
  expect_identical(
    first@schema_build_digest,
    fixture$bundle@schema_build_digest
  )
  expect_identical(names(first@records), names(fixture$bundle@records))
  expect_match(first@records$Source$id, "^graft:")
  expect_match(first@records$Claim$id, "^graft:")
  expect_identical(
    first@records$EvidenceSpan$source_id,
    first@records$Source$id
  )
  expect_identical(
    first@records$ClaimSupport$statement_id,
    first@records$Claim$id
  )
  expect_identical(
    first@records$ClaimSupport$evidence_span_id,
    first@records$EvidenceSpan$id
  )
})

test_that("promotion plan timestamps are idempotent at microsecond precision", {
  timestamps <- c(
    "2026-08-19T02:08:02.857780Z",
    "2026-08-19T02:06:43.114003Z",
    "2026-08-19T02:08:02Z",
    "2026-08-19T02:08:02.8Z"
  )
  expected <- c(
    timestamps[1:2],
    "2026-08-19T02:08:02.000000Z",
    "2026-08-19T02:08:02.800000Z"
  )
  normalized <- vapply(
    timestamps,
    tempest:::tempest_graft_plan_value,
    character(1)
  )

  expect_identical(unname(normalized), unname(expected))
  expect_identical(
    unname(vapply(
      normalized,
      tempest:::tempest_graft_plan_value,
      character(1)
    )),
    unname(expected)
  )

  accepted <- as.POSIXct(
    "2026-08-19T02:08:02.857781Z",
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  planned <- tempest:::tempest_graft_plan_value(accepted)
  expect_identical(
    planned,
    "2026-08-19T02:08:02.857781Z"
  )
  expect_identical(
    tempest:::tempest_graft_record_matches(
      list(retrieved_at = planned),
      list(retrieved_at = accepted)
    ),
    TRUE
  )

  malformed <- c(
    "2026-02-30T02:08:02.857780Z",
    "2026-08-19T02:08:02.1234567Z"
  )
  expect_identical(
    vapply(
      malformed,
      \(value) {
        is.null(tempest:::tempest_graft_plan_timestamp_value(value))
      },
      logical(1)
    ),
    stats::setNames(rep(TRUE, length(malformed)), malformed)
  )
  expect_identical(
    unname(vapply(
      malformed,
      tempest:::tempest_graft_plan_value,
      character(1)
    )),
    malformed
  )
})

test_that("planning is read-only until the host accepts the final plan", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))

  plan <- tempest_graft_plan(store, fixture$bundle)
  expect_error(
    graft::graft_history(store, plan@records$Claim$id, limit = 1L),
    class = "graft_reference_error"
  )

  result <- graft::graft_commit(store, plan)
  expect_identical(result$batch_id, plan@plan_id)
})

test_that("planning binds exact manifest and schema provenance digests", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)

  replan <- function(metadata, version = plan@provenance@version) {
    graft::graft_plan(
      store,
      records = plan@records,
      provenance = graft::graft_provenance(
        producer = plan@provenance@producer,
        version = version,
        run_id = plan@provenance@run_id,
        idempotency_key = plan@provenance@idempotency_key,
        metadata = metadata
      )
    )
  }

  for (field in c(
    "research_manifest_digest",
    "schema_build_digest",
    "planning_snapshot_id"
  )) {
    metadata <- plan@provenance@metadata
    metadata[[field]] <- paste0("sha256:", strrep("0", 64L))
    tampered <- replan(metadata)
    expect_identical(tampered@valid, TRUE)
    expect_error(
      tempest:::tempest_graft_plan_assert_bundle(tampered, fixture$bundle),
      class = "tempest_graft_plan_error"
    )
  }

  extras <- list(
    c(plan@provenance@metadata, list(extra = "misleading")),
    c(
      plan@provenance@metadata,
      list(api_key = "sk-proj-12345678901234567890")
    )
  )
  for (metadata in extras) {
    tampered <- replan(metadata)
    expect_identical(tampered@valid, TRUE)
    error <- expect_error(
      tempest:::tempest_graft_plan_assert_bundle(tampered, fixture$bundle),
      class = "tempest_graft_plan_error"
    )
    expect_identical(
      grepl(
        "sk-proj-12345678901234567890",
        paste(conditionMessage(error), capture.output(error), collapse = "\n"),
        fixed = TRUE
      ),
      FALSE
    )
  }

  wrong_version <- replan(
    plan@provenance@metadata,
    version = "forged-version"
  )
  expect_error(
    tempest:::tempest_graft_plan_assert_bundle(
      wrong_version,
      fixture$bundle
    ),
    class = "tempest_graft_plan_error"
  )
})

test_that("planning failures do not expose Graft errors", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  secret <- "api_key=sk-proj-12345678901234567890"
  testthat::local_mocked_bindings(
    tempest_graft_plan_call = function(...) stop(secret)
  )

  error <- expect_error(
    tempest_graft_plan(store, fixture$bundle),
    class = "tempest_graft_plan_error"
  )
  rendered <- paste(
    conditionMessage(error),
    capture.output(error),
    collapse = "\n"
  )
  expect_identical(grepl(secret, rendered, fixed = TRUE), FALSE)
})
