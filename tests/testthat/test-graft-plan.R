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

test_that("promotion plan POSIX timestamps preserve instants and fail closed", {
  instants <- as.POSIXct(
    c("2026-11-01T05:30:00Z", "2026-11-01T06:15:00Z"),
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  fallback <- as.POSIXlt(instants, tz = "America/New_York")
  expected <- c(
    "2026-11-01T05:30:00.000000Z",
    "2026-11-01T06:15:00.000000Z"
  )

  normalized <- tempest:::tempest_graft_plan_posix_value(fallback)
  expect_identical(normalized, expected)
  expect_identical(
    unname(vapply(
      normalized,
      tempest:::tempest_graft_plan_value,
      character(1)
    )),
    expected
  )

  invalid <- structure(
    c(Inf, -Inf, 1e12, -1e12, 1e300, -1e300),
    class = c("POSIXct", "POSIXt"),
    tzone = "UTC"
  )
  expect_identical(
    tempest:::tempest_graft_plan_posix_value(invalid),
    rep(NA_character_, length(invalid))
  )

  mixed <- as.POSIXct(
    c(NA_real_, -0.0000006, 0, 0.9999996),
    origin = "1970-01-01",
    tz = "UTC"
  )
  mixed_expected <- c(
    NA_character_,
    "1969-12-31T23:59:59.999999Z",
    "1970-01-01T00:00:00.000000Z",
    "1970-01-01T00:00:01.000000Z"
  )
  mixed_normalized <- tempest:::tempest_graft_plan_posix_value(mixed)
  expect_identical(mixed_normalized, mixed_expected)
  expect_identical(
    vapply(
      stats::na.omit(mixed_normalized),
      \(value) {
        identical(
          tempest:::tempest_graft_plan_timestamp_value(value),
          value
        )
      },
      logical(1)
    ),
    rep(TRUE, 3L)
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


test_that("re-verified claim text resolves to the accepted Claim record", {
  skip_if_not_installed("graft")
  first <- tempest_promotion_bundle(test_promotion_storm_fixture()$research)
  second <- tempest_promotion_bundle(
    test_promotion_storm_fixture(run_id = "research-promotion-2")$research
  )
  first_ids <- vapply(
    first@records$Claim,
    `[[`,
    character(1),
    "tempest_claim_id"
  )
  second_ids <- vapply(
    second@records$Claim,
    `[[`,
    character(1),
    "tempest_claim_id"
  )
  renamed <- second_ids[!second_ids %in% first_ids]
  expect_gt(length(renamed), 0L)
  expect_identical(
    vapply(first@records$Claim, `[[`, character(1), "statement_text"),
    vapply(second@records$Claim, `[[`, character(1), "statement_text")
  )
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))

  accepted <- tempest_graft_plan(store, first)
  graft::graft_commit(store, accepted)
  replay <- tempest_graft_plan(store, second)
  accepted_claims <- accepted@changes[accepted@changes$class == "Claim", ]
  replay_claims <- replay@changes[replay@changes$class == "Claim", ]

  expect_setequal(replay_claims$record_id, accepted_claims$record_id)
  expect_false("insert" %in% replay_claims$action)
  expect_false("new" %in% replay_claims$disposition)
  renamed_rows <- replay_claims[
    replay@records$Claim$tempest_claim_id[replay_claims$input_row] %in% renamed,
  ]
  expect_identical(unique(renamed_rows$action), "update")
  expect_identical(unique(renamed_rows$disposition), "revision")
  expect_all_true(vapply(
    strsplit(renamed_rows$changed_fields, ", ", fixed = TRUE),
    \(fields) "tempest_claim_id" %in% fields && !"statement_text" %in% fields,
    logical(1)
  ))
  sources <- replay@changes[replay@changes$class == "Source", ]
  expect_identical(unique(sources$action), "insert")
  expect_identical(unique(sources$disposition), "new")
})

test_that("source origin keys follow the locator and content hash", {
  keys <- tempest:::tempest_source_origin_keys(
    c(
      "https://example.com/a",
      " https://example.com/a ",
      "https://example.com/a"
    ),
    c("sha256:1", "sha256:1", "sha256:2")
  )

  expect_identical(keys[[2L]], keys[[1L]])
  expect_match(keys[[1L]], "^tempest-source-locator-v1:[a-f0-9]{64}$")
  expect_false(identical(keys[[3L]], keys[[1L]]))
  expect_identical(
    tempest:::tempest_source_origin_keys(character(), character()),
    character()
  )
})

test_that("repeated source locators in one bundle coalesce and re-point evidence", {
  records <- list(
    Source = list(
      list(
        tempest_source_id = "S1",
        locator = "https://example.com/a",
        content_hash = "h1"
      ),
      list(
        tempest_source_id = "S2",
        locator = "https://example.com/a",
        content_hash = "h1"
      ),
      list(
        tempest_source_id = "S3",
        locator = "https://example.com/b",
        content_hash = "h2"
      )
    ),
    EvidenceSpan = list(
      list(id = "E1", source_id = "S2"),
      list(id = "E2", source_id = "S3")
    ),
    ClaimSupport = list(
      list(
        tempest_claim_support_id = "P1",
        source_id = "S2",
        tempest_claim_id = "C1",
        evidence_span_id = "E1"
      )
    ),
    Claim = list()
  )

  coalesced <- tempest:::tempest_graft_coalesce_bundle_rows(records)$records

  expect_identical(
    vapply(coalesced$Source, `[[`, character(1), "tempest_source_id"),
    c("S1", "S3")
  )
  expect_identical(
    vapply(coalesced$EvidenceSpan, `[[`, character(1), "source_id"),
    c("S1", "S3")
  )
  expect_identical(coalesced$ClaimSupport[[1L]]$source_id, "S1")

  changed <- records
  changed$Source[[2L]]$content_hash <- "h9"
  kept <- tempest:::tempest_graft_coalesce_bundle_rows(changed)$records
  expect_identical(
    vapply(kept$Source, `[[`, character(1), "tempest_source_id"),
    c("S1", "S2", "S3")
  )
  expect_identical(kept$EvidenceSpan[[1L]]$source_id, "S2")
})

test_that("re-verified claims merge previously accepted supports into their summary", {
  skip_if_not_installed("graft")
  first <- tempest_promotion_bundle(test_promotion_storm_fixture()$research)
  second <- tempest_promotion_bundle(
    test_promotion_storm_fixture(run_id = "research-promotion-2")$research
  )
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  accepted <- tempest_graft_plan(store, first)
  graft::graft_commit(store, accepted)
  accepted_support <- accepted@records$ClaimSupport[1L, ]
  contradicting <- as.list(accepted_support)
  contradicting$pair_verification_status <- "contradicted"
  contradicting$support_score <- 0.2
  testthat::local_mocked_bindings(
    tempest_graft_evidence_call = function(store, record_id) {
      list(
        related = list(
          evidence = data.frame(
            evidence_class = "ClaimSupport",
            record = I(list(contradicting))
          )
        ),
        truncated = list(evidence = FALSE)
      )
    }
  )

  replay <- tempest_graft_plan(store, second)
  planned_claim <- replay@records$Claim[1L, ]
  proposed <- tempest:::tempest_graft_coalesce_bundle_rows(second@records)
  expected <- tempest:::tempest_graft_coalesced_claim_summary(c(
    list(contradicting),
    proposed$records$ClaimSupport
  ))

  expect_identical(planned_claim$verification_status, expected$status)
  expect_identical(planned_claim$support_score, expected$score)
  expect_false(identical(
    planned_claim$verification_status,
    second@records$Claim[[1L]]$verification_status
  ))
  commit_result <- graft::graft_commit(store, replay)
  receipt <- tempest_promotion_receipt(store, second, replay, commit_result)
  expect_s7_class(receipt, tempest:::TempestPromotionReceipt)
})
test_that("claim origin keys normalize text", {
  keys <- tempest:::tempest_claim_origin_keys(c(
    "Output held steady.",
    "  output HELD   steady",
    "Output rose."
  ))

  expect_identical(keys[[2L]], keys[[1L]])
  expect_match(keys[[1L]], "^tempest-claim-text-v1:[a-f0-9]{64}$")
  expect_false(identical(keys[[3L]], keys[[1L]]))
  expect_identical(
    tempest:::tempest_claim_origin_keys(character()),
    character()
  )
})

test_that("repeated claim text in one bundle coalesces before planning", {
  testthat::local_mocked_bindings(
    tempest_graft_coalesced_claim_summary = function(supports) {
      list(
        status = "supported",
        score = max(vapply(supports, `[[`, numeric(1), "support_score"))
      )
    }
  )
  claim <- function(id, text, score) {
    list(
      tempest_claim_id = id,
      statement_text = text,
      verification_status = "supported",
      support_score = score
    )
  }
  support <- function(id, claim_id, span, score) {
    list(
      tempest_claim_support_id = id,
      tempest_claim_id = claim_id,
      evidence_span_id = span,
      support_score = score
    )
  }
  records <- list(
    Claim = list(
      claim("C1", "Output held steady.", 0.8),
      claim("C2", "output held steady", 0.95),
      claim("C3", "Output rose.", 0.9)
    ),
    ClaimSupport = list(
      support("S1", "C1", "E1", 0.8),
      support("S2", "C2", "E1", 0.99),
      support("S3", "C2", "E2", 0.95),
      support("S4", "C3", "E1", 0.9)
    )
  )

  coalesced <- tempest:::tempest_graft_coalesce_bundle_rows(records)

  expect_identical(coalesced$alias, c(C1 = "C2", C2 = "C2", C3 = "C3"))
  expect_identical(
    vapply(coalesced$records$Claim, `[[`, character(1), "tempest_claim_id"),
    c("C2", "C3")
  )
  expect_identical(
    vapply(
      coalesced$records$ClaimSupport,
      `[[`,
      character(1),
      "tempest_claim_support_id"
    ),
    c("S2", "S3", "S4")
  )
  expect_identical(coalesced$records$Claim[[1L]]$support_score, 0.99)
  expect_identical(coalesced$records$Claim[[2L]]$support_score, 0.9)
  reversed <- tempest:::tempest_graft_coalesce_bundle_rows(list(
    Claim = rev(records$Claim),
    ClaimSupport = rev(records$ClaimSupport)
  ))
  expect_identical(
    sort(vapply(
      reversed$records$Claim,
      `[[`,
      character(1),
      "tempest_claim_id"
    )),
    c("C2", "C3")
  )
  conflicting <- records
  conflicting$Claim[[2L]]$claim_type <- "observation"
  conflicting$Claim[[1L]]$claim_type <- "finding"
  expect_error(
    tempest:::tempest_graft_coalesce_bundle_rows(conflicting),
    class = "tempest_graft_plan_error"
  )
  expect_identical(
    tempest:::tempest_graft_coalesce_bundle_rows(list(Claim = list()))$alias,
    character()
  )
})

test_that("planning refuses to reactivate a retracted accepted claim", {
  skip_if_not_installed("graft")
  first <- tempest_promotion_bundle(test_promotion_storm_fixture()$research)
  second <- tempest_promotion_bundle(
    test_promotion_storm_fixture(run_id = "research-promotion-2")$research
  )
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  accepted <- tempest_graft_plan(store, first)
  graft::graft_commit(store, accepted)
  retracted <- accepted@records$Claim
  retracted$status <- "retracted"
  graft::graft_ingest(
    store,
    list(Claim = retracted),
    graft::graft_provenance("reviewer", idempotency_key = "retract-1")
  )

  expect_error(
    tempest_graft_plan(store, second),
    class = "tempest_graft_plan_error",
    regexp = "retracted"
  )
})

test_that("planning refuses to reclassify an accepted claim across runs", {
  skip_if_not_installed("graft")
  first <- tempest_promotion_bundle(test_promotion_storm_fixture()$research)
  second <- tempest_promotion_bundle(
    test_promotion_storm_fixture(run_id = "research-promotion-2")$research
  )
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  accepted <- tempest_graft_plan(store, first)
  graft::graft_commit(store, accepted)
  testthat::local_mocked_bindings(
    tempest_graft_get_call = function(store, record_id) {
      list(record = list(status = "active", claim_type = "speculation"))
    }
  )

  expect_error(
    tempest_graft_plan(store, second),
    class = "tempest_graft_plan_error",
    regexp = "reclassifying"
  )
})
