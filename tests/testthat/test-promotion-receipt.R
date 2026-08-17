test_that("promotion receipt binds the commit snapshot and every revision", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)

  receipt <- tempest_promotion_receipt(
    store,
    fixture$bundle,
    plan,
    result
  )

  expect_s7_class(receipt, TempestPromotionReceipt)
  expect_identical(receipt@batch_id, plan@plan_id)
  expect_identical(receipt@snapshot$batch_id, plan@plan_id)
  expect_identical(
    length(receipt@record_revisions),
    sum(vapply(plan@records, nrow, integer(1)))
  )
  expect_identical(
    unique(vapply(
      receipt@record_revisions,
      `[[`,
      character(1),
      "schema_build_digest"
    )),
    plan@schema_build_digest
  )
  expect_match(
    vapply(
      receipt@record_revisions,
      `[[`,
      character(1),
      "content_digest"
    ),
    "^sha256:[a-f0-9]{64}$"
  )
})

test_that("promotion receipt rejects an incorrect commit summary", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)
  result$observed[[1L]] <- result$observed[[1L]] + 1L

  expect_error(
    tempest_promotion_receipt(store, fixture$bundle, plan, result),
    class = "tempest_promotion_receipt_error"
  )
})

test_that("promotion receipt records an idempotent matching commit", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  first_plan <- tempest_graft_plan(store, fixture$bundle)
  graft::graft_commit(store, first_plan)

  plan <- tempest_graft_plan(store, fixture$bundle)
  expect_identical(unique(plan@changes$action), "match")
  expect_identical(
    identical(
      plan@provenance@idempotency_key,
      first_plan@provenance@idempotency_key
    ),
    FALSE
  )
  result <- graft::graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(store, fixture$bundle, plan, result)
  revision_actions <- vapply(
    receipt@record_revisions,
    `[[`,
    character(1),
    "action"
  )

  expect_identical(unique(revision_actions), "match")
  expect_identical(receipt@counts$matched, receipt@counts$observed)
  expect_identical(receipt@batch_id, plan@plan_id)
})

test_that("promotion receipt fails if the store advances after acceptance", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)
  artifact_id <- plan@records$ProgramArtifact$id[[1L]]
  procedure <- data.frame(
    tempest_governed_procedure_id = "procedure:receipt-race",
    stage = "verify_claim_support",
    program_artifact_id = artifact_id,
    contract_version = "1",
    evaluator_id = "evaluator:receipt-race",
    evaluator_version = "1"
  )
  later <- graft::graft_plan(
    store,
    records = list(GovernedProcedure = procedure),
    provenance = graft::graft_provenance(
      "receipt-race",
      idempotency_key = "receipt-race-1"
    )
  )
  expect_identical(later@valid, TRUE)
  graft::graft_commit(store, later)

  expect_error(
    tempest_promotion_receipt(store, fixture$bundle, plan, result),
    class = "tempest_promotion_receipt_error"
  )
})

test_that("promotion receipt rejects planned or exposed digest tampering", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)

  expect_error(
    tempest:::tempest_graft_receipt_content_digest(
      data.frame(
        content_digest = paste0("sha256:", strrep("0", 64L)),
        stringsAsFactors = FALSE
      ),
      plan@changes$proposed_content_digest[[1L]]
    ),
    class = "tempest_promotion_receipt_error"
  )

  data <- attr(plan, ".data", exact = TRUE)
  data$changes$proposed_content_digest[[1L]] <- paste0(
    "sha256:",
    strrep("0", 64L)
  )
  attr(plan, ".data") <- data
  expect_error(
    tempest_promotion_receipt(store, fixture$bundle, plan, result),
    class = "tempest_graft_plan_error"
  )
})

test_that("receipt validation rejects re-signed nested state tampering", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(store, fixture$bundle, plan, result)
  original <- tempest:::tempest_promotion_receipt_data(receipt)

  resign <- function(data) {
    payload <- data[setdiff(names(data), "receipt_id")]
    data$receipt_id <- tempest:::tempest_promotion_digest(payload)
    do.call(tempest:::TempestPromotionReceipt, data)
  }
  changes <- list(
    function(data) {
      data$snapshot$store_id <- "graft-store-forged"
      data
    },
    function(data) {
      data$store_id <- "graft-store-forged"
      data$snapshot$store_id <- data$store_id
      data
    },
    function(data) {
      data$snapshot$store_format_version <- "4.0.0"
      data
    },
    function(data) {
      data$counts$observed$Claim <- data$counts$observed$Claim + 1L
      data
    },
    function(data) {
      data$record_revisions <- c(
        data$record_revisions,
        data$record_revisions[1L]
      )
      data
    },
    function(data) {
      data$record_revisions[[1L]]$content_digest <- "not-a-digest"
      data
    },
    function(data) {
      data$record_revisions[[1L]]$action <- "match"
      data
    }
  )

  for (change in changes) {
    expect_error(resign(change(original)), class = "error")
  }
})

test_that("receipt failures do not expose Graft errors", {
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)
  secret <- "api_key=sk-proj-12345678901234567890"
  testthat::local_mocked_bindings(
    tempest_graft_snapshot_call = function(...) stop(secret)
  )

  error <- expect_error(
    tempest_promotion_receipt(store, fixture$bundle, plan, result),
    class = "tempest_promotion_receipt_error"
  )
  rendered <- paste(
    conditionMessage(error),
    capture.output(error),
    collapse = "\n"
  )
  expect_identical(grepl(secret, rendered, fixed = TRUE), FALSE)
})
