test_that("the daily briefing restores accepted evidence in a fresh process", {
  skip_if_not_installed("graft")
  fixture <- test_promotion_bundle()
  store_path <- withr::local_tempfile(fileext = ".duckdb")
  store <- graft::graft_open(
    tempest_graft_schema(),
    store_path,
    okf = "disabled"
  )
  withr::defer(graft::graft_close(store))

  before <- graft::graft_snapshot(store)
  report <- tempest_report(fixture$research)
  sources <- tempest_sources(fixture$research)
  claims <- tempest_claims(fixture$research)
  supports <- tempest_claim_supports(fixture$research)
  plan <- tempest_graft_plan(store, fixture$bundle)
  proposed <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle
  )

  expect_type(report, "character")
  expect_gt(nrow(sources), 0L)
  expect_gt(nrow(claims), 0L)
  expect_gt(nrow(supports), 0L)
  expect_identical(proposed@knowledge$promotion_state, "proposed")

  commit_result <- graft::graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(
    store,
    fixture$bundle,
    plan,
    commit_result
  )
  accepted <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle,
    promotion_receipt = receipt
  )

  expect_identical(accepted@knowledge$promotion_state, "accepted")

  next_plan <- tempest_graft_plan(store, fixture$bundle)
  expect_identical(unique(next_plan@changes$action), "match")
  expect_identical(unique(next_plan@changes$disposition), "duplicate")
  expect_identical(unique(plan@changes$disposition), "new")

  changes <- graft::graft_changes(store, since = before)
  expect_identical(unique(changes$action), "insert")
  expect_setequal(
    changes$record_id,
    vapply(receipt@record_revisions, `[[`, character(1), "record_id")
  )
  expect_identical(
    nrow(graft::graft_changes(store, since = commit_result$batch_id)),
    0L
  )

  next_view <- graft::graft_at(store, graft::graft_snapshot(store))
  evidence_revisions <- Filter(
    \(revision) {
      revision$class %in%
        c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
    },
    receipt@record_revisions
  )
  record_ids <- vapply(
    evidence_revisions,
    `[[`,
    character(1),
    "record_id"
  )
  knowledge <- tempest_knowledge(next_view, record_ids = record_ids)

  expect_setequal(knowledge@record_ids, record_ids)
  checkpoint <- withr::local_tempfile(fileext = ".rds")
  saveRDS(
    list(snapshot = knowledge@snapshot, record_ids = record_ids),
    checkpoint
  )
  expected <- lapply(knowledge@records, function(resource) {
    list(content = resource@content, metadata = resource@metadata)
  })
  expect_setequal(
    vapply(
      expected,
      function(record) record$metadata$graft_revision_id,
      character(1)
    ),
    vapply(evidence_revisions, `[[`, character(1), "revision_id")
  )
  graft::graft_close(store)
  restored <- callr::r(
    function(checkout, store_path, checkpoint) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      store <- graft::graft_open(
        tempest::tempest_graft_schema(),
        store_path,
        read_only = TRUE,
        okf = "disabled"
      )
      on.exit(graft::graft_close(store))
      basis <- readRDS(checkpoint)
      view <- graft::graft_at(store, basis$snapshot)
      knowledge <- tempest::tempest_knowledge(
        view,
        record_ids = basis$record_ids
      )
      list(
        changes = nrow(graft::graft_changes(store, since = basis$snapshot)),
        records = lapply(knowledge@records, function(resource) {
          list(content = resource@content, metadata = resource@metadata)
        })
      )
    },
    args = list(
      checkout = if (pkgload::is_dev_package("tempest")) {
        normalizePath(test_path("../.."))
      } else {
        NULL
      },
      store_path = store_path,
      checkpoint = checkpoint
    )
  )
  expect_identical(restored$changes, 0L)
  expect_identical(restored$records, expected)
})

test_that("a briefing retains complete evidence across unchanged days and corrections", {
  skip_if_not_installed("graft")
  host <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "briefing-basis.R", package = "tempest"),
    host
  )
  initial <- test_promotion_storm_fixture(
    "pilot-initial",
    "The pilot recovered 82% of the material."
  )
  correction <- test_promotion_storm_fixture(
    "pilot-correction",
    "The corrected pilot result is 62%, not 82%."
  )
  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  accept <- function(research) {
    bundle <- tempest_promotion_bundle(research)
    plan <- tempest_graft_plan(store, bundle)
    commit <- graft::graft_commit(store, plan)
    receipt <- tempest_promotion_receipt(store, bundle, plan, commit)
    list(receipt = receipt, plan = plan)
  }
  empty <- host$capture_briefing_basis(store, list())
  expect_length(host$read_briefing_basis(store, empty)@records, 0L)
  first <- accept(initial$research)
  selections <- list(host$briefing_selection(first$receipt))
  basis <- host$capture_briefing_basis(
    store,
    selections,
    tempest_report(initial$research)
  )
  original <- host$read_briefing_basis(store, basis)
  evidence <- function(knowledge) {
    lapply(knowledge@records, function(record) {
      list(content = record@content, metadata = record@metadata)
    })
  }
  expect_length(original@records, 4L)
  expect_setequal(
    vapply(
      original@records,
      function(x) x@metadata$graft_record_class,
      character(1)
    ),
    c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
  )
  checkpoint <- withr::local_tempfile(fileext = ".rds")
  saveRDS(basis, checkpoint)
  unchanged <- readRDS(checkpoint)
  expect_identical(nrow(host$briefing_changes(store, unchanged)), 0L)
  expect_identical(unchanged, basis)
  expect_identical(
    evidence(host$read_briefing_basis(store, unchanged)),
    evidence(original)
  )

  later <- accept(correction$research)
  combined <- host$capture_briefing_basis(
    store,
    c(selections, list(host$briefing_selection(later$receipt)))
  )
  expect_length(combined$record_ids, 8L)
  expect_match(basis$report_md, "82%", fixed = TRUE)
  old_claim <- first$plan@records$Claim
  old_claim$status <- "superseded"
  retire <- graft::graft_plan(
    store,
    list(Claim = old_claim),
    graft::graft_provenance("host-review", idempotency_key = "pilot-correction")
  )
  graft::graft_commit(store, retire)
  changed <- host$briefing_changes(store, basis)
  expect_identical(changed$record_id, old_claim$id)
  expect_identical(changed$record[[1L]]$status, "superseded")
  expect_identical(
    evidence(host$read_briefing_basis(store, basis)),
    evidence(original)
  )
  expect_snapshot(error = TRUE, host$capture_briefing_basis(store, selections))

  corrected <- host$capture_briefing_basis(
    store,
    list(host$briefing_selection(later$receipt)),
    tempest_report(correction$research)
  )
  current <- host$read_briefing_basis(store, corrected)
  expect_length(current@records, 4L)
  expect_match(corrected$report_md, "62%, not 82%", fixed = TRUE)
  expect_identical(nrow(host$briefing_changes(store, corrected)), 0L)
  expect_identical(readRDS(checkpoint)$report_md, basis$report_md)
  partial <- corrected
  partial$record_ids <- partial$record_ids[-1L]
  expect_snapshot(error = TRUE, host$read_briefing_basis(store, partial))

  active_update <- later$plan@records$Claim
  active_update$support_score <- 0.01
  graft::graft_commit(
    store,
    graft::graft_plan(
      store,
      list(Claim = active_update),
      graft::graft_provenance("another-host", idempotency_key = "score-update")
    )
  )
  expect_snapshot(
    error = TRUE,
    host$capture_briefing_basis(store, corrected$selections)
  )
  expect_identical(
    evidence(host$read_briefing_basis(store, corrected)),
    evidence(current)
  )

  reviewed <- accept(correction$research)
  refreshed <- host$capture_briefing_basis(
    store,
    c(list(host$briefing_selection(reviewed$receipt)), corrected$selections)
  )
  expect_identical(refreshed$record_ids, corrected$record_ids)
  expect_equal(
    graft::graft_history(store, active_update$id, limit = 1L)$revision_number,
    3L
  )
  expect_length(host$read_briefing_basis(store, refreshed)@records, 4L)
  stale <- refreshed
  stale$selections <- corrected$selections
  expect_snapshot(error = TRUE, host$read_briefing_basis(store, stale))
})
