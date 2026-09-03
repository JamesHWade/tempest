test_that("the daily briefing composes review, diagnostics, and acceptance", {
  skip_if_not_installed("graft")
  skip_if_not_installed("scans", "0.0.0.9000")
  fixture <- test_promotion_bundle()
  store <- test_promotion_store()
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
  trajectory <- scans::as_trajectory_tempest(proposed)

  expect_type(report, "character")
  expect_gt(nrow(sources), 0L)
  expect_gt(nrow(claims), 0L)
  expect_gt(nrow(supports), 0L)
  expect_identical(proposed@knowledge$promotion_state, "proposed")
  expect_identical(
    scans::trajectory_info(trajectory)$source_type,
    "tempest"
  )
  expect_s3_class(scans::summarize_trajectories(trajectory), "data.frame")
  expect_s3_class(scans::scan_trajectories(trajectory), "data.frame")

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
})

test_that("the daily briefing keeps lifecycle changes for carried claims", {
  path <- system.file("doc", "daily-briefing.Rmd", package = "tempest")
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..",
      "..",
      "vignettes",
      "daily-briefing.Rmd"
    )
  }
  source <- readLines(path, warn = FALSE)
  chunk_start <- match("```{r basis}", source)
  chunk_end <- which(
    seq_along(source) > chunk_start & source == "```"
  )[[1L]]
  expressions <- as.list(parse(
    text = source[seq.int(chunk_start + 1L, chunk_end - 1L)]
  ))
  assignment <- Filter(
    function(expr) {
      is.call(expr) &&
        identical(expr[[1L]], as.name("<-")) &&
        identical(expr[[2L]], as.name("in_scope"))
    },
    expressions
  )
  expect_length(assignment, 1L)

  env <- new.env(parent = baseenv())
  env$graft_find <- \(...) data.frame(id = character())
  eval(assignment[[1L]], envir = env)

  changed <- data.frame(
    record_id = c("carried", "unrelated"),
    class = c("Claim", "Claim"),
    commit_order = c(2L, 2L)
  )
  changed$record <- I(list(
    list(status = "retracted"),
    list(status = "active")
  ))

  kept <- env$in_scope(
    changed,
    basis_claims = "carried",
    scope_view = NULL
  )

  expect_identical(kept$record_id, "carried")
})
