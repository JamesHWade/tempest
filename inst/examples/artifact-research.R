# Offline public-Graft consumer proof using synthetic completed research bundles.
artifact_research_example <- function() {
  pins <- c(
    initial = "sha256:748b600c3c198dcdf22d96a6b460a8d45c141a0e4fe4d668efc111dc753cb11f",
    correction = "sha256:f54ffb3fcaab7b368287b9354c16e3a03e22382a66239f608d2a05aa538f0dbb"
  )
  inputs <- system.file("examples", "accepted-research", package = "tempest")
  directory <- tempfile("artifact-research-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  store <- graft::graft_store(directory, create = TRUE)
  publish <- function(day) {
    bundle <- tempest::tempest_read_promotion_bundle(
      file.path(inputs, day),
      expected_bundle_id = pins[[day]]
    )
    report <- paste(
      readLines(file.path(inputs, paste0(day, "-report.md")), warn = FALSE),
      collapse = "\n"
    )
    tempest::tempest_publish_artifact_research(bundle, store, report = report)
  }
  accept <- function(key, expected, selection) {
    graft::graft_accept(
      store,
      graft::graft_read_selection(store, selection),
      "pilot",
      expected = expected,
      key = key,
      actor = "reviewer",
      reason = "Evidence reviewed",
      purpose = "briefing"
    )
  }
  first <- publish("initial")
  initial <- accept("day-one", NULL, first)
  store <- graft::graft_store(directory)
  unchanged <- accept("day-two", initial@id, first)
  correction <- accept("correction", unchanged@id, publish("correction"))
  knowledge <- tempest::tempest_reuse_artifact_research(
    store,
    "pilot",
    correction@id,
    "briefing",
    eligible = function(event) TRUE
  )
  historical <- tempest::tempest_read_artifact_research(
    store,
    initial@selection
  )
  withdrawal <- graft::graft_withdraw(
    store,
    "pilot",
    expected = correction@id,
    key = "withdraw",
    actor = "reviewer",
    reason = "Review required"
  )
  denied <- tryCatch(
    {
      tempest::tempest_reuse_artifact_research(
        store,
        "pilot",
        correction@id,
        "briefing",
        eligible = function(event) TRUE
      )
      FALSE
    },
    tempest_knowledge_error = function(error) TRUE
  )
  retry <- tryCatch(
    {
      accept("day-one", NULL, first)
      FALSE
    },
    graft_stale_review_error = function(error) TRUE
  )
  stopifnot(
    denied,
    retry,
    identical(initial@selection, unchanged@selection),
    !identical(initial@id, unchanged@id),
    identical(
      utils::tail(graft::graft_history(store, "pilot"), 1L)[[1L]],
      withdrawal
    )
  )
  list(
    evidence_records = length(knowledge@records),
    historical_report_retained = nzchar(historical$report_md),
    unchanged_review_is_distinct = initial@id != unchanged@id,
    corrected_selection_is_distinct = initial@selection != correction@selection,
    withdrawal_blocks_reuse = denied,
    retry_preserves_withdrawal = retry
  )
}
