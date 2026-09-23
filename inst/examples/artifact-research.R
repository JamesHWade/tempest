# Offline public-Graft consumer proof using synthetic completed research bundles.
artifact_research_example <- function() {
  pins <- c(
    initial = "sha256:5082654b0f6a348b6e1bbe053e751c3d01fbdd9b4d0c0831b8b64902bb13d4b8",
    correction = "sha256:c5d758ca494d8499f8c7c99f3b0a245c2efaeef8440176e9ea88eac0de06227c"
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
  retry <- accept("day-one", NULL, first)
  stopifnot(
    denied,
    identical(retry, initial),
    identical(initial@selection, unchanged@selection),
    !identical(initial@id, unchanged@id),
    identical(tail(graft::graft_history(store, "pilot"), 1L)[[1L]], withdrawal)
  )
  list(
    evidence_records = length(knowledge@records),
    historical_report_retained = nzchar(historical$report_md),
    unchanged_review_is_distinct = initial@id != unchanged@id,
    corrected_selection_is_distinct = initial@selection != correction@selection,
    withdrawal_blocks_reuse = denied,
    retry_preserves_withdrawal = TRUE
  )
}
