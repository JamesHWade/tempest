# Offline public-Graft consumer proof using synthetic completed research bundles.
artifact_research_example <- function() {
  pins <- c(
    initial = "sha256:b19dedc6127d20c515af3bcb9bae9c960bb04cf4a05af5ffafa259f7acf8c43d",
    correction = "sha256:55485f222bdcaf8aa7fa3233184029fb7aad472cfcee4a01250f727f3c5cbc1a"
  )
  inputs <- system.file("examples", "accepted-research", package = "tempest")
  directory <- tempfile("artifact-research-")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  store <- graft::graft_artifact_store(directory, create = TRUE)
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
    graft::graft_artifact_decide(
      store,
      "pilot",
      key,
      expected,
      selection,
      "accept",
      "reviewer",
      "Evidence reviewed",
      "briefing"
    )
  }
  first <- publish("initial")
  initial <- accept("day-one", NULL, first)
  store <- graft::graft_artifact_store(directory)
  unchanged <- accept("day-two", initial$id, first)
  correction <- accept("correction", unchanged$id, publish("correction"))
  knowledge <- tempest::tempest_reuse_artifact_research(
    store,
    "pilot",
    correction$id,
    "briefing",
    eligible = function(event) TRUE
  )
  historical <- tempest::tempest_read_artifact_research(
    store,
    initial$selection
  )
  withdrawal <- graft::graft_artifact_decide(
    store,
    "pilot",
    "withdraw",
    correction$id,
    correction$selection,
    "withdraw",
    "reviewer",
    "Review required",
    "briefing"
  )
  denied <- tryCatch(
    {
      tempest::tempest_reuse_artifact_research(
        store,
        "pilot",
        correction$id,
        "briefing",
        eligible = function(event) TRUE
      )
      FALSE
    },
    graft_artifact_error = function(error) TRUE
  )
  retry <- accept("day-one", NULL, first)
  stopifnot(
    denied,
    identical(retry, initial),
    identical(initial$selection, unchanged$selection),
    !identical(initial$id, unchanged$id),
    identical(graft::graft_artifact_read_decision(store, "pilot"), withdrawal)
  )
  list(
    evidence_records = length(knowledge@records),
    historical_report_retained = nzchar(historical$report_md),
    unchanged_review_is_distinct = initial$id != unchanged$id,
    corrected_selection_is_distinct = initial$selection != correction$selection,
    withdrawal_blocks_reuse = denied,
    retry_preserves_withdrawal = TRUE
  )
}
