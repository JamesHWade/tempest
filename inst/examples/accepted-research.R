# Offline host workflow; the bundled inputs are synthetic completed products.
accepted_research_example <- function() {
  recipe <- new.env(parent = baseenv())
  sys.source(
    system.file("examples", "briefing-basis.R", package = "tempest"),
    recipe
  )
  fixture_pins <- c(
    initial = "sha256:b19dedc6127d20c515af3bcb9bae9c960bb04cf4a05af5ffafa259f7acf8c43d",
    correction = "sha256:55485f222bdcaf8aa7fa3233184029fb7aad472cfcee4a01250f727f3c5cbc1a"
  )
  inputs <- system.file("examples", "accepted-research", package = "tempest")
  directory <- tempfile("accepted-research-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "knowledge.duckdb")
  store <- graft::graft_open(
    tempest::tempest_graft_schema(),
    path,
    okf = "disabled"
  )
  on.exit(graft::graft_close(store), add = TRUE)
  accept <- function(day) {
    bundle <- tempest::tempest_read_promotion_bundle(
      file.path(inputs, day),
      expected_bundle_id = fixture_pins[[day]]
    )
    plan <- tempest::tempest_graft_plan(store, bundle)
    commit <- graft::graft_commit(store, plan)
    receipt <- tempest::tempest_promotion_receipt(store, bundle, plan, commit)
    list(plan = plan, receipt = receipt)
  }
  report <- function(day) {
    paste(
      readLines(file.path(inputs, paste0(day, "-report.md")), warn = FALSE),
      collapse = "\n"
    )
  }

  first <- accept("initial")
  basis <- recipe$capture_briefing_basis(
    store,
    list(recipe$briefing_selection(first$receipt)),
    report("initial")
  )
  checkpoint <- file.path(directory, "initial.rds")
  saveRDS(basis, checkpoint)
  graft::graft_close(store)
  store <- graft::graft_open(
    tempest::tempest_graft_schema(),
    path,
    okf = "disabled"
  )
  basis <- readRDS(checkpoint)
  stopifnot(nrow(recipe$briefing_changes(store, basis)) == 0L)
  original <- recipe$read_briefing_basis(store, basis)

  corrected <- accept("correction")
  old_claim <- first$plan@records$Claim
  old_claim$status <- "superseded"
  retire <- graft::graft_plan(
    store,
    list(Claim = old_claim),
    graft::graft_provenance("pilot-review", idempotency_key = "correct-pilot")
  )
  graft::graft_commit(store, retire)
  changed <- recipe$briefing_changes(store, basis)
  next_basis <- recipe$capture_briefing_basis(
    store,
    list(recipe$briefing_selection(corrected$receipt)),
    report("correction")
  )
  current <- recipe$read_briefing_basis(store, next_basis)
  historical <- recipe$read_briefing_basis(store, basis)
  stopifnot(identical(
    lapply(original@records, function(x) x@content),
    lapply(historical@records, function(x) x@content)
  ))
  list(
    initial_report = basis$report_md,
    corrected_report = next_basis$report_md,
    selected_records = c(
      initial = length(original@records),
      corrected = length(current@records)
    ),
    reviewed_changes = changed[, c("class", "action", "record_id")],
    original_preserved = TRUE
  )
}
