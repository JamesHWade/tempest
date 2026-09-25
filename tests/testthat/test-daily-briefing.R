test_that("daily briefing checkpoints preserve exact history across processes", {
  skip_if_not_installed("graft")
  skip_if_not_installed("callr")
  host <- new.env(parent = baseenv())
  recipe <- system.file("examples", "briefing-basis.R", package = "tempest")
  sys.source(recipe, host)
  directory <- withr::local_tempdir()
  store <- test_graft_store(
    file.path(directory, "artifacts"),
    create = TRUE
  )
  initial <- test_promotion_storm_fixture("day-one", "The pilot recovered 82%.")
  selection <- tempest_publish_artifact_research(initial$research, store)
  accept <- function(key, expected, selection) {
    test_graft_decide(
      store,
      "pilot",
      key,
      expected,
      selection,
      "accept",
      "host",
      "Reviewed",
      "briefing"
    )
  }
  first <- accept("first", NULL, selection)
  basis <- host$capture_briefing_basis(store, "pilot", first$id, "briefing")
  saved <- file.path(directory, "basis.rds")
  saveRDS(basis, saved)
  original <- host$read_briefing_basis(store, basis)
  expect_identical(
    host$briefing_changes(store, basis),
    list(
      decision_changed = FALSE,
      selection_changed = FALSE,
      action = "accept"
    )
  )
  unchanged <- accept("unchanged", first$id, selection)
  expect_identical(
    host$briefing_changes(store, basis),
    list(
      decision_changed = TRUE,
      selection_changed = FALSE,
      action = "accept"
    )
  )
  expect_error(
    host$reuse_briefing_basis(store, basis, eligible = function(event) TRUE),
    class = "tempest_knowledge_error"
  )
  corrected <- test_promotion_storm_fixture(
    "corrected",
    "The corrected pilot recovered 62%."
  )
  correction <- accept(
    "correction",
    unchanged$id,
    tempest_publish_artifact_research(corrected$research, store)
  )
  expect_identical(host$read_briefing_basis(store, basis), original)
  expect_identical(host$briefing_changes(store, basis)$selection_changed, TRUE)
  fresh <- callr::r(
    function(checkout, recipe, path, saved, current) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      host <- new.env(parent = baseenv())
      sys.source(recipe, host)
      store <- graft::graft_store(path)
      historical <- host$read_briefing_basis(store, readRDS(saved))
      basis <- host$capture_briefing_basis(store, "pilot", current, "briefing")
      knowledge <- host$reuse_briefing_basis(
        store,
        basis,
        eligible = function(event) TRUE
      )
      list(
        report = historical$report_md,
        contents = historical$contents,
        selection = knowledge@artifact_selection$selection_id,
        exports = getNamespaceExports("graft")
      )
    },
    args = list(
      checkout = if (pkgload::is_dev_package("tempest")) {
        normalizePath(test_path("../.."))
      } else {
        NULL
      },
      recipe = recipe,
      path = file.path(directory, "artifacts"),
      saved = saved,
      current = correction$id
    )
  )
  expect_identical(fresh$report, original$report_md)
  expect_identical(fresh$contents, original$contents)
  expect_identical(fresh$selection, correction$selection)
})
