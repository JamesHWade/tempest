test_that("platform files preserve UTF-8 across atomic replacement", {
  root <- withr::local_tempdir()
  path <- file.path(root, "records with spaces.json")
  initial <- enc2utf8("naïve café — 東京")
  replacement <- enc2utf8("über revised — 東京")

  tempest:::tempest_write_text(path, initial)
  expect_identical(tempest:::tempest_read_text(path), initial)

  tempest:::tempest_write_text(path, replacement)
  expect_identical(tempest:::tempest_read_text(path), replacement)
  expect_identical(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    file.path(
      normalizePath(root, winslash = "/", mustWork = TRUE),
      basename(path)
    )
  )
})

test_that("platform paths enforce portable bundle boundaries", {
  safe <- c("bundle.json", "nested/records.json", "nested/résumé.json")
  unsafe <- c(
    "../secret.json",
    "/tmp/secret.json",
    "~/secret.json",
    "C:/secret.json",
    "C:\\secret.json",
    "nested//secret.json",
    "nested/./secret.json"
  )

  expect_identical(
    unname(vapply(safe, tempest:::tempest_product_path_is_safe, logical(1))),
    rep(TRUE, length(safe))
  )
  expect_identical(
    unname(vapply(unsafe, tempest:::tempest_product_path_is_safe, logical(1))),
    rep(FALSE, length(unsafe))
  )
})

test_that("platform completion digests survive deep Turn duplication", {
  skip_if_not_installed("ellmer")

  response <- enc2utf8("Queued response — 東京.")
  provider_turn <- ellmer::AssistantTurn(
    list(ellmer::ContentText(response)),
    tokens = c(7, 5, 0),
    cost = 0
  )
  deputy_execution <- list(
    deputy_run_id = "deputy-run-platform",
    deputy_session_id = "deputy-session-platform",
    expert_id = "moderator",
    correlation_id = "correlation-platform"
  )
  digest <- function(turn) {
    tempest:::tempest_agent_completion_digest(
      prompt = "Queued prompt.",
      response = response,
      provider_turn = turn,
      deputy_execution = deputy_execution
    )
  }

  expect_identical(
    digest(rlang::duplicate(provider_turn, shallow = FALSE)),
    digest(provider_turn)
  )
})

test_that("platform Graft contract pin accepts the installed Graft", {
  skip_if_not_installed("graft")
  expect_no_warning(
    valid <- tempest:::tempest_graft_pin_valid(graft::graft_contract_version())
  )
  expect_identical(valid, TRUE)
})
