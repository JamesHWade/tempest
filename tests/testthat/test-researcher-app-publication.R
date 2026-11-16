test_that("a verified worker report is published, previewed, and downloaded", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("zip")
  root <- withr::local_tempdir()
  fixture <- test_promotion_fixture("storm")
  process <- new.env(parent = emptyenv())
  process$alive <- TRUE
  local_mocked_bindings(tempest_researcher_app_spawn = function(
    func,
    args,
    libpath
  ) {
    report <- tempest_enrich_report(
      fixture$research,
      args$work$output_dir,
      plan = test_enriched_plan(fixture$resource@resource_id),
      allow_code = TRUE,
      executor = test_enriched_executor
    )
    saveRDS(
      list(
        ok = TRUE,
        result = list(path = report@path, status = report@status)
      ),
      args$result_path
    )
    list(
      is_alive = function() process$alive,
      kill_tree = function(...) process$alive <- FALSE,
      wait = function(...) invisible(NULL)
    )
  })
  published_path <- NULL
  shiny::testServer(
    tempest_researcher_app_server(tempest_config(), list(), NULL, root),
    {
      session$setInputs(
        topic = "Offline publication check",
        model = "openai/gpt-6.1-sol",
        n_experts = 1,
        max_rounds = 1,
        max_questions = 1,
        max_analyses = 1,
        max_illustrations = 0,
        allow_code = TRUE,
        allow_images = FALSE,
        run = 1
      )
      session$elapse(250)
      session$flushReact()
      expect_match(output$status$html, "Report ready", fixed = TRUE)
      expect_match(output$report$html, "<iframe", fixed = TRUE)
      expect_identical(output$report_ready, TRUE)
      published_path <<- published()$path
      expect_identical(
        tempest_read_enriched_report(published_path)@status,
        "complete"
      )
      expect_identical(process$alive, FALSE)
      bundle <- output$download
      unpacked <- file.path(root, "downloaded")
      utils::unzip(bundle, exdir = unpacked)
      expect_identical(
        tempest_read_enriched_report(unpacked)@status,
        "complete"
      )
    }
  )
  expect_identical(dir.exists(published_path), TRUE)
  expect_length(list.files(root, pattern = "^\\.tempest", all.files = TRUE), 0L)
})
