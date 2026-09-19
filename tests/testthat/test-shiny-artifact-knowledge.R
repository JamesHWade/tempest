test_that("Shiny creation and restore require current artifact admission", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinychat")
  app <- tempest:::tempest_shiny_module_env()
  store <- app$new_session_store()
  cfg <- tempest_config(chat_fn = function(...) fake_chat())
  supplied <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input()
  )
  admitted <- supplied
  policy <- new.env(parent = emptyenv())
  policy$allowed <- TRUE
  checks <- 0L
  supplied@admission <- function() {
    checks <<- checks + 1L
    if (!policy$allowed) {
      tempest_knowledge_abort("Current access revoked.")
    }
    admitted
  }
  bundle <- file.path(withr::local_tempdir(), "session")

  shiny::testServer(
    app$mod_chat_server,
    args = list(
      config = shiny::reactive(cfg),
      store = store,
      knowledge = function() supplied
    ),
    {
      binding <- resolve_program_binding()
      expect_identical(checks, 0L)
      created <- create_session(
        "Artifact Shiny session",
        1L,
        cfg,
        list(test_expert()),
        "artifact-shiny-session",
        binding$program_set,
        binding$knowledge
      )
      expect_r6_class(created, "TempestSession")
      expect_identical(checks, 1L)
      expect_identical(
        tempest:::tempest_session_workspace(created)$artifact_selection,
        admitted@artifact_selection
      )
      expect_equal(nrow(tempest_sources(created)), 2L)
      saved <- store$save_costorm_session(bundle)
      restored <- restore_session_bundle(saved)
      expect_identical(checks, 2L)
      expect_identical(
        tempest:::tempest_session_workspace(restored)$artifact_selection,
        admitted@artifact_selection
      )
      expect_equal(nrow(tempest_sources(restored)), 2L)
      policy$allowed <- FALSE
      expect_error(
        restore_session_bundle(saved),
        "revoked",
        class = "tempest_knowledge_error"
      )
      expect_identical(checks, 3L)
      expect_identical(store$peek_costorm_session(), restored)
    }
  )
})
