test_that("artifact knowledge validates exact bytes, references and dependency closure", {
  input <- test_artifact_knowledge_input()
  knowledge <- do.call(tempest_artifact_knowledge, input)
  expect_null(knowledge@view)
  expect_null(knowledge@snapshot)
  expect_identical(knowledge@governed_procedures, list())
  expect_identical(knowledge@record_ids, names(input$contents))
  expect_identical(
    lapply(knowledge@records, \(resource) resource@content),
    unname(input$contents)
  )
  altered <- input
  altered$contents[[1L]] <- "Changed evidence"
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    "SHA-256",
    class = "tempest_knowledge_error"
  )
  altered <- input
  altered$contents <- altered$contents[-1L]
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    "exactly once",
    class = "tempest_knowledge_error"
  )
  altered <- input
  altered$selection$records <- altered$selection$records[-2L]
  altered$contents <- altered$contents[-2L]
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    "exact dependency",
    class = "tempest_knowledge_error"
  )
  altered <- input
  altered$selection$records[[1L]]$class <- "GovernedProcedure"
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    "not readable evidence",
    class = "tempest_knowledge_error"
  )
  altered <- input
  altered$selection$records[[2L]] <- altered$selection$records[[1L]]
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    class = "tempest_knowledge_error"
  )
  altered <- input
  altered$selection$records[[1L]]$dependencies[[
    1L
  ]]$revision_id <- "wrong-revision"
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    "exact dependency",
    class = "tempest_knowledge_error"
  )
})

test_that("both public research constructors admit the artifact selection", {
  input <- test_artifact_knowledge_input()
  knowledge <- do.call(tempest_artifact_knowledge, input)
  config <- tempest_config(chat_fn = \(...) fake_chat())
  session <- tempest_session(
    "Artifact briefing",
    config = config,
    experts = list(test_expert()),
    knowledge = knowledge
  )
  workspace <- tempest:::tempest_session_workspace(session)
  expect_identical(workspace$artifact_selection, knowledge@artifact_selection)
  expect_length(workspace$list_retrieved_resources(), 2L)
  expect_equal(nrow(tempest_sources(session)), 2L)
  expect_equal(nrow(tempest_claims(session)), 0L)
  expect_identical(
    tempest:::tempest_workspace_accepted_claim_keys(workspace),
    "the pilot recovered 82%"
  )
  expect_null(workspace$graft_snapshot)
  local_mocked_bindings(
    tempest_wiki_search = function(...) {
      data.frame(title = character(), url = character(), snippet = character())
    },
    tempest_extract_toc_from_url = \(...) character()
  )
  config <- tempest_config(chat_fn = function(...) {
    fake_chat_r6(list(
      chat_structured = function(...) {
        list(
          title = "Artifact briefing",
          perspectives = list(list(
            name = "Analyst",
            description = "Reads retained evidence",
            key_questions = "What changed?"
          ))
        )
      }
    ))
  })
  previous_cache <- dsprrr::configure_cache(enable = FALSE)
  withr::defer({
    if (is.null(previous_cache)) {
      dsprrr::configure_cache()
    } else {
      do.call(dsprrr::configure_cache, previous_cache)
    }
  })
  output <- withr::local_tempdir()
  result <- tempest_run(
    "Artifact briefing",
    config = config,
    experts = list(test_expert()),
    knowledge = knowledge,
    steps = "perspectives",
    output_dir = output,
    verbose = FALSE
  )
  expect_identical(
    result@workspace$artifact_selection,
    knowledge@artifact_selection
  )
  expect_length(result@workspace$list_retrieved_resources(), 2L)
  resumed <- tempest_run(
    "Artifact briefing",
    config = config,
    experts = list(test_expert()),
    steps = "perspectives",
    output_dir = output,
    resume = TRUE,
    verbose = FALSE
  )
  expect_identical(
    resumed@workspace$artifact_selection,
    knowledge@artifact_selection
  )
  expect_identical(tempest_sources(resumed), tempest_sources(result))
})

test_that("saved sessions retain artifact inputs across processes and corrections", {
  first <- test_artifact_knowledge_input()
  corrected <- test_artifact_knowledge_input(
    "The corrected result is 62%.",
    "v2"
  )
  config <- tempest_config(chat_fn = \(...) fake_chat())
  directory <- withr::local_tempdir()
  for (name in c("initial", "corrected")) {
    input <- if (name == "initial") first else corrected
    session <- tempest_session(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      knowledge = do.call(tempest_artifact_knowledge, input)
    )
    tempest_session_save(session, file.path(directory, name))
  }
  resumed <- tempest_session_resume(
    file.path(directory, "initial"),
    config = config
  )
  expect_identical(
    tempest:::tempest_session_workspace(resumed)$artifact_selection,
    tempest_artifact_selection(first$selection)
  )
  expect_identical(
    tempest:::tempest_workspace_accepted_claim_keys(tempest:::tempest_session_workspace(
      resumed
    )),
    "the pilot recovered 82%"
  )
  fresh <- callr::r(
    function(checkout, path) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      config <- tempest::tempest_config(chat_fn = function(...) {
        ellmer::chat_openai(credentials = \() "offline-test")
      })
      session <- tempest::tempest_session_resume(path, config = config)
      workspace <- tempest:::tempest_session_workspace(session)
      list(
        selection = workspace$artifact_selection,
        keys = tempest:::tempest_workspace_accepted_claim_keys(workspace),
        graft_loaded = "graft" %in% loadedNamespaces()
      )
    },
    args = list(
      checkout = if (pkgload::is_dev_package("tempest")) {
        normalizePath(test_path("../.."))
      } else {
        NULL
      },
      path = file.path(directory, "corrected")
    )
  )
  expect_identical(
    fresh$selection,
    tempest_artifact_selection(corrected$selection)
  )
  expect_identical(fresh$keys, "the corrected result is 62%")
  expect_identical(fresh$graft_loaded, FALSE)
})

test_that("workspace persistence rejects missing or modified artifact evidence", {
  input <- test_artifact_knowledge_input()
  knowledge <- do.call(tempest_artifact_knowledge, input)
  workspace <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    workspace,
    knowledge@records,
    knowledge@artifact_selection
  )
  original <- tempest_research_workspace_snapshot(workspace)
  altered <- original
  altered$retrieved_resources <- altered$retrieved_resources[-1L]
  expect_error(
    tempest_research_workspace_restore(altered),
    class = "tempest_error"
  )
  altered <- original
  altered$artifact_selection <- list()
  expect_error(
    tempest_research_workspace_restore(altered),
    class = "tempest_error"
  )
  altered <- original
  altered$artifact_selection$purpose <- "different-purpose"
  expect_error(
    tempest_research_workspace_restore(altered),
    class = "tempest_error"
  )
  altered <- knowledge
  altered@artifact_selection$records <- altered@artifact_selection$records[-1L]
  expect_error(
    tempest_knowledge_argument(altered),
    class = "tempest_knowledge_error"
  )
})


test_that("changing a pinned selection fails without mutating the workspace", {
  first <- do.call(tempest_artifact_knowledge, test_artifact_knowledge_input())
  second <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input("Correction", "v2")
  )
  workspace <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    workspace,
    first@records,
    first@artifact_selection
  )
  before <- tempest_research_workspace_snapshot(workspace)
  expect_error(
    tempest_knowledge_insert_records(
      workspace,
      second@records,
      second@artifact_selection
    ),
    class = "tempest_knowledge_error"
  )
  expect_identical(tempest_research_workspace_snapshot(workspace), before)
})


test_that("artifact input bounds cover both admission and restored content", {
  input <- test_artifact_knowledge_input()
  input$selection$purpose <- "Tempest research"
  expect_s3_class(
    do.call(tempest_artifact_knowledge, input),
    "tempest_knowledge"
  )
  input$contents[[1L]] <- strrep("x", 1024^2 + 1L)
  input$selection$records[[1L]]$sha256 <- digest::digest(
    charToRaw(input$contents[[1L]]),
    algo = "sha256",
    serialize = FALSE
  )
  expect_error(
    do.call(tempest_artifact_knowledge, input),
    "exceed 1 MiB",
    class = "tempest_knowledge_error"
  )
  selection <- tempest_artifact_selection(input$selection)
  resources <- lapply(selection$records, function(ref) {
    tempest_artifact_resource(selection, ref, input$contents[[ref$record_id]])
  })
  expect_error(
    tempest_artifact_selection_validate(selection, resources),
    "exceed 1 MiB",
    class = "tempest_knowledge_error"
  )
  input <- test_artifact_knowledge_input()
  input$selection$provenance$oversized <- strrep("x", 1024^2)
  expect_error(
    do.call(tempest_artifact_knowledge, input),
    "metadata exceeds",
    class = "tempest_knowledge_error"
  )
})
