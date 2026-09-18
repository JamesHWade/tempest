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
  altered$contents[[1L]] <- NA_character_
  expect_error(
    do.call(tempest_artifact_knowledge, altered),
    "nonmissing",
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
  expect_error(
    tempest_run(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      steps = "perspectives",
      output_dir = output,
      resume = TRUE,
      verbose = FALSE
    ),
    "fresh admission",
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_run(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      steps = "perspectives",
      output_dir = output,
      knowledge = do.call(
        tempest_artifact_knowledge,
        test_artifact_knowledge_input("A corrected claim", "v2")
      ),
      resume = TRUE,
      verbose = FALSE
    ),
    class = "tempest_error"
  )
  checks <- 0L
  declined <- knowledge
  declined@admission <- function() {
    tempest_knowledge_abort("Current access revoked.")
  }
  caller_workspace <- tempest_research_workspace()
  caller_retriever <- tempest_retriever(
    config = config,
    workspace = caller_workspace
  )
  before <- tempest_research_workspace_snapshot(caller_workspace)
  expect_error(
    tempest_run(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      retriever = caller_retriever,
      knowledge = declined,
      steps = "perspectives",
      output_dir = output,
      resume = TRUE,
      verbose = FALSE
    ),
    "revoked",
    class = "tempest_knowledge_error"
  )
  expect_identical(
    tempest_research_workspace_snapshot(caller_workspace),
    before
  )
  expect_identical(
    tempest_research_workspace_mutation_state(caller_workspace),
    "open"
  )
  knowledge@admission <- function() {
    checks <<- checks + 1L
    knowledge
  }
  plain_output <- withr::local_tempdir()
  tempest_run(
    "Artifact briefing",
    config = config,
    experts = list(test_expert()),
    steps = "perspectives",
    output_dir = plain_output,
    verbose = FALSE
  )
  expect_error(
    tempest_run(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      retriever = caller_retriever,
      knowledge = knowledge,
      steps = "perspectives",
      output_dir = plain_output,
      resume = TRUE,
      verbose = FALSE
    ),
    "exact retained knowledge",
    class = "tempest_knowledge_error"
  )
  expect_identical(checks, 0L)
  expect_identical(
    tempest_research_workspace_snapshot(caller_workspace),
    before
  )
  resumed <- tempest_run(
    "Artifact briefing",
    config = config,
    experts = list(test_expert()),
    knowledge = knowledge,
    steps = "perspectives",
    output_dir = output,
    resume = TRUE,
    verbose = FALSE
  )
  expect_identical(checks, 1L)
  expect_identical(
    resumed@workspace$artifact_selection,
    knowledge@artifact_selection
  )
  expect_identical(tempest_sources(resumed), tempest_sources(result))
  input$selection <- rev(input$selection)
  input$selection$records <- lapply(input$selection$records, rev)
  readmitted <- with_mocked_bindings(
    do.call(tempest_artifact_knowledge, input),
    tempest_now_utc = \() "2040-01-01T00:00:00Z"
  )
  expect_identical(
    identical(
      readmitted@records[[1L]]@retrieved_at,
      knowledge@records[[1L]]@retrieved_at
    ),
    FALSE
  )
  resumed <- tempest_run(
    "Artifact briefing",
    config = config,
    experts = list(test_expert()),
    knowledge = readmitted,
    steps = "perspectives",
    output_dir = output,
    resume = TRUE,
    verbose = FALSE
  )
  expect_identical(tempest_sources(resumed), tempest_sources(result))
  expect_identical(
    resumed@workspace$artifact_selection,
    knowledge@artifact_selection
  )
  expert <- test_expert()
  fields <- c(
    "name",
    "title",
    "description",
    "instructions",
    "initial_questions"
  )
  expert_args <- stats::setNames(
    lapply(fields, \(field) S7::prop(expert, field)),
    fields
  )
  fresh <- callr::r(
    function(checkout, input, path, expert_args) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      knowledge <- do.call(tempest::tempest_artifact_knowledge, input)
      result <- tempest::tempest_run(
        "Artifact briefing",
        config = tempest::tempest_config(chat_fn = function(...) {
          ellmer::chat_openai(model = "gpt-4.1-mini", credentials = \() {
            "offline-test"
          })
        }),
        experts = list(do.call(tempest::tempest_expert, expert_args)),
        knowledge = knowledge,
        steps = "perspectives",
        output_dir = path,
        resume = TRUE,
        verbose = FALSE
      )
      list(
        sources = tempest::tempest_sources(result),
        read_at = knowledge@records[[1L]]@retrieved_at
      )
    },
    args = list(
      checkout = if (pkgload::is_dev_package("tempest")) {
        normalizePath(test_path("../.."))
      } else {
        NULL
      },
      input = input,
      path = output,
      expert_args = expert_args
    )
  )
  expect_identical(
    identical(fresh$read_at, knowledge@records[[1L]]@retrieved_at),
    FALSE
  )
  expect_identical(fresh$sources, tempest_sources(result))
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
    knowledge = do.call(tempest_artifact_knowledge, first),
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
    function(checkout, path, input) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      config <- tempest::tempest_config(chat_fn = function(...) {
        ellmer::chat_openai(credentials = \() "offline-test")
      })
      session <- tempest::tempest_session_resume(
        path,
        config = config,
        knowledge = do.call(tempest::tempest_artifact_knowledge, input)
      )
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
      input = corrected,
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


test_that("only explicit active artifact claims count as accepted statements", {
  for (content in c(
    "statement_text: X",
    "statement_text: X\nstatus: superseded",
    "statement_text: X\nstatus: active\nstatus: active",
    "statement_text: X\nstatement_text: Y\nstatus: active"
  )) {
    input <- test_artifact_knowledge_input()
    input$contents[[1L]] <- content
    input$selection$records[[1L]]$sha256 <- digest::digest(
      charToRaw(content),
      algo = "sha256",
      serialize = FALSE
    )
    knowledge <- do.call(tempest_artifact_knowledge, input)
    workspace <- tempest_research_workspace()
    tempest_knowledge_insert_records(
      workspace,
      knowledge@records,
      knowledge@artifact_selection
    )
    expect_identical(
      tempest_workspace_accepted_claim_keys(workspace),
      character()
    )
  }
})

test_that("malformed persisted artifact selections use the STORM restore error", {
  workspace <- tempest_research_workspace()
  metadata <- list(
    schema_version = 9L,
    workspace = tempest_storm_workspace_identity_record(workspace)
  )
  metadata$workspace$artifact_selection <- list(selection_id = "incomplete")
  expect_error(
    tempest_storm_restore_workspace(metadata),
    "artifact selection is invalid",
    class = "tempest_run_restore_error"
  )
})


test_that("re-admitted artifact input preserves the saved research workspace", {
  input <- test_artifact_knowledge_input()
  first <- do.call(tempest_artifact_knowledge, input)
  later <- with_mocked_bindings(
    do.call(tempest_artifact_knowledge, input),
    tempest_now_utc = \() "2040-01-01T00:00:00Z"
  )
  saved <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    saved,
    first@records,
    first@artifact_selection
  )
  source <- tempest_resource(
    resource_kind = "web.page",
    locator = "https://example.org/new",
    title = "New source",
    media_type = "text/plain",
    content = "New research evidence."
  )
  saved$upsert_retrieved_resource(source)
  saved$add_proposed_claim(tempest_claim(
    "New finding",
    source_ids = source@resource_id
  ))
  before <- tempest_research_workspace_snapshot(saved)
  supplied <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    supplied,
    later@records,
    later@artifact_selection
  )
  restored <- tempest_storm_assert_workspace_equivalent(supplied, saved)
  expect_identical(restored, supplied)
  expect_identical(tempest_research_workspace_snapshot(restored), before)

  changed <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    changed,
    later@records,
    later@artifact_selection
  )
  changed$upsert_retrieved_resource(source)
  supplied_before <- tempest_research_workspace_snapshot(changed)
  expect_error(
    tempest_storm_assert_workspace_equivalent(changed, saved),
    "diverges",
    class = "tempest_run_restore_error"
  )
  expect_identical(
    tempest_research_workspace_snapshot(changed),
    supplied_before
  )
})


test_that("artifact reference segments cannot collide in resource identity", {
  input <- test_artifact_knowledge_input()
  input$selection$records[[1L]]$record_id <- "a/b"
  input$selection$records[[1L]]$revision_id <- "c"
  input$selection$records[[1L]]$dependencies <- list()
  input$selection$records[[2L]]$record_id <- "a"
  input$selection$records[[2L]]$revision_id <- "b/c"
  input$selection$records[[2L]]$class <- "Claim"
  names(input$contents) <- c("a/b", "a")
  knowledge <- do.call(tempest_artifact_knowledge, input)
  session <- tempest_session(
    "Distinct artifact identities",
    config = tempest_config(chat_fn = \(...) fake_chat()),
    experts = list(test_expert()),
    knowledge = knowledge
  )
  sources <- tempest_sources(session)
  expect_equal(nrow(sources), 2L)
  expect_length(unique(sources$id), 2L)
  snapshot <- tempest_research_workspace_snapshot(
    tempest_session_workspace(session)
  )
  restored <- tempest_research_workspace_restore(snapshot)
  expect_identical(tempest_research_workspace_snapshot(restored), snapshot)
})

test_that("artifact identifiers reject surrounding whitespace before admission", {
  for (field in c("record_id", "revision_id", "class")) {
    input <- test_artifact_knowledge_input()
    input$selection$records[[1L]][[field]] <- paste0(
      " ",
      input$selection$records[[1L]][[field]],
      " "
    )
    expect_error(
      do.call(tempest_artifact_knowledge, input),
      "surrounding whitespace",
      class = "tempest_knowledge_error"
    )
  }
  for (field in c("record_id", "revision_id")) {
    input <- test_artifact_knowledge_input()
    input$selection$records[[1L]]$dependencies[[1L]][[field]] <- paste0(
      " ",
      input$selection$records[[1L]]$dependencies[[1L]][[field]],
      " "
    )
    expect_error(
      do.call(tempest_artifact_knowledge, input),
      "surrounding whitespace",
      class = "tempest_knowledge_error"
    )
  }
})

test_that("malformed persisted selections use the workspace restore error", {
  workspace <- tempest_research_workspace()
  snapshot <- tempest_research_workspace_snapshot(workspace)
  snapshot$artifact_selection <- list(selection_id = "incomplete")
  expect_error(
    tempest_research_workspace_restore(snapshot),
    "artifact_selection.*invalid",
    class = "tempest_research_workspace_restore_error"
  )
})


test_that("artifact selections canonicalize all JSON object members", {
  input <- test_artifact_knowledge_input()
  input$selection$provenance <- list(
    producer = "offline-test",
    source = list(uri = "urn:pilot", receipt = "receipt:1")
  )
  first <- do.call(tempest_artifact_knowledge, input)
  reverse_members <- function(value) {
    if (!is.list(value)) {
      return(value)
    }
    value <- lapply(value, reverse_members)
    if (!is.null(names(value))) rev(value) else value
  }
  input$selection <- reverse_members(input$selection)
  second <- do.call(tempest_artifact_knowledge, input)
  expect_identical(first@artifact_selection, second@artifact_selection)
  workspace <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    workspace,
    first@records,
    first@artifact_selection
  )
  tempest_knowledge_insert_records(
    workspace,
    second@records,
    second@artifact_selection
  )
  expect_identical(workspace$artifact_selection, first@artifact_selection)
  snapshot <- tempest_research_workspace_snapshot(workspace)
  snapshot$artifact_selection <- reverse_members(snapshot$artifact_selection)
  restored <- tempest_research_workspace_restore(snapshot)
  expect_identical(restored$artifact_selection, first@artifact_selection)
})


test_that("JSON claim inputs require explicit active status for no-change identity", {
  for (status in list(NULL, FALSE, "superseded", "active")) {
    input <- test_artifact_knowledge_input()
    content <- as.character(jsonlite::toJSON(
      list(statement_text = "The pilot recovered 82%.", status = status),
      auto_unbox = TRUE,
      null = "null"
    ))
    input$contents[[1L]] <- content
    input$selection$records[[1L]]$sha256 <- digest::digest(
      charToRaw(content),
      algo = "sha256",
      serialize = FALSE
    )
    knowledge <- do.call(tempest_artifact_knowledge, input)
    workspace <- tempest_research_workspace()
    tempest_knowledge_insert_records(
      workspace,
      knowledge@records,
      knowledge@artifact_selection
    )
    expect_identical(
      tempest_workspace_accepted_claim_keys(workspace),
      if (identical(status, "active")) {
        "the pilot recovered 82%"
      } else {
        character()
      }
    )
  }
})


test_that("STORM resume validates saved data before invoking admission", {
  knowledge <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input()
  )
  checks <- 0L
  knowledge@admission <- function() {
    checks <<- checks + 1L
    tempest_knowledge_abort("Current access revoked.")
  }
  output <- withr::local_tempdir()
  tempest_product_write_json(
    file.path(
      tempest_storm_prepare_run_dir(output, "Artifact briefing"),
      "run_config.json"
    ),
    list(schema_version = 7L)
  )
  config <- tempest_config(chat_fn = function(...) fake_chat())
  expect_error(
    tempest_run(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      knowledge = knowledge,
      output_dir = output,
      resume = TRUE,
      verbose = FALSE
    ),
    class = "tempest_run_restore_error"
  )
  expect_identical(checks, 0L)
  expect_error(
    tempest_run(
      "Artifact briefing",
      config = config,
      experts = list(test_expert()),
      knowledge = knowledge,
      output_dir = withr::local_tempdir(),
      resume = TRUE,
      verbose = FALSE
    ),
    "revoked",
    class = "tempest_knowledge_error"
  )
  expect_identical(checks, 1L)
})


test_that("Co-STORM resume cannot introduce a new artifact selection", {
  knowledge <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input()
  )
  checks <- 0L
  knowledge@admission <- function() {
    checks <<- checks + 1L
    knowledge
  }
  config <- tempest_config(chat_fn = function(...) fake_chat())
  plain_session <- tempest_session(
    "Artifact briefing",
    config = config,
    experts = list(test_expert())
  )
  expect_error(
    tempest_session_restore(
      tempest_session_snapshot(plain_session),
      config = config,
      knowledge = knowledge
    ),
    "exact retained knowledge",
    class = "tempest_knowledge_error"
  )
  expect_identical(checks, 0L)
})

test_that("STORM validates run inputs before invoking artifact admission", {
  knowledge <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input()
  )
  checks <- 0L
  knowledge@admission <- function() {
    checks <<- checks + 1L
    tempest_knowledge_abort("Current access revoked.")
  }
  config <- tempest_config(chat_fn = function(...) fake_chat())
  workspace <- tempest_research_workspace()
  retriever <- tempest_retriever(config = config, workspace = workspace)
  other_config <- tempest_config(
    chat_fn = function(...) fake_chat(),
    max_active_experts = 2L
  )
  mismatched <- tempest_retriever(config = other_config, workspace = workspace)
  before <- tempest_research_workspace_snapshot(workspace)
  args <- list(
    topic = "Artifact briefing",
    config = config,
    retriever = retriever,
    knowledge = knowledge,
    n_experts = 1L,
    steps = "perspectives",
    verbose = FALSE
  )
  for (invalid in list(
    list(topic = ""),
    list(config = list()),
    list(resume = NA),
    list(n_experts = 0L),
    list(max_rounds = 0L),
    list(progress = 1),
    list(retriever = list()),
    list(retriever = mismatched),
    list(retriever = mismatched, resume = TRUE)
  )) {
    supplied <- args
    supplied[names(invalid)] <- invalid
    expect_error(
      do.call(tempest_run, supplied),
      names(invalid)[[1L]],
      class = "tempest_error"
    )
  }
  expect_identical(checks, 0L)
  expect_error(
    do.call(tempest_run, args),
    "revoked",
    class = "tempest_knowledge_error"
  )
  expect_identical(checks, 1L)
  expect_identical(tempest_research_workspace_snapshot(workspace), before)
  expect_identical(tempest_research_workspace_mutation_state(workspace), "open")
})

test_that("Co-STORM validates session inputs before invoking artifact admission", {
  knowledge <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input()
  )
  checks <- 0L
  knowledge@admission <- function() {
    checks <<- checks + 1L
    tempest_knowledge_abort("Current access revoked.")
  }
  config <- tempest_config(chat_fn = function(...) fake_chat())
  workspace <- tempest_research_workspace()
  retriever <- tempest_retriever(config = config, workspace = workspace)
  mismatched <- tempest_retriever(
    config = tempest_config(
      chat_fn = function(...) fake_chat(),
      max_active_experts = 2L
    ),
    workspace = workspace
  )
  before <- tempest_research_workspace_snapshot(workspace)
  args <- list(
    topic = "Artifact briefing",
    config = config,
    retriever = retriever,
    knowledge = knowledge,
    n_experts = 1L
  )
  for (invalid in list(
    list(topic = ""),
    list(config = list()),
    list(n_experts = 0L),
    list(experts = list("invalid")),
    list(session_id = ""),
    list(progress = 1),
    list(retriever = list()),
    list(retriever = mismatched)
  )) {
    supplied <- args
    supplied[names(invalid)] <- invalid
    expect_error(
      do.call(tempest_session, supplied),
      names(invalid)[[1L]],
      class = "tempest_error"
    )
  }
  expect_identical(checks, 0L)
  expect_error(
    do.call(tempest_session, args),
    "revoked",
    class = "tempest_knowledge_error"
  )
  expect_identical(checks, 1L)
  expect_identical(tempest_research_workspace_snapshot(workspace), before)
  expect_identical(tempest_research_workspace_mutation_state(workspace), "open")
})

test_that("incompatible research workspaces fail before artifact admission", {
  knowledge <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input()
  )
  different <- do.call(
    tempest_artifact_knowledge,
    test_artifact_knowledge_input("A corrected claim", "v2")
  )
  checks <- 0L
  knowledge@admission <- function() {
    checks <<- checks + 1L
    tempest_knowledge_abort("Current access revoked.")
  }
  config <- tempest_config(chat_fn = function(...) fake_chat())
  for (mode in c("storm", "costorm")) {
    for (state in c("different_selection", "sealed")) {
      workspace <- tempest_research_workspace()
      retriever <- tempest_retriever(config = config, workspace = workspace)
      input <- if (state == "different_selection") different else knowledge
      tempest_knowledge_insert_records(
        workspace,
        input@records,
        input@artifact_selection
      )
      if (state == "sealed") {
        tempest_research_workspace_seal(workspace)
      }
      before <- tempest_research_workspace_snapshot(workspace)
      mutation_state <- tempest_research_workspace_mutation_state(workspace)
      args <- list(
        topic = "Artifact briefing",
        config = config,
        retriever = retriever,
        knowledge = knowledge,
        experts = list(test_expert())
      )
      if (mode == "storm") {
        args$steps <- "perspectives"
        args$verbose <- FALSE
      }
      run <- if (mode == "storm") tempest_run else tempest_session
      expect_error(
        do.call(run, args),
        if (state == "different_selection") {
          "pinned artifact selection"
        } else {
          "open workspace"
        },
        class = "tempest_knowledge_error"
      )
      expect_identical(checks, 0L)
      expect_identical(tempest_research_workspace_snapshot(workspace), before)
      expect_identical(
        tempest_research_workspace_mutation_state(workspace),
        mutation_state
      )
    }
  }
})
