test_that("completed research retains exact report, proof and source contents", {
  skip_if_not_installed("graft")
  fixture <- test_promotion_storm_fixture()
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  expect_identical(
    tempest_publish_artifact_research(fixture$research, store),
    selection
  )
  retained <- tempest_read_artifact_research(store, selection)
  expect_identical(retained$report_md, tempest_report(fixture$research))
  expect_identical(
    retained$bundle@bundle_id,
    tempest_promotion_bundle(fixture$research)@bundle_id
  )
  expect_match(
    retained$contents[[paste0("Source:", fixture$resource@resource_id)]],
    fixture$resource@content,
    fixed = TRUE
  )
  accepted <- graft::graft_artifact_decide(
    store,
    "topic",
    "review-1",
    expected = NULL,
    selection = selection,
    action = "accept",
    actor = "host",
    reason = "Reviewed",
    purpose = "briefing"
  )
  knowledge <- tempest_reuse_artifact_research(
    store,
    "topic",
    accepted$id,
    "briefing",
    eligible = function(event) TRUE
  )
  expect_identical(
    knowledge@artifact_selection$provenance$graft_decision,
    tempest_product_canonical_value(accepted)
  )
  expect_identical(knowledge@governed_procedures, list())
  expect_length(knowledge@records, length(retained$records))
  session <- tempest_session(
    "Briefing",
    config = fixture$config,
    experts = list(test_expert()),
    knowledge = knowledge
  )
  path <- tempfile()
  tempest_session_save(session, path)
  expect_error(
    tempest_session_resume(path, config = fixture$config),
    "fresh admission",
    class = "tempest_knowledge_error"
  )
  resumed <- tempest_session_resume(
    path,
    config = fixture$config,
    knowledge = knowledge
  )
  expect_identical(tempest_sources(resumed), tempest_sources(session))
  withdrawal <- graft::graft_artifact_decide(
    store,
    "topic",
    "withdraw-1",
    expected = accepted$id,
    selection = selection,
    action = "withdraw",
    actor = "host",
    reason = "Reconsider",
    purpose = "briefing"
  )
  expect_error(
    tempest_session("Briefing", config = fixture$config, knowledge = knowledge),
    "current acceptance",
    class = "graft_artifact_error"
  )
  expect_error(
    tempest_session_resume(
      path,
      config = fixture$config,
      knowledge = knowledge
    ),
    "current acceptance",
    class = "graft_artifact_error"
  )
  expect_identical(
    tempest_read_artifact_research(store, selection)$report_md,
    retained$report_md
  )
  expect_identical(
    graft::graft_artifact_decide(
      store,
      "topic",
      "review-1",
      expected = NULL,
      selection = selection,
      action = "accept",
      actor = "host",
      reason = "Reviewed",
      purpose = "briefing"
    ),
    accepted
  )
  expect_identical(
    graft::graft_artifact_read_decision(store, "topic"),
    withdrawal
  )
})

test_that("fresh reviews, corrections and current host eligibility stay distinct", {
  skip_if_not_installed("graft")
  initial <- test_promotion_storm_fixture()
  corrected <- test_promotion_storm_fixture(
    evidence_text = "Corrected evidence contradicts the initial result."
  )
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  first <- tempest_publish_artifact_research(initial$research, store)
  second <- tempest_publish_artifact_research(corrected$research, store)
  expect_identical(identical(first, second), FALSE)
  accept <- function(key, expected, selection) {
    graft::graft_artifact_decide(
      store,
      "topic",
      key,
      expected,
      selection,
      "accept",
      "reviewer",
      "Reviewed evidence",
      "briefing"
    )
  }
  old <- accept("one", NULL, first)
  unchanged <- accept("two", old$id, first)
  expect_identical(identical(old$id, unchanged$id), FALSE)
  expect_identical(old$selection, unchanged$selection)
  allowed <- TRUE
  knowledge <- tempest_reuse_artifact_research(
    store,
    "topic",
    unchanged$id,
    "briefing",
    eligible = function(event) allowed
  )
  allowed <- FALSE
  expect_error(
    tempest_session("Briefing", config = initial$config, knowledge = knowledge),
    "eligibility",
    class = "graft_artifact_error"
  )
  allowed <- TRUE
  correction <- accept("three", unchanged$id, second)
  expect_error(
    tempest_knowledge_argument(knowledge),
    "current acceptance",
    class = "graft_artifact_error"
  )
  expect_error(
    accept("three", unchanged$id, first),
    "different request",
    class = "graft_artifact_error"
  )
  expect_error(
    accept("stale", unchanged$id, first),
    "predecessor",
    class = "graft_artifact_error"
  )
  admitted <- tempest_reuse_artifact_research(
    store,
    "topic",
    correction$id,
    "briefing",
    eligible = function(event) TRUE
  )
  expect_identical(admitted@artifact_selection$selection_id, second)
  expect_identical(
    tempest_read_artifact_research(store, first)$bundle@bundle_id,
    tempest_promotion_bundle(initial$research)@bundle_id
  )
  expect_error(
    tempest_reuse_artifact_research(
      store,
      "topic",
      correction$id,
      "another-purpose",
      eligible = function(event) TRUE
    ),
    "current acceptance",
    class = "graft_artifact_error"
  )
  reopened <- callr::r(
    function(checkout, store_path, selection, decision) {
      if (!is.null(checkout)) {
        pkgload::load_all(checkout, quiet = TRUE)
      }
      store <- graft::graft_artifact_store(store_path)
      history <- tempest::tempest_read_artifact_research(store, selection)
      knowledge <- tempest::tempest_reuse_artifact_research(
        store,
        "topic",
        decision,
        "briefing",
        eligible = function(event) TRUE
      )
      list(report = history$report_md, input = knowledge@artifact_selection)
    },
    args = list(
      checkout = if (pkgload::is_dev_package("tempest")) {
        normalizePath(test_path("../.."))
      } else {
        NULL
      },
      store_path = store$path,
      selection = first,
      decision = correction$id
    )
  )
  expect_identical(reopened$report, tempest_report(initial$research))
  expect_identical(reopened$input, admitted@artifact_selection)
})

test_that("semantically invalid selections fail even with valid Graft hashes", {
  skip_if_not_installed("graft")
  fixture <- test_promotion_storm_fixture()
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  root <- graft::graft_artifact_read(
    store,
    graft::graft_artifact_read_selection(store, selection)$roots[[1L]]
  )
  candidate <- jsonlite::fromJSON(rawToChar(root$bytes), simplifyVector = FALSE)
  candidate$records <- candidate$records[-1L]
  forged <- graft::graft_artifact_save(
    store,
    "forged",
    charToRaw(as.character(jsonlite::toJSON(
      candidate,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ))),
    "application/json",
    dependencies = root$metadata$dependencies
  )
  selection <- graft::graft_artifact_select(store, list(forged))
  expect_error(
    tempest_read_artifact_research(store, selection),
    "cover its evidence",
    class = "tempest_knowledge_error"
  )
  accepted <- graft::graft_artifact_decide(
    store,
    "forged",
    "one",
    NULL,
    selection,
    "accept",
    "host",
    "Reviewed",
    "briefing"
  )
  expect_error(
    tempest_reuse_artifact_research(
      store,
      "forged",
      accepted$id,
      "briefing",
      eligible = function(event) TRUE
    ),
    "cover its evidence",
    class = "tempest_knowledge_error"
  )
})
