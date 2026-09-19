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
  checks <- 0L
  expect_error(
    tempest_reuse_artifact_research(
      store,
      "topic",
      accepted$id,
      "briefing",
      eligible = function(event) {
        checks <<- checks + 1L
        checks == 1L
      }
    ),
    "eligibility",
    class = "graft_artifact_error"
  )
  expect_identical(checks, 2L)
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
  detached <- tempest_artifact_knowledge(
    knowledge@artifact_selection,
    retained$contents
  )
  expect_error(
    tempest_knowledge_argument(detached),
    "current admission",
    class = "tempest_knowledge_error"
  )
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
  workspace <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    workspace,
    knowledge@records,
    knowledge@artifact_selection
  )
  retriever <- tempest_retriever(config = fixture$config, workspace = workspace)
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
  expect_error(
    tempest_session(
      "Briefing",
      config = fixture$config,
      experts = list(test_expert()),
      retriever = retriever
    ),
    "pinned artifact selection",
    class = "tempest_knowledge_error"
  )
  expect_error(
    tempest_run(
      "Briefing",
      config = fixture$config,
      experts = list(test_expert()),
      retriever = retriever,
      steps = "perspectives",
      verbose = FALSE
    ),
    "pinned artifact selection",
    class = "tempest_knowledge_error"
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
  root_ref <- graft::graft_artifact_read_selection(store, selection)$roots[[1L]]
  root <- graft::graft_artifact_read(store, root_ref)
  alias <- graft::graft_artifact_save(
    store,
    "aliased-research",
    root$bytes,
    "application/json",
    dependencies = root$metadata$dependencies
  )
  alias_selection <- graft::graft_artifact_select(store, list(alias))
  expect_error(
    tempest_read_artifact_research(store, alias_selection),
    "proposal identity",
    class = "tempest_knowledge_error"
  )
  alias_decision <- graft::graft_artifact_decide(
    store,
    "alias",
    "one",
    NULL,
    alias_selection,
    "accept",
    "host",
    "Reviewed",
    "briefing"
  )
  expect_error(
    tempest_reuse_artifact_research(
      store,
      "alias",
      alias_decision$id,
      "briefing",
      eligible = function(event) TRUE
    ),
    "proposal identity",
    class = "tempest_knowledge_error"
  )
  candidate <- jsonlite::fromJSON(rawToChar(root$bytes), simplifyVector = FALSE)
  duplicate <- c(candidate, list(format = "unknown-format"))
  duplicate_ref <- graft::graft_artifact_save(
    store,
    root_ref$id,
    charToRaw(as.character(jsonlite::toJSON(
      duplicate,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ))),
    "application/json",
    dependencies = root$metadata$dependencies
  )
  expect_error(
    tempest_read_artifact_research(
      store,
      graft::graft_artifact_select(store, list(duplicate_ref))
    ),
    "Unsupported",
    class = "tempest_knowledge_error"
  )
  invalid_version <- candidate$bundle
  invalid_version$schema_version <- "invalid"
  for (bad_bundle in list("bad", NULL, list(), invalid_version)) {
    malformed <- candidate
    malformed["bundle"] <- list(bad_bundle)
    malformed_ref <- graft::graft_artifact_save(
      store,
      root_ref$id,
      charToRaw(as.character(jsonlite::toJSON(
        malformed,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      ))),
      "application/json",
      dependencies = root$metadata$dependencies
    )
    error <- expect_error(
      tempest_read_artifact_research(
        store,
        graft::graft_artifact_select(store, list(malformed_ref))
      ),
      "invalid bundle",
      class = "tempest_knowledge_error"
    )
    expect_s3_class(error$parent, "tempest_promotion_error")
  }
  for (field in c("evidence", "report")) {
    valid_ref <- if (field == "report") {
      candidate$report
    } else {
      candidate$records[[1L]]$ref
    }
    for (bad_ref in list(
      "bad",
      NULL,
      list(id = valid_ref$id),
      list(id = valid_ref$id, revision = list()),
      list(id = valid_ref$id, revision = ""),
      list(id = valid_ref$id, revision = "invalid-digest"),
      list(id = valid_ref$id, revision = c("one", "two")),
      list(id = "wrong-id", revision = valid_ref$revision),
      c(valid_ref, list(extra = "unexpected")),
      rev(valid_ref)
    )) {
      malformed <- candidate
      if (field == "report") {
        malformed["report"] <- list(bad_ref)
      } else {
        malformed$records[[1L]]["ref"] <- list(bad_ref)
      }
      malformed_ref <- graft::graft_artifact_save(
        store,
        root_ref$id,
        charToRaw(as.character(jsonlite::toJSON(
          malformed,
          auto_unbox = TRUE,
          null = "null",
          digits = NA
        ))),
        "application/json",
        dependencies = root$metadata$dependencies
      )
      expect_error(
        tempest_read_artifact_research(
          store,
          graft::graft_artifact_select(store, list(malformed_ref))
        ),
        "identity",
        class = "tempest_knowledge_error"
      )
    }
  }
  named_records <- candidate
  names(named_records$records) <- paste0("record", seq_along(candidate$records))
  named_ref <- graft::graft_artifact_save(
    store,
    root_ref$id,
    charToRaw(as.character(jsonlite::toJSON(
      named_records,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ))),
    "application/json",
    dependencies = root$metadata$dependencies
  )
  expect_error(
    tempest_read_artifact_research(
      store,
      graft::graft_artifact_select(store, list(named_ref))
    ),
    "cover its evidence",
    class = "tempest_knowledge_error"
  )
  candidate$records <- candidate$records[-1L]
  forged <- graft::graft_artifact_save(
    store,
    root_ref$id,
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

test_that("an unchanged research day executes with native accepted evidence", {
  skip_if_not_installed("graft")
  initial <- test_promotion_storm_fixture(
    evidence_text = "STORM progress emits stage events."
  )
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(initial$research, store)
  accepted <- graft::graft_artifact_decide(
    store,
    "daily",
    "day-one",
    NULL,
    selection,
    "accept",
    "reviewer",
    "Reviewed evidence",
    "briefing"
  )
  reviewed <- graft::graft_artifact_decide(
    store,
    "daily",
    "day-two",
    accepted$id,
    selection,
    "accept",
    "reviewer",
    "Unchanged evidence reviewed",
    "briefing"
  )
  knowledge <- tempest_reuse_artifact_research(
    store,
    "daily",
    reviewed$id,
    "briefing",
    eligible = function(event) TRUE
  )
  fixture <- storm_product_fixture()
  original_chat <- fixture$config@chat_fn
  fixture$config@chat_fn <- function(role, model, system_prompt, echo) {
    chat <- original_chat(role, model, system_prompt, echo)
    if (identical(role, "writer")) {
      structured <- chat$chat_structured
      chat$chat_structured <- function(...) {
        result <- structured(...)
        if (is.list(result$items)) {
          result$items <- lapply(result$items, function(item) {
            item$kind <- "no_change"
            item$confidence <- "high"
            item
          })
        }
        result
      }
    }
    chat
  }
  # The fixture retriever must use the same final configuration as the run.
  fixture$retriever <- tempest_retriever(
    config = fixture$config,
    workspace = fixture$store
  )
  output <- withr::local_tempdir()
  result <- tempest_run(
    "Progress events",
    config = fixture$config,
    retriever = fixture$retriever,
    knowledge = knowledge,
    output_dir = output,
    n_experts = 1,
    max_questions_per_perspective = 1,
    verbose = FALSE
  )
  expect_identical(result@manifest@status, "succeeded")
  expect_match(tempest_report(result), "No material change", fixed = TRUE)
  sealed_before <- tempest_research_workspace_snapshot(result@workspace)
  resumed <- tempest_run(
    "Progress events",
    config = fixture$config,
    retriever = result@retriever,
    knowledge = knowledge,
    output_dir = output,
    resume = TRUE,
    n_experts = 1,
    max_questions_per_perspective = 1,
    verbose = FALSE
  )
  expect_identical(resumed@retriever, result@retriever)
  expect_identical(resumed@workspace, result@workspace)
  expect_identical(
    tempest_research_workspace_mutation_state(resumed@workspace),
    "sealed"
  )
  expect_identical(
    tempest_research_workspace_snapshot(resumed@workspace),
    sealed_before
  )
  expect_identical(tempest_report(resumed), tempest_report(result))
  expect_identical(
    result@workspace$artifact_selection,
    knowledge@artifact_selection
  )
  review <- tempest_trajectory_review(result)
  expect_identical(review@knowledge$input_selection$selection_id, selection)
  expect_identical(
    review@knowledge$input_selection$reported_decision$decision_id,
    reviewed$id
  )
  expect_identical(
    review@knowledge$input_selection$digest,
    tempest_product_record_hash(knowledge@artifact_selection)
  )
  expect_identical(
    review@knowledge$input_selection$reported_decision$verification,
    "unverified"
  )
  expect_null(review@knowledge$proposal)
  expect_null(review@knowledge$acceptance)
  expect_identical(review@knowledge$promotion_state, "none")
  expect_in(
    "artifact_selection",
    vapply(review@joins$items, `[[`, character(1), "to_type")
  )
  withdrawal <- graft::graft_artifact_decide(
    store,
    "daily",
    "withdraw",
    reviewed$id,
    selection,
    "withdraw",
    "host",
    "Reconsidered",
    "briefing"
  )
  expect_identical(tempest_trajectory_review(result), review)
  expect_error(
    tempest_session("New work", config = fixture$config, knowledge = knowledge),
    class = "graft_artifact_error"
  )

  expect_identical(
    tempest_read_artifact_research(store, selection)$report_md,
    tempest_report(initial$research)
  )
})

test_that("malformed resume snapshots retain the restoration condition", {
  expect_error(
    tempest_session_restore(1),
    class = "tempest_session_restore_error"
  )
  expect_error(
    tempest_session_restore(list(workspace = 1)),
    class = "tempest_session_restore_error"
  )
})


test_that("multiline accepted claims retain their complete no-change identity", {
  skip_if_not_installed("graft")
  statement <- "The first line remains true.\nstatement_text: The second line remains true."
  fixture <- test_promotion_storm_fixture(evidence_text = statement)
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  retained <- tempest_read_artifact_research(store, selection)
  claim_id <- paste0(
    "Claim:",
    retained$bundle@records$Claim[[1L]]$tempest_claim_id
  )
  expect_identical(
    jsonlite::fromJSON(retained$contents[[claim_id]], simplifyVector = FALSE),
    retained$bundle@records$Claim[[1L]]
  )
  accepted <- graft::graft_artifact_decide(
    store,
    "topic",
    "review",
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
  workspace <- tempest_research_workspace()
  tempest_knowledge_insert_records(
    workspace,
    knowledge@records,
    knowledge@artifact_selection
  )
  keys <- tempest_workspace_accepted_claim_keys(workspace)
  expect_identical(keys, tempest_claim_text_key(statement))
  expect_identical(
    tempest_briefing_claim_disposition(statement, keys),
    "duplicate"
  )
  expect_identical(
    tempest_briefing_claim_disposition("The first line remains true.", keys),
    "new"
  )
  tempest_knowledge_argument(knowledge)
})


test_that("snapshot validation precedes fresh artifact admission", {
  input <- test_artifact_knowledge_input()
  knowledge <- do.call(tempest_artifact_knowledge, input)
  config <- tempest_config(chat_fn = function(...) fake_chat())
  session <- tempest_session(
    "Saved research",
    config = config,
    experts = list(test_expert()),
    knowledge = knowledge
  )
  tempest_session_expert_manager(session)$get_or_create(
    session$experts[[1L]]@expert_id
  )
  snapshot <- tempest_session_snapshot(session)
  checks <- 0L
  knowledge@admission <- function() {
    checks <<- checks + 1L
    knowledge
  }
  for (field in c(
    "schema_version",
    "topic",
    "workspace",
    "suggested_questions",
    "progress_events"
  )) {
    invalid <- snapshot
    invalid[field] <- list(list(123))
    expect_error(
      tempest_session_restore(invalid, config = config, knowledge = knowledge),
      class = "tempest_session_restore_error"
    )
    expect_error(
      tempest_session_restore(invalid, config = config),
      class = "tempest_session_restore_error"
    )
  }
  for (field in c(
    "title",
    "created_at",
    "expert_version",
    "expert_fingerprint"
  )) {
    invalid <- snapshot
    if (field == "title") {
      invalid$title <- "One\nTwo"
    } else {
      invalid$expert_sessions[[1L]][[field]] <- "invalid"
    }
    expect_error(
      tempest_session_restore(invalid, config = config, knowledge = knowledge),
      class = "tempest_session_restore_error"
    )
  }
  invalid <- snapshot
  invalid$retired_expert_ids <- list(session$experts[[1L]]@expert_id)
  expect_error(
    tempest_session_restore(invalid, config = config, knowledge = knowledge),
    class = "tempest_session_restore_error"
  )
  expect_identical(checks, 0L)
  expect_error(
    tempest_session_restore(snapshot, config = config),
    class = "tempest_knowledge_error"
  )
  restored <- tempest_session_restore(
    snapshot,
    config = config,
    knowledge = knowledge
  )
  expect_identical(checks, 1L)
  expect_identical(tempest_sources(restored), tempest_sources(session))
})

test_that("malformed retained report bytes raise knowledge errors", {
  skip_if_not_installed("graft")
  fixture <- test_promotion_storm_fixture()
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  root_ref <- graft::graft_artifact_read_selection(store, selection)$roots[[1L]]
  root <- graft::graft_artifact_read(store, root_ref)
  candidate <- jsonlite::fromJSON(rawToChar(root$bytes), simplifyVector = FALSE)
  report <- graft::graft_artifact_read(store, candidate$report)
  for (bytes in list(
    c(charToRaw("bad"), as.raw(0), charToRaw("report")),
    as.raw(255),
    raw(),
    charToRaw("A different report")
  )) {
    invalid <- candidate
    invalid$report <- graft::graft_artifact_save(
      store,
      candidate$report$id,
      bytes,
      "text/markdown",
      dependencies = report$metadata$dependencies
    )
    invalid_root <- graft::graft_artifact_save(
      store,
      root_ref$id,
      charToRaw(as.character(jsonlite::toJSON(
        invalid,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      ))),
      "application/json",
      dependencies = c(list(invalid$report), report$metadata$dependencies)
    )
    expect_error(
      tempest_read_artifact_research(
        store,
        graft::graft_artifact_select(store, list(invalid_root))
      ),
      class = "tempest_knowledge_error"
    )
  }
})
