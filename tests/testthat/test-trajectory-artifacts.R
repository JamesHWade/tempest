test_that("artifact publication is distinct from recorded acceptance and current reuse", {
  fixture <- test_promotion_storm_fixture()
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  proposed <- tempest_trajectory_review(
    fixture$research,
    store = store,
    selection = selection
  )
  expect_identical(proposed@knowledge$promotion_state, "proposed")
  expect_identical(proposed@knowledge$proposal$selection_id, selection)
  expect_null(proposed@knowledge$acceptance)
  accepted <- graft::graft_artifact_decide(
    store,
    "research",
    "accept",
    NULL,
    selection,
    "accept",
    "host",
    "Reviewed",
    "briefing"
  )
  review <- tempest_trajectory_review(
    fixture$research,
    store = store,
    selection = selection,
    stream = "research",
    decision = accepted$id
  )
  expect_identical(review@knowledge$promotion_state, "accepted")
  expect_identical(review@knowledge$acceptance$decision_id, accepted$id)
  expect_identical(review@knowledge$acceptance$selection_id, selection)
  expect_identical(review@knowledge$acceptance$stream, "research")
  expect_identical(review@knowledge$acceptance$sequence, 1L)
  expect_null(review@knowledge$input_selection)
  expect_identical(test_contains_runtime_value(S7::props(review)), FALSE)
  expect_null(review@knowledge$acceptance$actor)
  expect_null(review@knowledge$acceptance$reason)
  relations <- vapply(review@joins$items, `[[`, character(1), "relation")
  expect_in(c("published_as", "accepted_as"), relations)
  withdrawal <- graft::graft_artifact_decide(
    store,
    "research",
    "withdraw",
    accepted$id,
    selection,
    "withdraw",
    "host",
    "Reconsidered",
    "briefing"
  )
  expect_identical(
    tempest_trajectory_review(
      fixture$research,
      store = store,
      selection = selection,
      stream = "research",
      decision = accepted$id
    ),
    review
  )
  expect_error(
    tempest_trajectory_review(
      fixture$research,
      store = store,
      selection = selection,
      stream = "research",
      decision = withdrawal$id
    ),
    class = "tempest_trajectory_review_error"
  )
  expect_error(
    tempest_reuse_artifact_research(
      store,
      "research",
      accepted$id,
      "briefing",
      eligible = \(event) TRUE
    ),
    class = "graft_artifact_error"
  )
})

test_that("publication review rejects cross-product and cross-selection bindings", {
  fixture <- test_promotion_storm_fixture()
  other <- test_promotion_storm_fixture(run_id = "research-other")
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  other_selection <- tempest_publish_artifact_research(other$research, store)
  accepted <- graft::graft_artifact_decide(
    store,
    "research",
    "accept",
    NULL,
    other_selection,
    "accept",
    "host",
    "Reviewed",
    "briefing"
  )
  expect_error(
    tempest_trajectory_review(
      other$research,
      store = store,
      selection = selection
    ),
    class = "tempest_trajectory_review_error"
  )
  expect_error(
    tempest_trajectory_review(
      fixture$research,
      store = store,
      selection = selection,
      stream = "research",
      decision = accepted$id
    ),
    class = "tempest_trajectory_review_error"
  )
  expect_error(
    tempest_trajectory_review(
      fixture$research,
      store = store,
      selection = selection,
      stream = "research"
    ),
    class = "tempest_trajectory_review_error"
  )
  expect_error(
    tempest_trajectory_review(fixture$research, selection = selection),
    class = "tempest_trajectory_review_error"
  )
  expect_error(
    tempest_trajectory_review(fixture$research, store = store),
    class = "tempest_trajectory_review_error"
  )
  expect_error(
    tempest_trajectory_review(
      fixture$research,
      store = store,
      selection = selection,
      stream = "missing",
      decision = accepted$id
    ),
    class = "tempest_trajectory_review_error"
  )
})

test_that("input selection projection is bounded and contains only inspection identities", {
  fixture <- test_artifact_knowledge_input()
  input <- fixture$selection
  input$records <- lapply(seq_len(251L), function(i) {
    record <- input$records[[1L]]
    record$record_id <- paste0("record-", i)
    record$dependencies <- list()
    record
  })
  knowledge <- tempest_artifact_knowledge(
    input,
    stats::setNames(
      rep(list(fixture$contents[[1L]]), 251L),
      vapply(input$records, `[[`, character(1), "record_id")
    )
  )
  manifest <- tempest_research_manifest(
    "input-review",
    "storm",
    config = tempest_config(),
    artifact_selection = knowledge@artifact_selection
  )
  value <- tempest_trajectory_knowledge(manifest, NULL, NULL)$input_selection
  expect_identical(value$selection_id, input$selection_id)
  expect_identical(
    value$digest,
    tempest_product_record_hash(knowledge@artifact_selection)
  )
  expect_identical(value$records$total, 251L)
  expect_identical(value$records$retained, 250L)
  expect_identical(value$records$omitted, 1L)
  expect_null(value$reported_decision)
  expect_null(value$records$items[[1L]]$dependencies)
  expect_null(value$records$items[[1L]]$content)
  expect_no_error(tempest_trajectory_validate_input_selection(value))
  value$records$items[[1L]]$sha256 <- "invalid"
  expect_error(
    tempest_trajectory_validate_input_selection(value),
    class = "tempest_trajectory_review_error"
  )
})

test_that("closed reviews reject obsolete schemas and inconsistent publication joins", {
  fixture <- test_promotion_storm_fixture()
  store <- graft::graft_artifact_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  event <- graft::graft_artifact_decide(
    store,
    "research",
    "accept",
    NULL,
    selection,
    "accept",
    "host",
    "Reviewed",
    "briefing"
  )
  review <- tempest_trajectory_review(
    fixture$research,
    store = store,
    selection = selection,
    stream = "research",
    decision = event$id
  )
  knowledge <- review@knowledge
  knowledge$acceptance$selection_id <- strrep("a", 64L)
  expect_error(
    tempest_trajectory_validate_knowledge(
      knowledge,
      review@product$research_run_id
    ),
    class = "tempest_trajectory_review_error"
  )
  values <- S7::props(review)
  values$schema_version <- 1L
  expect_snapshot(error = TRUE, do.call(TempestTrajectoryReview, values))
  values <- S7::props(review)
  values$joins <- tempest_trajectory_collection(
    Filter(
      \(join) !identical(join$relation, "published_as"),
      values$joins$items
    ),
    preserve_order = FALSE
  )
  values$review_id <- tempest_trajectory_digest(do.call(
    tempest_trajectory_review_payload,
    values[setdiff(names(values), "review_id")]
  ))
  expect_snapshot(error = TRUE, do.call(TempestTrajectoryReview, values))
})


test_that("host-reported decisions remain unverified and malformed metadata is harmless", {
  fixture <- test_artifact_knowledge_input()
  fixture$selection$selection_id <- strrep("a", 64L)
  event <- list(
    id = strrep("b", 64L),
    sequence = 1L,
    stream = "research",
    action = "accept",
    selection = fixture$selection$selection_id,
    purpose = fixture$selection$purpose
  )
  project <- function(event) {
    fixture$selection$provenance$graft_decision <- event
    knowledge <- do.call(tempest_artifact_knowledge, fixture)
    manifest <- tempest_research_manifest(
      "host-reported-input",
      "storm",
      config = tempest_config(),
      artifact_selection = knowledge@artifact_selection
    )
    tempest_trajectory_knowledge(manifest, NULL, NULL)
  }
  value <- project(event)
  reported <- value$input_selection$reported_decision
  expect_identical(reported$verification, "unverified")
  expect_identical(reported$decision_id, event$id)
  expect_identical(reported$sequence, 1L)
  expect_identical(value$promotion_state, "none")
  expect_null(value$acceptance)
  expect_no_error(tempest_trajectory_validate_input_selection(
    value$input_selection
  ))
  altered <- value$input_selection
  altered$reported_decision$verification <- "verified"
  expect_error(
    tempest_trajectory_validate_input_selection(altered),
    class = "tempest_trajectory_review_error"
  )
  malformed <- list("arbitrary metadata", list(), list(id = "incomplete"))
  for (field in c("sequence", "selection", "purpose", "action")) {
    invalid <- event
    invalid[[field]] <- if (identical(field, "sequence")) 1.5 else "mismatch"
    malformed[[length(malformed) + 1L]] <- invalid
  }
  for (sequence in list("1", 0, .Machine$integer.max + 1)) {
    invalid <- event
    invalid$sequence <- sequence
    malformed[[length(malformed) + 1L]] <- invalid
  }
  for (metadata in malformed) {
    projected <- project(metadata)
    expect_null(projected$input_selection$reported_decision)
    expect_identical(projected$promotion_state, "none")
    expect_null(projected$acceptance)
    expect_no_error(tempest_trajectory_validate_input_selection(
      projected$input_selection
    ))
  }
  expect_no_match(
    jsonlite::toJSON(tempest_trajectory_artifact_joins(
      value,
      "host-reported-input"
    )),
    "accepted_as",
    fixed = TRUE
  )
})
