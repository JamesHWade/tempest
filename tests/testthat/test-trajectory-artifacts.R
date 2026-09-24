test_that("artifact publication is distinct from recorded acceptance and current reuse", {
  fixture <- test_promotion_storm_fixture()
  store <- test_graft_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  proposed <- tempest_trajectory_review(
    fixture$research,
    store = store,
    selection = selection
  )
  expect_identical(proposed@knowledge$promotion_state, "proposed")
  expect_identical(proposed@knowledge$proposal$selection_id, selection)
  expect_null(proposed@knowledge$acceptance)
  accepted <- test_graft_decide(
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
  withdrawal <- test_graft_decide(
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
    class = "tempest_knowledge_error"
  )
})

test_that("publication review rejects cross-product and cross-selection bindings", {
  fixture <- test_promotion_storm_fixture()
  other <- test_promotion_storm_fixture(run_id = "research-other")
  store <- test_graft_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  other_selection <- tempest_publish_artifact_research(other$research, store)
  accepted <- test_graft_decide(
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
  store <- test_graft_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  event <- test_graft_decide(
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
  for (stream in c(
    strrep("a", 1025L),
    "line\nbreak",
    "/tmp/private"
  )) {
    invalid <- event
    invalid$stream <- stream
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
  unsafe <- fixture$selection
  unsafe$provenance$graft_decision <- event
  unsafe$provenance$graft_decision$stream <- "api_key=secret"
  expect_null(tempest_trajectory_reported_decision(unsafe))
  expect_no_match(
    jsonlite::toJSON(tempest_trajectory_artifact_joins(
      value,
      "host-reported-input"
    )),
    "accepted_as",
    fixed = TRUE
  )
})


test_that("reconstructed reviews enforce bounded and canonical input identities", {
  fixture <- test_promotion_storm_fixture()
  values <- S7::props(tempest_trajectory_review(fixture$research))
  values$knowledge$input_selection <- tempest_trajectory_input_selection(
    test_artifact_knowledge_input()$selection
  )
  values$joins <- tempest_trajectory_collection(
    c(
      values$joins$items,
      tempest_trajectory_artifact_joins(
        values$knowledge,
        values$product$research_run_id
      )
    ),
    preserve_order = FALSE
  )
  rebuild <- function(values) {
    values$review_id <- tempest_trajectory_digest(do.call(
      tempest_trajectory_review_payload,
      values[setdiff(names(values), "review_id")]
    ))
    do.call(TempestTrajectoryReview, values)
  }
  expect_s7_class(rebuild(values), TempestTrajectoryReview)
  for (field in c("record_id", "revision_id")) {
    for (identifier in c(
      strrep("a", 129L),
      "identity containing prose"
    )) {
      altered <- values
      records <- altered$knowledge$input_selection$records$items
      records[[1L]][[field]] <- identifier
      altered$knowledge$input_selection$records <- tempest_trajectory_collection(
        records,
        preserve_order = FALSE
      )
      expect_error(
        rebuild(altered),
        "bounded opaque identifier",
        class = "simpleError"
      )
    }
  }
  for (label in c(strrep("a", 1025L), "line\nbreak", " padded ")) {
    altered <- values
    altered$knowledge$input_selection$purpose <- label
    expect_error(rebuild(altered), "1024 bytes", class = "simpleError")
  }
  altered <- values
  altered$knowledge$input_selection$records <- tempest_trajectory_collection(
    rev(altered$knowledge$input_selection$records$items),
    preserve_order = TRUE
  )
  expect_error(
    rebuild(altered),
    "canonical order",
    class = "simpleError"
  )
})


test_that("decision labels retain the store bounds in reconstructed reviews", {
  fixture <- test_promotion_storm_fixture()
  store <- test_graft_store(tempfile(), create = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  event <- test_graft_decide(
    store,
    "research",
    "accept",
    NULL,
    selection,
    "accept",
    "host",
    "Reviewed",
    "Research briefing"
  )
  values <- S7::props(tempest_trajectory_review(
    fixture$research,
    store = store,
    selection = selection,
    stream = "research",
    decision = event$id
  ))
  for (field in c("stream", "purpose")) {
    for (label in c(strrep("a", 1025L), "line\nbreak", " padded ")) {
      altered <- values
      altered$knowledge$acceptance[[field]] <- label
      altered$review_id <- tempest_trajectory_digest(do.call(
        tempest_trajectory_review_payload,
        altered[setdiff(names(altered), "review_id")]
      ))
      expect_error(
        do.call(TempestTrajectoryReview, altered),
        "1024 bytes",
        class = "simpleError"
      )
    }
  }
})
