test_that("trajectory review returns the exact bounded STORM projection", {
  fixture <- test_promotion_fixture("storm")
  before <- tempest:::tempest_research_workspace_snapshot(fixture$workspace)

  review <- tempest_trajectory_review(fixture$research)
  repeated <- tempest_trajectory_review(fixture$research)

  expect_s7_class(review, tempest:::TempestTrajectoryReview)
  expect_named(
    formals(tempest_trajectory_review),
    c("research", "promotion_bundle", "promotion_receipt")
  )
  expect_identical(
    S7::prop_names(review),
    tempest:::tempest_trajectory_review_fields()
  )
  expect_identical(review@schema_version, 1L)
  expect_identical(review@review_id, repeated@review_id)
  expect_match(review@review_id, "^sha256:[a-f0-9]{64}$")
  expect_named(
    review@product,
    tempest:::tempest_trajectory_product_fields()
  )
  expect_identical(review@product$mode, "storm")
  expect_identical(review@product$status, "succeeded")
  expect_named(review@programs, tempest:::tempest_program_set_stages())
  expect_length(review@programs, 10L)
  for (lane in c("stages", "agent_runs", "evidence", "joins", "findings")) {
    expect_named(
      S7::prop(review, lane),
      tempest:::tempest_trajectory_collection_fields()
    )
    expect_lte(S7::prop(review, lane)$retained, 250L)
  }
  expect_identical(
    tempest:::tempest_research_workspace_snapshot(fixture$workspace),
    before
  )
  expect_identical(
    test_contains_runtime_value(S7::props(review)),
    FALSE
  )
  projection <- tempest:::tempest_product_canonical_json(S7::props(review))
  expect_identical(grepl("event", projection, fixed = TRUE), FALSE)
  expect_identical(
    grepl(fixture$resource@content, projection, fixed = TRUE),
    FALSE
  )
})

test_that("trajectory review accepts the exact public tempest_run outline", {
  result <- storm_product_baseline_fixture()$result
  public_bullets <- result$outline$sections[[1L]]$subsections[[1L]]$bullets
  state_bullets <- result$state$outline$sections[[1L]]$subsections[[1L]]$bullets
  expected_attempts <- vapply(
    result$state$stage_records,
    \(record) record@attempt_id,
    character(1)
  )

  expect_type(public_bullets, "character")
  expect_type(state_bullets, "list")
  review <- tempest_trajectory_review(result)
  expect_identical(
    vapply(review@stages$items, `[[`, character(1), "attempt_id"),
    expected_attempts
  )
})

test_that("trajectory review retains exact noncausal Deputy identities", {
  fixture <- test_promotion_fixture("costorm")
  review <- tempest_trajectory_review(fixture$research)

  expect_identical(review@product$mode, "costorm")
  expect_gt(review@agent_runs$total, 0L)
  expect_named(
    review@agent_runs$items[[1L]],
    tempest:::tempest_trajectory_agent_fields()
  )
  relations <- vapply(
    review@joins$items,
    `[[`,
    character(1),
    "relation"
  )
  expect_setequal(
    unique(relations),
    intersect(unique(relations), tempest:::tempest_trajectory_relations())
  )
  expect_length(intersect(relations, c("caused_by", "produced_by")), 0L)
  correlated <- Filter(
    \(join) identical(join$relation, "correlated_with"),
    review@joins$items
  )
  expect_gt(length(correlated), 0L)
  expect_identical(
    unique(vapply(correlated, \(join) join$proof$kind, character(1))),
    "correlation_only"
  )
  rendered <- tempest:::tempest_product_canonical_json(
    review@agent_runs$items
  )
  expect_identical(
    any(vapply(
      c("prompt", "response", "tool_input", "tool_result"),
      \(pattern) grepl(pattern, rendered, fixed = TRUE),
      logical(1)
    )),
    FALSE
  )
})

test_that("trajectory review rejects changed live Co-STORM ProgramSets", {
  session <- test_promotion_fixture("costorm")$research
  private <- session$.__enclos_env__$private
  original <- private$program_set_value
  withr::defer(private$program_set_value <- original)
  programs <- test_program_set_programs()
  programs$perspectives <- dsprrr::module(dsprrr::signature(
    "input -> output",
    instructions = "A deliberately changed trajectory test module"
  ))
  private$program_set_value <- tempest_program_set(
    programs = programs,
    path = file.path(withr::local_tempdir(), "changed-program-set")
  )

  expect_error(
    tempest_trajectory_review(session),
    class = "tempest_trajectory_review_error"
  )
})

test_that("trajectory collections cap retained records and digest omissions", {
  items <- lapply(seq_len(251L), function(index) {
    list(
      record_type = "claim",
      record_id = sprintf("claim-%04d", index)
    )
  })
  first <- tempest:::tempest_trajectory_collection(
    items,
    preserve_order = FALSE
  )
  reversed <- tempest:::tempest_trajectory_collection(
    rev(items),
    preserve_order = FALSE
  )
  changed <- items
  changed[[251L]]$record_id <- "claim-9999"
  second <- tempest:::tempest_trajectory_collection(
    changed,
    preserve_order = FALSE
  )

  expect_identical(
    first[c("total", "retained", "omitted")],
    list(total = 251L, retained = 250L, omitted = 1L)
  )
  expect_identical(first, reversed)
  expect_identical(first$items, second$items)
  expect_identical(identical(first$digest, second$digest), FALSE)
})

test_that("trajectory ordered collections preserve authoritative prefixes", {
  items <- lapply(seq_len(251L), function(index) {
    list(
      record_type = "claim",
      record_id = sprintf("claim-%04d", 252L - index)
    )
  })
  first <- tempest:::tempest_trajectory_collection(
    items,
    preserve_order = TRUE
  )
  changed <- items
  changed[[251L]]$record_id <- "claim-9999"
  second <- tempest:::tempest_trajectory_collection(
    changed,
    preserve_order = TRUE
  )

  expect_identical(first$items, unname(items[seq_len(250L)]))
  expect_identical(first$items, second$items)
  expect_identical(identical(first$digest, second$digest), FALSE)
})

test_that("trajectory findings use fixed complete per-stage counts", {
  records <- test_promotion_fixture("storm")$research$state$stage_records
  counts <- tempest:::tempest_trajectory_stage_finding_counts(records)
  codes <- names(tempest:::tempest_trajectory_finding_severities())[1:6]

  expect_named(counts, tempest:::tempest_program_set_stages())
  expect_identical(
    lapply(counts, names),
    rep(list(codes), length(counts)) |>
      stats::setNames(names(counts))
  )
  expect_all_true(unlist(counts, use.names = FALSE) >= 0L)
})

test_that("trajectory promotion lanes rebind proposals and acceptance", {
  fixture <- test_promotion_bundle("storm")
  bundle_before <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  proposed <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle
  )

  expect_identical(proposed@knowledge$promotion_state, "proposed")
  expect_identical(
    proposed@knowledge$proposal$bundle_id,
    fixture$bundle@bundle_id
  )
  expect_null(proposed@knowledge$acceptance)

  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(store, fixture$bundle, plan, result)
  receipt_before <- tempest:::tempest_promotion_receipt_data(receipt)
  accepted <- tempest_trajectory_review(
    fixture$research,
    promotion_bundle = fixture$bundle,
    promotion_receipt = receipt
  )

  expect_identical(accepted@knowledge$promotion_state, "accepted")
  expect_identical(
    accepted@knowledge$acceptance$receipt_id,
    receipt@receipt_id
  )
  expect_identical(
    accepted@knowledge$acceptance$record_revisions$total,
    as.integer(length(receipt@record_revisions))
  )
  accepted_joins <- Filter(
    \(join) identical(join$relation, "accepted_as"),
    accepted@joins$items
  )
  expect_gt(length(accepted_joins), 0L)
  expect_identical(
    tempest:::tempest_promotion_bundle_data(fixture$bundle),
    bundle_before
  )
  expect_identical(
    tempest:::tempest_promotion_receipt_data(receipt),
    receipt_before
  )
})

test_that("trajectory review rejects loose products and receipt-only input", {
  fixture <- test_promotion_bundle("storm")
  loose <- list(
    manifest = fixture$manifest,
    workspace = fixture$workspace,
    stage_records = fixture$stage_records
  )

  expect_error(
    tempest_trajectory_review(loose),
    class = "tempest_trajectory_review_error"
  )

  store <- test_promotion_store()
  withr::defer(graft::graft_close(store))
  plan <- tempest_graft_plan(store, fixture$bundle)
  result <- graft::graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(store, fixture$bundle, plan, result)

  expect_error(
    tempest_trajectory_review(
      fixture$research,
      promotion_receipt = receipt
    ),
    class = "tempest_trajectory_review_error"
  )

  other <- test_promotion_fixture("costorm")$research
  expect_error(
    tempest_trajectory_review(
      other,
      promotion_bundle = fixture$bundle
    ),
    class = "tempest_trajectory_review_error"
  )
})
