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

test_that("trajectory review data exposes the validated closed projection", {
  fixture <- test_promotion_fixture("storm")
  review <- tempest_trajectory_review(fixture$research)
  expected <- S7::props(review)

  expect_identical(tempest_trajectory_review_data(review), expected)
  expect_identical(
    tempest_trajectory_review_data(unserialize(serialize(review, NULL))),
    expected
  )
  ReviewLookalike <- S7::new_class(
    "TempestTrajectoryReviewLookalike",
    properties = list(
      schema_version = S7::class_integer,
      review_id = S7::class_character,
      product = S7::class_list,
      stages = S7::class_list,
      agent_runs = S7::class_list,
      programs = S7::class_list,
      knowledge = S7::class_list,
      evidence = S7::class_list,
      joins = S7::class_list,
      findings = S7::class_list
    )
  )
  lookalike <- do.call(ReviewLookalike, expected)
  condition <- rlang::catch_cnd(tempest_trajectory_review_data(lookalike))
  expect_s3_class(condition, "tempest_trajectory_review_error")
  expect_s3_class(condition, "tempest_input_error")
  expect_snapshot(error = TRUE, tempest_trajectory_review_data(lookalike))
})

test_that("trajectory review validation rechecks canonical collection invariants", {
  fixture <- test_promotion_fixture("storm")
  review <- tempest_trajectory_review(fixture$research)
  properties <- S7::props(review)
  finding <- list(
    code = "support_unverified",
    severity = "warning",
    ref_type = "stage_attempt",
    ref_id = "attempt-duplicate"
  )

  duplicate <- properties
  duplicate$findings <- tempest_trajectory_collection(
    list(finding, finding),
    preserve_order = FALSE
  )
  payload <- do.call(
    tempest_trajectory_review_payload,
    duplicate[setdiff(names(duplicate), "review_id")]
  )
  duplicate$review_id <- tempest_trajectory_digest(payload)
  expect_snapshot(
    error = TRUE,
    do.call(TempestTrajectoryReview, duplicate)
  )

  object_count <- properties
  object_count$findings$total <- factor(0L)
  expect_snapshot(
    error = TRUE,
    do.call(TempestTrajectoryReview, object_count)
  )
})

test_that("trajectory review validation owns nested source invariants", {
  review <- tempest_trajectory_review(test_promotion_fixture("storm")$research)
  properties <- S7::props(review)
  for (lane in c("stages", "agent_runs", "evidence", "joins", "findings")) {
    expect_gt(length(properties[[lane]]$items), 0L)
  }
  mutations <- list(
    product_mode = function(value) {
      value$product["mode"] <- list(NULL)
      value
    },
    stage_artifact = function(value) {
      value$stages$items[[1L]]$program_artifact_id <- "not-a-digest"
      value
    },
    agent_status = function(value) {
      value$agent_runs$items[[1L]]$status <- "unknown"
      value
    },
    agent_trace_id = function(value) {
      value$agent_runs$items[[1L]]$trace_id <- "another-run"
      value
    },
    agent_lineage = function(value) {
      value$agent_runs$items[[1L]]$parent_run_id <- "partial-parent"
      value
    },
    agent_context = function(value) {
      value$agent_runs$items[[1L]]$role <- "moderator"
      value
    },
    program_evaluator = function(value) {
      value$programs[[1L]]["evaluator_id"] <- list(NULL)
      value
    },
    evidence_type = function(value) {
      value$evidence$items[[1L]]["record_type"] <- list(NULL)
      value
    },
    join_proof = function(value) {
      value$joins$items[[1L]]$proof["kind"] <- list(NULL)
      value
    },
    finding_code = function(value) {
      value$findings$items[[1L]]["code"] <- list(NULL)
      value
    }
  )
  for (mutate in mutations) {
    malformed <- mutate(unserialize(serialize(properties, NULL)))
    payload <- do.call(
      tempest_trajectory_review_payload,
      malformed[setdiff(names(malformed), "review_id")]
    )
    malformed$review_id <- tempest_trajectory_digest(payload)
    expect_error(do.call(TempestTrajectoryReview, malformed))
  }
})

test_that("trajectory review revalidates ordered stage and Deputy identities", {
  review <- tempest_trajectory_review(test_promotion_fixture("storm")$research)
  properties <- S7::props(review)
  rebuild <- function(value) {
    payload <- do.call(
      tempest_trajectory_review_payload,
      value[setdiff(names(value), "review_id")]
    )
    value$review_id <- tempest_trajectory_digest(payload)
    do.call(TempestTrajectoryReview, value)
  }

  reversed <- unserialize(serialize(properties, NULL))
  reversed$stages <- tempest_trajectory_collection(
    rev(reversed$stages$items),
    preserve_order = TRUE
  )
  expect_error(rebuild(reversed))

  incomplete <- unserialize(serialize(properties, NULL))
  incomplete$stages$items[[1L]]["completed_at"] <- list(NULL)
  incomplete$stages <- tempest_trajectory_collection(
    incomplete$stages$items,
    preserve_order = TRUE
  )
  expect_error(rebuild(incomplete))

  wrong_stage_trace <- unserialize(serialize(properties, NULL))
  wrong_stage_trace$stages$items[[1L]]$trace_id <- "another-stage-trace"
  wrong_stage_trace$stages <- tempest_trajectory_collection(
    wrong_stage_trace$stages$items,
    preserve_order = TRUE
  )
  expect_error(rebuild(wrong_stage_trace))

  wrong_trust <- unserialize(serialize(properties, NULL))
  wrong_trust$stages$items[[1L]]$publication_allowed <-
    !wrong_trust$stages$items[[1L]]$publication_allowed
  wrong_trust$stages <- tempest_trajectory_collection(
    wrong_trust$stages$items,
    preserve_order = TRUE
  )
  expect_error(rebuild(wrong_trust))

  wrong_session <- unserialize(serialize(properties, NULL))
  wrong_session$agent_runs$items[[1L]]$deputy_session_id <- "other-session"
  wrong_session$agent_runs <- tempest_trajectory_collection(
    wrong_session$agent_runs$items,
    preserve_order = FALSE
  )
  expect_error(rebuild(wrong_session))

  wrong_join <- unserialize(serialize(properties, NULL))
  wrong_join$joins$items[[1L]] <- list(
    from_type = "product",
    from_id = wrong_join$product$research_run_id,
    relation = "contains",
    to_type = "stage_attempt",
    to_id = "missing-stage",
    proof = list(
      kind = "authority_validated",
      matched_fields = list("research_run_id", "attempt_id")
    )
  )
  wrong_join$joins <- tempest_trajectory_collection(
    wrong_join$joins$items,
    preserve_order = FALSE
  )
  expect_error(rebuild(wrong_join))

  wrong_join_type <- unserialize(serialize(properties, NULL))
  wrong_join_type$joins$items[[1L]]$to_type <- "source"
  wrong_join_type$joins <- tempest_trajectory_collection(
    wrong_join_type$joins$items,
    preserve_order = FALSE
  )
  expect_error(rebuild(wrong_join_type))
})

test_that("trajectory review accepts the exact public tempest_run outline", {
  result <- storm_product_baseline_fixture()$result
  public_bullets <- result@outline$sections[[1L]]$subsections[[1L]]$bullets
  state_bullets <- result@state$outline$sections[[1L]]$subsections[[1L]]$bullets
  expected_attempts <- vapply(
    result@state$stage_records,
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
  records <- test_promotion_fixture("storm")$research@state$stage_records
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

  malformed <- S7::props(accepted)
  revision <- malformed$knowledge$acceptance$record_revisions$items[[1L]]
  malformed$knowledge$acceptance$record_revisions <-
    tempest_trajectory_collection(
      list(revision, revision),
      preserve_order = FALSE
    )
  payload <- do.call(
    tempest_trajectory_review_payload,
    malformed[setdiff(names(malformed), "review_id")]
  )
  malformed$review_id <- tempest_trajectory_digest(payload)
  expect_snapshot(
    error = TRUE,
    do.call(TempestTrajectoryReview, malformed)
  )

  missing_class <- S7::props(accepted)
  classes <- tempest_promotion_receipt_classes()
  for (action in c("inserted", "updated", "matched", "observed")) {
    missing_class$knowledge$acceptance$counts[[action]] <-
      missing_class$knowledge$acceptance$counts[[action]][-1L]
  }
  expect_false(
    classes[[1L]] %in%
      names(missing_class$knowledge$acceptance$counts$observed)
  )
  payload <- do.call(
    tempest_trajectory_review_payload,
    missing_class[setdiff(names(missing_class), "review_id")]
  )
  missing_class$review_id <- tempest_trajectory_digest(payload)
  expect_error(do.call(TempestTrajectoryReview, missing_class))

  empty_selection <- S7::props(accepted)
  empty_selection$knowledge$proposal$claim_selection$count <- 0L
  payload <- do.call(
    tempest_trajectory_review_payload,
    empty_selection[setdiff(names(empty_selection), "review_id")]
  )
  empty_selection$review_id <- tempest_trajectory_digest(payload)
  expect_error(do.call(TempestTrajectoryReview, empty_selection))

  wrong_receipt <- S7::props(accepted)
  wrong_receipt$knowledge$acceptance$receipt_id <- paste0(
    "sha256:",
    strrep("f", 64L)
  )
  payload <- do.call(
    tempest_trajectory_review_payload,
    wrong_receipt[setdiff(names(wrong_receipt), "review_id")]
  )
  wrong_receipt$review_id <- tempest_trajectory_digest(payload)
  expect_error(do.call(TempestTrajectoryReview, wrong_receipt))

  receipt_mutations <- list(
    plan = function(value) {
      value$knowledge$acceptance$plan_id <- "not-a-plan"
      value$knowledge$acceptance$batch_id <- "not-a-plan"
      value$knowledge$acceptance$snapshot$batch_id <- "not-a-plan"
      value
    },
    store = function(value) {
      value$knowledge$acceptance$store_id <- "not-a-store"
      value$knowledge$acceptance$snapshot$store_id <- "not-a-store"
      value
    },
    snapshot_schema = function(value) {
      value$knowledge$acceptance$snapshot$schema_version <- 2L
      value
    },
    store_format = function(value) {
      value$knowledge$acceptance$snapshot$store_format_version <- "2.0.0"
      value
    },
    commit_order = function(value) {
      value$knowledge$acceptance$snapshot$commit_order <- 0L
      value
    },
    committed_at = function(value) {
      value$knowledge$acceptance$snapshot$committed_at <- "not-a-time"
      value
    }
  )
  for (mutate in receipt_mutations) {
    malformed <- mutate(S7::props(accepted))
    acceptance <- malformed$knowledge$acceptance
    receipt_payload <- tempest_promotion_receipt_payload(
      acceptance$bundle_id,
      acceptance$plan_id,
      acceptance$plan_digest,
      acceptance$batch_id,
      acceptance$store_id,
      acceptance$schema_build_digest,
      acceptance$snapshot,
      acceptance$counts,
      acceptance$record_revisions$items
    )
    malformed$knowledge$acceptance$receipt_id <-
      tempest_promotion_digest(receipt_payload)
    payload <- do.call(
      tempest_trajectory_review_payload,
      malformed[setdiff(names(malformed), "review_id")]
    )
    malformed$review_id <- tempest_trajectory_digest(payload)
    expect_error(do.call(TempestTrajectoryReview, malformed))
  }
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
