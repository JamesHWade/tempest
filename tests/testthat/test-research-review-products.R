test_that("STORM publishes canonical claim review and bounded repair evidence", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  repair_query <- "Compare the measured outcome with a control group."
  repair_url <- "https://example.org/review-repair"
  repair_source_id <- tempest_source_id(repair_url)
  repair_quote <- "The controlled comparison found no measurable change."
  repair_text <- paste(
    "An independent controlled study was conducted.",
    repair_quote
  )
  initial_source_id <- tempest_source_id("https://example.org/progress")
  fixture <- research_review_storm_fixture(
    reviewer_response = list(
      status = "potential_issue",
      finding = "The claim makes a causal statement without a comparison.",
      rationale = "The captured source describes the topic but no comparison.",
      evidence = list(list(
        source_id = initial_source_id,
        quote = "Progress uses staged events and persisted artifacts."
      )),
      query = repair_query
    ),
    adjudicator_response = list(
      verdict = "confirmed",
      rationale = "The independent evidence confirms the comparison is null.",
      evidence = list(list(
        source_id = repair_source_id,
        quote = repair_quote
      ))
    ),
    repair_evidence = TRUE,
    max_review_claims = 1L
  )
  search_queries <- character()
  search_budgets <- integer()
  fetch_calls <- 0L
  local_mocked_bindings(
    tempest_wiki_search = function(query, limit = 8L) {
      tibble::tibble(
        title = character(),
        url = character(),
        snippet = character()
      )
    },
    tempest_retriever_search = function(retriever, query, k) {
      if (!identical(query, repair_query)) {
        return(tibble::tibble(
          title = character(),
          url = character(),
          snippet = character(),
          source_id = character()
        ))
      }
      search_queries <<- c(search_queries, query)
      search_budgets <<- c(search_budgets, k)
      tibble::tibble(
        title = "Independent repair source",
        url = repair_url,
        snippet = repair_quote,
        source_id = repair_source_id
      )
    },
    tempest_fetch_url_text = function(url, user_agent = NULL) {
      fetch_calls <<- fetch_calls + 1L
      expect_identical(url, repair_url)
      list(
        text = repair_text,
        title = "Independent repair source",
        kind = "html"
      )
    }
  )

  support_before <- NULL
  progress <- function(event) {
    if (
      identical(event$event_type, "step") &&
        identical(event$stage, "research") &&
        identical(event$step, "scientific_review") &&
        identical(event$status, "started")
    ) {
      support_before <<- tempest:::tempest_claim_supports_resolved(
        fixture$store
      )
    }
    invisible(event)
  }
  result <- research_review_run_storm(
    fixture,
    run_id = "storm-scientific-review",
    progress = progress
  )
  records <- tempest_claim_reviews(result)
  report <- tempest_report(result)
  claims <- tempest_claims(result)

  expect_identical(result@status, "succeeded")
  expect_gt(length(claims), 0L)
  expect_length(
    records,
    min(result@config@max_review_claims, length(claims))
  )
  expect_identical(
    records,
    lapply(
      result@workspace$list_claim_reviews(),
      tempest:::tempest_claim_review_record
    )
  )
  record <- records[[1L]]
  expect_identical(record$repair$status, "completed")
  expect_identical(record$repair$query, repair_query)
  expect_identical(record$repair$source_ids, repair_source_id)
  expect_identical(record$adjudication$verdict, "confirmed")
  expect_identical(
    record$adjudication$rationale,
    "The independent evidence confirms the comparison is null."
  )
  expect_identical(
    record$adjudication$evidence,
    list(list(source_id = repair_source_id, quote = repair_quote))
  )
  expect_identical(
    result@workspace$get_retrieved_resource(repair_source_id)@content,
    repair_text
  )
  expect_identical(search_queries, repair_query)
  expect_identical(search_budgets, 3L)
  expect_identical(fetch_calls, 1L)
  expect_identical(fixture$review_calls$reviewer, 1L)
  expect_identical(fixture$review_calls$adjudicator, 1L)
  expect_length(fixture$review_calls$reviewer_prompts, 1L)
  expect_length(fixture$review_calls$adjudicator_prompts, 1L)
  expect_match(
    fixture$review_calls$reviewer_prompts[[1L]],
    initial_source_id,
    fixed = TRUE
  )
  expect_match(
    fixture$review_calls$adjudicator_prompts[[1L]],
    repair_source_id,
    fixed = TRUE
  )
  expect_identical(tempest_claim_supports(result), support_before)

  appendix <- tempest:::tempest_claim_review_markdown(result@workspace)
  expect_identical(grepl(appendix, report, fixed = TRUE), TRUE)
  markdown_rationale <- tempest:::tempest_markdown_escape_plain_text(
    record$adjudication$rationale,
    "review text"
  )
  expect_identical(grepl(markdown_rationale, report, fixed = TRUE), TRUE)
  expect_identical(
    length(regmatches(
      report,
      gregexpr("(?m)^## Scientific review$", report, perl = TRUE)
    )[[1L]]),
    1L
  )

  tampered_report <- sub(
    markdown_rationale,
    "Tampered scientific-review rationale.",
    report,
    fixed = TRUE
  )
  expect_identical(identical(report, tampered_report), FALSE)
  expect_identical(
    grepl(markdown_rationale, tampered_report, fixed = TRUE),
    FALSE
  )
  validate_report <- function(report_md, workspace = result@workspace) {
    tempest:::tempest_final_report_validate(
      report_md = report_md,
      workspace = workspace,
      title = result@title,
      citation_policy = result@config@citation_policy,
      on_unsupported_claim = result@config@on_unsupported_claim,
      min_support_score = result@config@min_support_score,
      stage_records = result@state$stage_records
    )
  }
  expect_error(
    validate_report(tampered_report),
    class = "tempest_product_report_error",
    regexp = "scientific review"
  )
  expect_error(
    validate_report(report, tempest_research_workspace()),
    class = "tempest_product_report_error",
    regexp = "without retained records"
  )
})

test_that("Co-STORM retains exact scientific review findings after resume", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  fixture <- research_review_costorm_fixture(
    reviewer_response = list(
      status = "no_issue",
      finding = "",
      rationale = "No concrete scientific issue appears in this dossier.",
      evidence = list(),
      query = ""
    ),
    adjudicator_response = list(
      verdict = "no_issue",
      rationale = "The independent review found no concrete issue.",
      evidence = list()
    ),
    session_id = "costorm-scientific-review"
  )
  support_before <- tempest_claim_supports(fixture$session)
  report <- fixture$session$publish()
  records <- tempest_claim_reviews(fixture$session)
  appendix <- tempest:::tempest_claim_review_markdown(fixture$store)

  expect_identical(
    tempest:::tempest_session_manifest(fixture$session)@status,
    "succeeded"
  )
  expect_length(records, 1L)
  expect_identical(
    records,
    lapply(
      fixture$store$list_claim_reviews(),
      tempest:::tempest_claim_review_record
    )
  )
  expect_identical(records[[1L]]$reviewer$status, "no_issue")
  expect_identical(records[[1L]]$adjudication$verdict, "no_issue")
  expect_identical(grepl(appendix, report, fixed = TRUE), TRUE)
  expect_identical(tempest_claim_supports(fixture$session), support_before)
  expect_identical(fixture$review_calls$reviewer, 1L)
  expect_identical(fixture$review_calls$adjudicator, 1L)

  bundle <- file.path(withr::local_tempdir(), "reviewed-session")
  tempest_session_save(fixture$session, bundle)
  saved <- tempest:::tempest_product_read_json(file.path(
    bundle,
    "session.json"
  ))
  expect_identical(saved$schema_version, 14L)
  resumed <- tempest_session_resume(bundle, config = fixture$config)

  expect_identical(tempest_claim_reviews(resumed), records)
  expect_identical(tempest_report(resumed), report)
  expect_identical(fixture$review_calls$reviewer, 1L)
  expect_identical(fixture$review_calls$adjudicator, 1L)
})

test_that("disabled scientific review makes no reviewer or adjudicator calls", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  storm <- research_review_storm_fixture(
    reviewer_response = list(),
    adjudicator_response = list(),
    research_review = FALSE
  )
  storm_result <- research_review_run_storm(
    storm,
    run_id = "storm-review-disabled"
  )
  costorm <- research_review_costorm_fixture(
    reviewer_response = list(),
    adjudicator_response = list(),
    research_review = FALSE,
    session_id = "costorm-review-disabled"
  )
  costorm_report <- costorm$session$publish()

  expect_length(tempest_claim_reviews(storm_result), 0L)
  expect_length(tempest_claim_reviews(costorm$session), 0L)
  expect_identical(
    grepl(
      "(?m)^## Scientific review$",
      tempest_report(storm_result),
      perl = TRUE
    ),
    FALSE
  )
  expect_identical(
    grepl("(?m)^## Scientific review$", costorm_report, perl = TRUE),
    FALSE
  )
  expect_identical(storm$review_calls$reviewer, 0L)
  expect_identical(storm$review_calls$adjudicator, 0L)
  expect_identical(costorm$review_calls$reviewer, 0L)
  expect_identical(costorm$review_calls$adjudicator, 0L)
})

test_that("Co-STORM cancellation during review publishes no report", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  fixture <- research_review_costorm_fixture(
    reviewer_response = list(
      status = "no_issue",
      finding = "",
      rationale = "No concrete scientific issue appears in this dossier.",
      evidence = list(),
      query = ""
    ),
    adjudicator_response = list(
      verdict = "no_issue",
      rationale = "The independent review found no concrete issue.",
      evidence = list()
    ),
    session_id = "costorm-review-cancelled"
  )
  current <- TRUE
  request <- tempest:::tempest_session_report_async(
    fixture$session,
    is_current = function() current
  )
  current <- FALSE
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_null(settled$value)
  expect_identical(
    tempest:::tempest_session_manifest(fixture$session)@status,
    "running"
  )
  expect_null(tempest:::tempest_session_report_value(fixture$session))
  expect_length(
    tempest:::tempest_session_workspace(fixture$session)$list_claim_reviews(),
    0L
  )
  expect_identical(fixture$review_calls$adjudicator, 0L)
})

test_that("STORM cancellation before review leaves the report unpublished", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  fixture <- research_review_storm_fixture(
    reviewer_response = list(),
    adjudicator_response = list(),
    research_review = TRUE
  )
  output_dir <- withr::local_tempdir()
  collector <- tempest_progress_collector(include_payload = TRUE)
  progress <- function(event) {
    collector$record(event)
    if (
      identical(event$event_type, "step") &&
        identical(event$stage, "research") &&
        identical(event$step, "scientific_review") &&
        identical(event$status, "started")
    ) {
      stop(structure(
        list(message = "Cancel before scientific review."),
        class = c("interrupt", "condition")
      ))
    }
    invisible(event)
  }
  condition <- tryCatch(
    tempest_run(
      "Progress events",
      config = fixture$config,
      retriever = fixture$retriever,
      n_experts = 1L,
      max_questions_per_perspective = 1L,
      output_dir = output_dir,
      run_id = "storm-review-cancelled",
      progress = progress,
      verbose = FALSE
    ),
    interrupt = identity
  )
  manifest <- tempest:::tempest_product_read_json(file.path(
    output_dir,
    "storm-review-cancelled",
    "run_config.json"
  ))

  expect_s3_class(condition, "interrupt")
  expect_identical(tempest_progress_state(collector$data())$status, "cancelled")
  expect_null(manifest$research_manifest$deliverables$report_md)
  expect_identical(
    file.exists(file.path(output_dir, "storm-review-cancelled", "report.md")),
    FALSE
  )
  expect_length(fixture$store$list_claim_reviews(), 0L)
  expect_identical(fixture$review_calls$reviewer, 0L)
})
