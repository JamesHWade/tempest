test_that("claim review prioritizes weak claims and preserves review authority", {
  fixture <- test_review_workspace(c(
    "supported",
    "partially_supported",
    "unsupported",
    "contradicted"
  ))
  support_before <- fixture$workspace$list_claim_supports()
  statuses_before <- vapply(
    fixture$workspace$list_proposed_claims(),
    function(claim) claim@verification_status,
    character(1)
  )
  model <- test_review_mock_models()
  result <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config(
        max_review_claims = 3L,
        review_concurrency = 1L
      )
    )
  )
  records <- lapply(result, tempest_claim_review_record)
  reviewed <- vapply(
    records,
    `[[`,
    character(1),
    "claim_id"
  )
  reviewer_order <- vapply(
    Filter(
      function(request) identical(request$role, "reviewer"),
      model$requests
    ),
    `[[`,
    character(1),
    "claim_id"
  )

  expect_identical(
    reviewer_order,
    c("claim.review.4", "claim.review.3", "claim.review.2")
  )
  expect_setequal(reviewed, reviewer_order)
  expect_length(result, 3L)
  expect_length(model$chat_calls, 6L)
  expect_length(unique(vapply(model$chat_calls, `[[`, character(1), "id")), 6L)
  expect_length(intersect("claim.review.1", reviewed), 0L)
  expect_setequal(fixture$workspace$list_claim_supports(), support_before)
  expect_identical(
    vapply(
      fixture$workspace$list_proposed_claims(),
      function(claim) claim@verification_status,
      character(1)
    ),
    statuses_before
  )
  expect_identical(
    records[[1]]$programs$reviewer,
    dsprrr::program_artifact_id(
      tempest:::tempest_research_review_reviewer_module()
    )
  )
  expect_identical(records[[1]]$models$reviewer, "test/review-model")
  expect_identical(records[[1]]$models$adjudicator, "test/review-model")
  expect_identical(
    identical(
      records[[1]]$programs$reviewer,
      records[[1]]$programs$adjudicator
    ),
    FALSE
  )
})

test_that("review and adjudication retain confirmed, dismissed, and unresolved outcomes", {
  fixture <- test_review_workspace(rep("unsupported", 3L))
  reviewer <- function(inputs, claim_id) {
    if (identical(claim_id, "claim.review.3")) {
      return(list(
        status = "insufficient_evidence",
        finding = "The captured excerpt cannot resolve the question.",
        rationale = "No source in the dossier reports the required comparison.",
        evidence = list(),
        query = "Find a primary source reporting the comparison."
      ))
    }
    test_review_default_reviewer(inputs, claim_id)
  }
  adjudicator <- function(inputs, claim_id) {
    packet <- jsonlite::fromJSON(inputs$review_packet, simplifyVector = FALSE)
    if (identical(claim_id, "claim.review.2")) {
      source <- packet$dossier[[1L]]
      return(list(
        verdict = "dismissed",
        rationale = "The cited passage does not establish the proposed concern.",
        evidence = list(list(source_id = source$source_id, quote = source$text))
      ))
    }
    test_review_default_adjudicator(inputs, claim_id)
  }
  test_review_mock_models(reviewer, adjudicator)
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config(review_concurrency = 1L)
    )
  )
  records <- stats::setNames(
    lapply(records, tempest_claim_review_record),
    vapply(
      records,
      function(review) {
        tempest_claim_review_record(review)$claim_id
      },
      character(1)
    )
  )

  expect_identical(
    records[["claim.review.1"]]$adjudication$verdict,
    "confirmed"
  )
  expect_identical(
    records[["claim.review.2"]]$adjudication$verdict,
    "dismissed"
  )
  expect_identical(
    records[["claim.review.3"]]$reviewer$status,
    "insufficient_evidence"
  )
  expect_identical(
    records[["claim.review.3"]]$adjudication$verdict,
    "unresolved"
  )
  expect_length(records[["claim.review.3"]]$reviewer$evidence, 0L)
})

test_that("claims without captured source text receive an insufficient review", {
  workspace <- tempest_research_workspace()
  workspace$add_proposed_claim(tempest_claim(
    claim_text = "A claim without captured sources.",
    claim_id = "claim.review.no-source",
    created_at = "2026-11-14T12:02:00Z"
  ))
  test_review_mock_models()
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      workspace,
      test_review_retriever(),
      test_review_config()
    )
  )
  record <- tempest_claim_review_record(records[[1L]])

  expect_length(records, 1L)
  expect_length(record$dossier, 0L)
  expect_identical(record$reviewer$status, "insufficient_evidence")
  expect_identical(record$adjudication$verdict, "unresolved")
})

test_that("a no-issue outcome means no defect was found, not that the claim is true", {
  fixture <- test_review_workspace()
  reviewer <- function(inputs, claim_id) {
    list(
      status = "no_issue",
      finding = "",
      rationale = "The bounded dossier did not reveal a concrete flaw.",
      evidence = list(),
      query = ""
    )
  }
  adjudicator <- function(inputs, claim_id) {
    list(
      verdict = "no_issue",
      rationale = "No specific issue was established; claim truth is not assessed.",
      evidence = list()
    )
  }
  test_review_mock_models(reviewer, adjudicator)
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config()
    )
  )
  record <- tempest_claim_review_record(records[[1L]])

  expect_identical(record$reviewer$status, "no_issue")
  expect_identical(record$adjudication$verdict, "no_issue")
  expect_match(record$adjudication$rationale, "truth is not assessed")
})

test_that("one repair search fetches at most two sources and commits atomically", {
  fixture <- test_review_workspace()
  urls <- paste0("https://example.org/repair/", 1:3)
  resources <- lapply(urls, test_review_source_record)
  names(resources) <- urls
  retriever <- test_review_retriever(
    search_results = test_review_make_search_results(urls),
    fetched_resources = resources
  )
  reviewer <- function(inputs, claim_id) {
    output <- test_review_default_reviewer(inputs, claim_id)
    output$query <- "Find a primary study evaluating this result."
    output
  }
  adjudicator <- function(inputs, claim_id) {
    packet <- jsonlite::fromJSON(inputs$review_packet, simplifyVector = FALSE)
    repair_source <- tail(packet$dossier, 1L)[[1L]]
    list(
      verdict = "dismissed",
      rationale = "The additional source does not establish the candidate flaw.",
      evidence = list(list(
        source_id = repair_source$source_id,
        quote = repair_source$text
      ))
    )
  }
  support_before <- fixture$workspace$list_claim_supports()
  model <- test_review_mock_models(reviewer, adjudicator)
  config <- test_review_config(repair_evidence = TRUE)
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      retriever,
      config
    )
  )
  record <- tempest_claim_review_record(records[[1L]])

  expect_length(retriever$search_calls, 1L)
  expect_identical(retriever$search_calls[[1L]]$k, 3L)
  expect_length(retriever$fetch_calls, 2L)
  expect_identical(record$repair$status, "budget_exhausted")
  expect_length(record$repair$source_ids, 2L)
  expect_length(record$dossier, 3L)
  expect_length(
    intersect(
      record$adjudication$evidence[[1L]]$source_id,
      record$repair$source_ids
    ),
    1L
  )
  expect_length(fixture$workspace$list_retrieved_resources(), 3L)
  expect_setequal(fixture$workspace$list_claim_supports(), support_before)
  reviewer_input <- test_review_dossier(model$requests[[1L]]$inputs$dossier)
  adjudicator_packet <- jsonlite::fromJSON(
    model$requests[[2L]]$inputs$review_packet,
    simplifyVector = FALSE
  )
  expect_length(reviewer_input, 1L)
  expect_length(adjudicator_packet$dossier, 3L)

  request_count <- length(model$requests)
  chat_count <- length(model$chat_calls)
  repeated <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      retriever,
      config
    )
  )
  expect_identical(
    tempest_claim_review_record(repeated[[1L]])$review_id,
    record$review_id
  )
  expect_identical(length(model$requests), request_count)
  expect_identical(length(model$chat_calls), chat_count)
  expect_length(retriever$search_calls, 1L)
})

test_that("a finite workspace repair budget keeps excess claims reviewable", {
  fixture <- test_review_workspace(
    statuses = c("unsupported", "unsupported"),
    max_sources = 3L
  )
  urls <- c(
    "https://example.org/repair/claim-1",
    "https://example.org/repair/claim-2"
  )
  resources <- lapply(urls, test_review_source_record)
  names(resources) <- urls
  retriever <- test_review_retriever(
    search_results = function(query, k) {
      index <- if (grepl("claim.review.1", query, fixed = TRUE)) 1L else 2L
      test_review_make_search_results(urls[[index]])
    },
    fetched_resources = resources
  )
  reviewer <- function(inputs, claim_id) {
    output <- test_review_default_reviewer(inputs, claim_id)
    output$query <- paste("Find a primary source for", claim_id)
    output
  }
  adjudicator <- function(inputs, claim_id) {
    test_review_default_adjudicator(inputs, claim_id)
  }
  test_review_mock_models(reviewer, adjudicator, delay_s = 0.001)
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      retriever,
      test_review_config(
        repair_evidence = TRUE,
        review_concurrency = 2L
      )
    )
  )
  by_claim <- stats::setNames(
    lapply(records, tempest_claim_review_record),
    vapply(
      records,
      function(review) {
        tempest_claim_review_record(review)$claim_id
      },
      character(1)
    )
  )

  expect_identical(by_claim[["claim.review.1"]]$repair$status, "completed")
  expect_identical(
    by_claim[["claim.review.2"]]$repair$status,
    "budget_exhausted"
  )
  expect_length(by_claim[["claim.review.1"]]$repair$source_ids, 1L)
  expect_length(by_claim[["claim.review.2"]]$repair$source_ids, 0L)
  expect_length(retriever$fetch_calls, 1L)
  expect_length(records, 2L)
})

test_that("clean empty repair results are recorded without fetched sources", {
  fixture <- test_review_workspace()
  retriever <- test_review_retriever()
  reviewer <- function(inputs, claim_id) {
    output <- test_review_default_reviewer(inputs, claim_id)
    output$query <- "Find a source for this claim."
    output
  }
  test_review_mock_models(reviewer)
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      retriever,
      test_review_config(repair_evidence = TRUE)
    )
  )
  record <- tempest_claim_review_record(records[[1L]])

  expect_identical(record$repair$status, "no_results")
  expect_identical(record$repair$query, "Find a source for this claim.")
  expect_length(record$repair$source_ids, 0L)
  expect_length(retriever$search_calls, 1L)
  expect_length(retriever$fetch_calls, 0L)
})

test_that("a custom fetch cannot invalidate verified workspace support", {
  fixture <- test_review_workspace()
  url <- "https://example.org/repair/mutation"
  resource <- test_review_source_record(url)
  retriever <- test_review_retriever(
    search_results = test_review_make_search_results(url),
    fetched_resources = setNames(list(resource), url)
  )
  retriever$fetch <- function(url) {
    fixture$workspace$upsert_retrieved_resource(resource)
    resource
  }
  reviewer <- function(inputs, claim_id) {
    output <- test_review_default_reviewer(inputs, claim_id)
    output$query <- "Find a primary source for this claim."
    output
  }
  test_review_mock_models(reviewer)
  support_before <- fixture$workspace$list_claim_supports()
  claims_before <- fixture$workspace$list_proposed_claims()

  request <- tempest:::tempest_research_review_async(
    fixture$workspace,
    retriever,
    test_review_config(repair_evidence = TRUE)
  )
  expect_error(
    tempest:::tempest_research_review_await(request),
    class = "tempest_research_review_retrieval_error"
  )

  expect_setequal(fixture$workspace$list_claim_supports(), support_before)
  expect_identical(fixture$workspace$list_proposed_claims(), claims_before)
  expect_length(fixture$workspace$list_retrieved_resources(), 1L)
  expect_length(fixture$workspace$list_claim_reviews(), 0L)
})

test_that("unknown citations and malformed model fields fail without commit", {
  for (reviewer in list(
    function(inputs, claim_id) {
      list(
        status = "potential_issue",
        finding = "Potential flaw.",
        rationale = "A source should support this concern.",
        evidence = list(list(source_id = "source.unknown", quote = "invented")),
        query = ""
      )
    },
    function(inputs, claim_id) {
      list(
        status = "potential_issue",
        finding = "Potential flaw.",
        rationale = "A source should support this concern.",
        evidence = list(),
        query = "",
        unexpected = "extra"
      )
    }
  )) {
    fixture <- test_review_workspace()
    test_review_mock_models(reviewer)
    request <- tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config()
    )
    expect_error(
      tempest:::tempest_research_review_await(request),
      class = "tempest_research_review_error"
    )
    expect_length(fixture$workspace$list_claim_reviews(), 0L)
  }
})

test_that("async cancellation after reviewer output prevents repair and commit", {
  fixture <- test_review_workspace()
  retriever <- test_review_retriever()
  current <- TRUE
  reviewer <- function(inputs, claim_id) {
    current <<- FALSE
    test_review_default_reviewer(inputs, claim_id)
  }
  test_review_mock_models(reviewer)
  request <- tempest:::tempest_research_review_async(
    fixture$workspace,
    retriever,
    test_review_config(repair_evidence = TRUE),
    is_current = function() current
  )

  expect_error(
    tempest:::tempest_research_review_await(request),
    class = "tempest_research_review_cancelled"
  )
  expect_length(retriever$search_calls, 0L)
  expect_length(fixture$workspace$list_claim_reviews(), 0L)
})

test_that("aggregate failure cancels a still-pending sibling review", {
  fixture <- test_review_workspace(rep("unsupported", 2L))
  retriever <- test_review_retriever()
  reviewer <- function(inputs, claim_id) {
    if (identical(claim_id, "claim.review.1")) {
      stop("simulated reviewer failure", call. = FALSE)
    }
    test_review_default_reviewer(inputs, claim_id)
  }
  model <- test_review_mock_models(reviewer, delay_s = 0.05)
  request <- tempest:::tempest_research_review_async(
    fixture$workspace,
    retriever,
    test_review_config(
      repair_evidence = TRUE,
      review_concurrency = 2L
    )
  )

  expect_error(tempest:::tempest_research_review_await(request))
  later::run_now(0.1)

  expect_length(model$requests, 2L)
  expect_length(retriever$search_calls, 0L)
  expect_length(retriever$fetch_calls, 0L)
  expect_length(fixture$workspace$list_claim_reviews(), 0L)
})

test_that("sync timeout revokes the current token before a late provider response", {
  fixture <- test_review_workspace()
  test_review_mock_models(delay_s = 0.05)
  withr::local_options(tempest.research_review_sync_timeout_s = 0.005)

  expect_error(
    tempest:::tempest_research_review(
      fixture$workspace,
      test_review_retriever(),
      test_review_config()
    ),
    class = "tempest_research_review_error"
  )
  later::run_now(0.1)

  expect_length(fixture$workspace$list_claim_reviews(), 0L)
})

test_that("async reviews respect the configured claim concurrency", {
  fixture <- test_review_workspace(rep("unsupported", 4L))
  model <- test_review_mock_models(delay_s = 0.002)
  records <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config(
        max_review_claims = 4L,
        review_concurrency = 2L
      )
    )
  )
  first_wave_claims <- unique(vapply(
    head(model$requests, 4L),
    `[[`,
    character(1),
    "claim_id"
  ))

  expect_length(records, 4L)
  expect_lte(model$max_active_requests, 2L)
  expect_length(first_wave_claims, 2L)
})

test_that("bounded dossiers keep exact linked quotes inside long source text", {
  workspace <- tempest_research_workspace()
  quote <- "Required evidence span beyond the first twenty thousand characters."
  text <- paste0(strrep("background ", 2500L), quote, strrep(" tail", 2500L))
  resource <- tempest_resource(
    resource_kind = "review.fixture",
    locator = "review-fixture:long",
    title = "Long review source",
    media_type = "text/plain",
    resource_id = "source.review.long",
    content = text,
    retrieved_at = "2026-11-14T12:00:00Z"
  )
  workspace$upsert_retrieved_resource(resource)
  span <- tempest_evidence_span(
    source_id = resource@resource_id,
    quote = quote,
    evidence_span_id = "span.review.long",
    extracted_by = "review.fixture",
    created_at = "2026-11-14T12:01:00Z"
  )
  claim <- tempest_claim(
    claim_text = "The long source supports a result.",
    source_ids = resource@resource_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(quote),
    claim_id = "claim.review.long",
    created_at = "2026-11-14T12:02:00Z"
  )
  workspace$add_extracted_claim_batch(list(claim), list(span))
  dossier <- tempest:::tempest_research_review_claim_dossier(
    workspace,
    workspace$get_proposed_claim(claim@claim_id)
  )
  excerpt <- dossier[[1L]]$text

  expect_lte(nchar(excerpt), 20000L)
  expect_match(excerpt, quote, fixed = TRUE)
  expect_match(text, excerpt, fixed = TRUE)
})

test_that("disabled review returns existing records without model or retrieval calls", {
  fixture <- test_review_workspace()
  model <- test_review_mock_models()
  active <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config()
    )
  )
  requests <- length(model$requests)
  chats <- length(model$chat_calls)
  disabled <- tempest:::tempest_research_review_await(
    tempest:::tempest_research_review_async(
      fixture$workspace,
      test_review_retriever(),
      test_review_config(research_review = FALSE)
    )
  )

  expect_identical(
    vapply(
      disabled,
      function(review) {
        tempest_claim_review_record(review)$review_id
      },
      character(1)
    ),
    vapply(
      active,
      function(review) {
        tempest_claim_review_record(review)$review_id
      },
      character(1)
    )
  )
  expect_identical(length(model$requests), requests)
  expect_identical(length(model$chat_calls), chats)
})
