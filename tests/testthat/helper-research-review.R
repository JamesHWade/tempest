test_review_workspace <- function(statuses = "unsupported", max_sources = 24L) {
  workspace <- tempest_research_workspace(max_sources = max_sources)
  fixtures <- lapply(seq_along(statuses), function(index) {
    key <- as.character(index)
    source_id <- paste0("source.review.", key)
    span_id <- paste0("span.review.", key)
    claim_id <- paste0("claim.review.", key)
    quote <- paste("Evidence phrase for claim", key, "is exact.")
    resource <- tempest_resource(
      resource_kind = "review.fixture",
      locator = paste0("review-fixture:", key),
      title = paste("Review source", key),
      media_type = "text/plain",
      resource_id = source_id,
      content = paste("Captured text:", quote),
      retrieved_at = "2026-11-14T12:00:00Z"
    )
    workspace$upsert_retrieved_resource(resource)
    span <- tempest_evidence_span(
      source_id = source_id,
      quote = quote,
      evidence_span_id = span_id,
      extracted_by = "review.fixture",
      created_at = "2026-11-14T12:01:00Z"
    )
    claim <- tempest_claim(
      claim_text = paste("Claim", key, "states a scientific result."),
      source_ids = source_id,
      evidence_span_ids = span_id,
      supporting_quotes = list(quote),
      claim_id = claim_id,
      created_at = "2026-11-14T12:02:00Z"
    )
    workspace$add_extracted_claim_batch(list(claim), list(span))
    list(
      claim = workspace$get_proposed_claim(claim_id),
      span = workspace$get_evidence_span(span_id),
      resource = workspace$get_retrieved_resource(source_id),
      quote = quote
    )
  })
  support_status <- unname(statuses)
  supports <- lapply(seq_along(fixtures), function(index) {
    status <- support_status[[index]]
    score <- switch(
      status,
      supported = 0.95,
      partially_supported = 0.5,
      unsupported = 0.1,
      contradicted = 0,
      unverifiable = NA_real_,
      0.5
    )
    tempest_claim_support(
      claim_id = fixtures[[index]]$claim@claim_id,
      evidence_span_id = fixtures[[index]]$span@evidence_span_id,
      source_id = fixtures[[index]]$span@source_id,
      verification_status = status,
      support_score = score,
      rationale = paste("Fixture support status", status)
    )
  })
  workspace$verify_proposed_claims_batch(
    supports,
    verified_at = "2026-11-14T12:03:00Z",
    min_support_score = 0.7,
    verifier = "review.fixture.judge"
  )
  list(workspace = workspace, fixtures = fixtures, supports = supports)
}

test_review_config <- function(
  research_review = TRUE,
  repair_evidence = FALSE,
  max_review_claims = 5L,
  review_concurrency = 2L,
  max_sources = 24L
) {
  tempest_config(
    models = list(judge = "test/review-model"),
    research_review = research_review,
    repair_evidence = repair_evidence,
    max_review_claims = max_review_claims,
    review_concurrency = review_concurrency,
    max_sources = max_sources
  )
}

test_review_retriever <- function(
  search_results = NULL,
  fetched_resources = list()
) {
  retriever <- new.env(parent = emptyenv())
  retriever$workspace <- tempest_research_workspace(max_sources = 24L)
  retriever$search_calls <- list()
  retriever$fetch_calls <- character()
  retriever$search_results <- search_results %||%
    data.frame(
      title = character(),
      url = character(),
      snippet = character(),
      source_id = character()
    )
  retriever$fetched_resources <- fetched_resources
  retriever$search <- function(query, k) {
    retriever$search_calls[[length(retriever$search_calls) + 1L]] <- list(
      query = query,
      k = k
    )
    results <- retriever$search_results
    if (is.function(results)) {
      results <- results(query, k)
    }
    utils::head(results, k)
  }
  retriever$fetch <- function(url) {
    retriever$fetch_calls <- c(retriever$fetch_calls, url)
    resource <- retriever$fetched_resources[[url]] %||% NULL
    if (is.null(resource)) {
      stop("Fixture has no resource for requested URL.", call. = FALSE)
    }
    resource
  }
  retriever
}

test_review_source_record <- function(
  url,
  text = "Fetched evidence text for the review."
) {
  tempest_resource(
    resource_kind = "web",
    locator = url,
    title = "Fetched review source",
    media_type = "text/plain",
    content = text,
    retrieved_at = "2026-11-14T12:10:00Z"
  )
}

test_review_source_id <- function(url) {
  tempest_source_id(url)
}

test_review_dossier <- function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

test_review_default_reviewer <- function(inputs, claim_id) {
  dossier <- test_review_dossier(inputs$dossier)
  if (length(dossier) == 0L) {
    return(list(
      status = "insufficient_evidence",
      finding = "The claim has no captured source text.",
      rationale = "No source dossier was available for this review.",
      evidence = list(),
      query = "Find a primary source that evaluates this claim."
    ))
  }
  source <- dossier[[1L]]
  list(
    status = "potential_issue",
    finding = paste("Potential scope issue for", claim_id),
    rationale = "The claim needs independent scientific scrutiny.",
    evidence = list(list(source_id = source$source_id, quote = source$text)),
    query = ""
  )
}

test_review_default_adjudicator <- function(inputs, claim_id) {
  packet <- jsonlite::fromJSON(inputs$review_packet, simplifyVector = FALSE)
  if (identical(packet$reviewer$status, "insufficient_evidence")) {
    return(list(
      verdict = "unresolved",
      rationale = "The captured sources do not resolve the issue.",
      evidence = list()
    ))
  }
  source <- packet$dossier[[1L]]
  list(
    verdict = "confirmed",
    rationale = paste("The cited evidence supports a concern for", claim_id),
    evidence = list(list(source_id = source$source_id, quote = source$text))
  )
}

test_review_mock_models <- function(
  reviewer = test_review_default_reviewer,
  adjudicator = test_review_default_adjudicator,
  delay_s = 0,
  .local_envir = parent.frame()
) {
  state <- new.env(parent = emptyenv())
  state$chat_calls <- list()
  state$requests <- list()
  state$active_requests <- 0L
  state$max_active_requests <- 0L
  state$reviewer <- reviewer
  state$adjudicator <- adjudicator
  state$delay_s <- delay_s
  testthat::local_mocked_bindings(
    tempest_make_chat = function(config, role, system_prompt, echo) {
      id <- length(state$chat_calls) + 1L
      chat <- list(id = paste0("fake-chat-", id))
      state$chat_calls[[id]] <- list(
        id = chat$id,
        role = role,
        system_prompt = system_prompt,
        echo = echo
      )
      chat
    },
    tempest_research_review_async_call = function(
      module,
      ...,
      chat,
      trace_context
    ) {
      inputs <- list(...)
      role <- trace_context$role
      claim_id <- trace_context$claim_id
      state$requests[[length(state$requests) + 1L]] <- list(
        role = role,
        claim_id = claim_id,
        inputs = inputs,
        chat_id = chat$id
      )
      output <- if (identical(role, "reviewer")) {
        state$reviewer(inputs, claim_id)
      } else {
        state$adjudicator(inputs, claim_id)
      }
      result <- structure(
        list(
          output = output,
          metadata = list(
            program_artifact_id = dsprrr::program_artifact_id(module),
            trace_context = trace_context
          )
        ),
        class = "dsprrr_result"
      )
      if (state$delay_s > 0) {
        state$active_requests <- state$active_requests + 1L
        state$max_active_requests <- max(
          state$max_active_requests,
          state$active_requests
        )
        promises::promise(function(resolve, reject) {
          later::later(
            function() {
              state$active_requests <- state$active_requests - 1L
              resolve(result)
            },
            delay = state$delay_s
          )
        })
      } else {
        promises::promise_resolve(result)
      }
    },
    .env = .local_envir
  )
  state
}

test_review_make_search_results <- function(urls) {
  data.frame(
    title = paste("Repair source", seq_along(urls)),
    url = urls,
    snippet = paste("Captured search result", seq_along(urls)),
    source_id = vapply(urls, test_review_source_id, character(1)),
    stringsAsFactors = FALSE
  )
}
