# tests/testthat/helper-mocks.R
# Reusable fakes so ledger/verification logic is testable without network or API keys.

# A fake ellmer-style chat. $chat_structured() pops from a scripted queue of
# return values; $chat() returns scripted plain text. Records calls for asserts.
fake_chat <- function(structured = list(), text = list()) {
  state <- new.env(parent = emptyenv())
  state$structured <- structured
  state$text <- text
  state$calls <- list()
  list(
    chat_structured = function(prompt, type = NULL, ...) {
      state$calls <- c(
        state$calls,
        list(list(kind = "structured", prompt = prompt))
      )
      if (length(state$structured) == 0) {
        stop("fake_chat: structured queue exhausted")
      }
      out <- state$structured[[1]]
      state$structured <- state$structured[-1]
      out
    },
    chat = function(prompt, ...) {
      state$calls <- c(state$calls, list(list(kind = "text", prompt = prompt)))
      if (length(state$text) == 0) {
        return("")
      }
      out <- state$text[[1]]
      state$text <- state$text[-1]
      out
    },
    register_tools = function(...) invisible(NULL),
    .calls = function() state$calls
  )
}

# A fake judge that returns a fixed verification verdict for every claim.
fake_judge <- function(
  status = "supported",
  score = 0.9,
  rationale = "matches source"
) {
  fake_chat(structured = list()) # placeholder; verdict queue set per-test via fake_verdicts()
}

# Build a verdict queue (one per claim-span pair) for verification tests.
fake_verdicts <- function(...) {
  verdicts <- list(...)
  fake_chat(structured = verdicts)
}

# Fixture builders.
fake_source <- function(
  url = "https://example.org/a",
  title = "Example A",
  content_text = "Photosynthesis converts light to chemical energy."
) {
  tempest_source(
    url = url,
    title = title,
    content_text = content_text,
    fetched_at = "2026-01-01T00:00:00Z"
  )
}

fake_store_with_sources <- function(n = 2) {
  store <- test_research_workspace()
  for (i in seq_len(n)) {
    store$upsert_retrieved_resource(fake_source(
      url = paste0("https://example.org/", i),
      title = paste("Example", i),
      content_text = paste("Body text for source", i)
    ))
  }
  store
}

fake_stage_inputs <- function(stage) {
  switch(
    stage,
    perspectives = list(
      topic = "Topic",
      seed_context = "Seed context",
      n_experts = 1L
    ),
    personas = list(
      topic = "Topic",
      n_experts = 1L,
      requirements = "Distinct experts"
    ),
    query_decomposition = list(question = "Question", topic = "Topic"),
    extract_claims = list(
      answer_text = "Source-backed answer",
      source_context = "",
      source_ids = "",
      citation_mode = "tempest_inline"
    ),
    verify_claim_support = list(
      claim_text = "A supported claim",
      source_excerpts = "Source excerpt"
    ),
    next_question = list(
      topic = "Topic",
      perspective = "Perspective",
      answered = "",
      facts = ""
    ),
    draft_outline = list(topic = "Topic", report_title = "Report"),
    refined_outline = list(
      topic = "Topic",
      report_title = "Outline",
      draft_outline = "Draft outline",
      facts = "Verified facts"
    ),
    section_writing = list(
      section_title = "Evidence",
      section_summary = "",
      subsections = "",
      facts = "Verified facts"
    ),
    lead_section = list(
      topic = "Topic",
      title = "Report",
      article_body = "",
      facts = "Verified facts"
    )
  )
}

fake_verify_claim_supports <- function(
  workspace,
  claims,
  min_support_score = 0.7
) {
  requested <- stats::setNames(
    claims,
    vapply(claims, \(claim) claim@claim_id, character(1))
  )
  for (claim in workspace$list_proposed_claims()) {
    represented <- vapply(
      workspace$get_evidence_for_proposed_claim(claim@claim_id),
      \(span) span@source_id,
      character(1)
    )
    for (source_id in setdiff(claim@source_ids, represented)) {
      source <- workspace$get_retrieved_source(source_id)
      span_id <- workspace$add_evidence_span(tempest_evidence_span(
        source_id = source_id,
        quote = source$content_text,
        extracted_by = "test::extractor"
      ))
      workspace$link_evidence_to_proposed_claim(claim@claim_id, span_id)
    }
  }
  supports <- unlist(
    lapply(
      workspace$list_proposed_claims(),
      function(claim) {
        requested_claim <- requested[[claim@claim_id]] %||% claim
        status <- requested_claim@verification_status
        score <- requested_claim@support_score
        if (identical(status, "unverified")) {
          status <- "unverifiable"
          score <- NA_real_
        }
        lapply(
          workspace$get_evidence_for_proposed_claim(claim@claim_id),
          \(span) {
            tempest_claim_support(
              claim_id = claim@claim_id,
              evidence_span_id = span@evidence_span_id,
              source_id = span@source_id,
              verification_status = status,
              support_score = score,
              rationale = "Supported by exact source evidence."
            )
          }
        )
      }
    ),
    recursive = FALSE
  )
  workspace$verify_proposed_claims_batch(
    supports,
    verified_at = "2026-08-16T12:03:00Z",
    min_support_score = min_support_score,
    verifier = "test::verifier"
  )
  lapply(names(requested), workspace$get_proposed_claim)
}

test_program_executions <- function(
  config = tempest_config(),
  run_id = "test-program-execution"
) {
  program_set <- tempest_program_set()
  manifest <- tempest_research_manifest(
    run_id,
    mode = "storm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set)
  )
  tempest:::tempest_bind_program_set(program_set, manifest)
}

test_program_set_from_program <- function(
  program,
  registry = list(),
  .local_envir = parent.frame()
) {
  stages <- tempest:::tempest_program_set_stages()
  programs <- stats::setNames(rep(list(program), length(stages)), stages)
  root <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    "program-set"
  )
  tempest_program_set(
    programs = programs,
    path = root,
    registry = registry
  )
}

test_program_reference <- function(
  stage,
  program_artifact_id = NULL,
  governed_procedure_ref = NULL
) {
  program_artifact_id <- program_artifact_id %||%
    paste0(
      "sha256:",
      digest::digest(
        paste0("test-program:", stage),
        algo = "sha256",
        serialize = FALSE
      )
    )
  list(
    stage = stage,
    contract_version = 1L,
    program_artifact_id = program_artifact_id,
    artifact_reference = list(
      type = "builtin",
      id = paste0("tempest::", stage)
    ),
    governed_procedure_ref = governed_procedure_ref,
    evaluator_id = paste0("tempest::evaluator/", stage),
    evaluator_version = "1"
  )
}
