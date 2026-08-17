test_that("extraction evaluates and commits an exact claim batch", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(
    store$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  output <- list(
    facts = list(list(
      claim = "Exact multi-source claim",
      sources = lapply(source_ids, \(source_id) list(source_id = source_id)),
      confidence = "high",
      support_score = 0.84
    ))
  )
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )

  tempest:::tempest_extract_facts_from_answer(
    chat = NULL,
    answer_text = "Exact multi-source claim",
    store = store,
    module = module
  )

  claims <- store$list_proposed_claims()
  expect_length(claims, 1L)
  expect_equal(claims[[1]]@claim_text, "Exact multi-source claim")
  expect_equal(claims[[1]]@source_ids, source_ids)
  expect_identical(claims[[1]]@support_score, NA_real_)
  expect_identical(claims[[1]]@verification_status, "unverified")
})

test_that("extraction rejects an unknown source without partial insertion", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  output <- list(
    facts = list(
      list(
        claim = "Known claim",
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ),
      list(
        claim = "Unknown claim",
        sources = list(list(source_id = "Sdeadbeef0000")),
        confidence = "medium"
      )
    )
  )
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )

  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "Mixed batch",
      store = store,
      module = module
    ),
    class = "tempest_stage_output_validation_error"
  )
  expect_length(store$list_proposed_claims(), 0L)
})

test_that("extraction rejects coercive and legacy fact shapes", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  output <- NULL
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )
  malformed <- list(
    list(
      facts = list(list(
        claim = 12,
        sources = list(list(source_id = source_id)),
        confidence = "high"
      ))
    ),
    list(
      facts = list(list(
        claim = "Legacy source",
        source_ids = source_id,
        confidence = "high"
      ))
    ),
    list(
      facts = list(list(
        claim = "Vector source",
        sources = list(list(source_id = c(source_id, source_id))),
        confidence = "high"
      ))
    ),
    list(
      facts = list(list(
        claim = "Factor confidence",
        sources = list(list(source_id = source_id)),
        confidence = factor("high")
      ))
    )
  )

  for (value in malformed) {
    output <- value
    expect_error(
      tempest:::tempest_extract_facts_from_answer(
        chat = NULL,
        answer_text = "Malformed",
        store = store,
        module = module
      ),
      class = "tempest_stage_output_validation_error"
    )
  }
  expect_length(store$list_proposed_claims(), 0L)
})

test_that("extraction forwards provider-native source context", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  captured_inputs <- NULL
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(
      module,
      chat,
      inputs,
      step
    ) {
      captured_inputs <<- inputs
      structure(
        list(
          output = list(
            facts = list(list(
              claim = "Native claim",
              sources = list(list(source_id = source$id)),
              confidence = "high"
            ))
          )
        ),
        class = "dsprrr_result"
      )
    }
  )

  tempest:::tempest_extract_facts_from_answer(
    chat = NULL,
    answer_text = "Native claim",
    store = store,
    module = module,
    source_ids = source$id
  )

  expect_equal(captured_inputs$source_ids, source$id)
  expect_equal(captured_inputs$citation_mode, "provider_native")
  expect_match(captured_inputs$source_context, source$id, fixed = TRUE)
})

test_that("extraction is fail-closed on provider and binding errors", {
  store <- fake_store_with_sources(1)
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) stop("provider failed")
  )

  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "No claim",
      store = store,
      module = module
    ),
    class = "tempest_stage_execution_error"
  )
  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "No claim",
      store = store,
      module = NULL
    ),
    class = "tempest_ecosystem_contract_error"
  )
  expect_length(store$list_proposed_claims(), 0L)
})

test_that("extraction preserves validated quote-to-source lineage", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  output <- list(
    facts = list(list(
      claim = "The source contains body text.",
      sources = list(list(
        source_id = source$id,
        url = source$url,
        quote = "Body text for source 1"
      )),
      confidence = "high",
      support_score = 0.9
    ))
  )
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )

  tempest:::tempest_extract_facts_from_answer(
    chat = NULL,
    answer_text = "The source contains body text.",
    store = store,
    module = module
  )

  claims <- store$list_proposed_claims()
  spans <- store$list_evidence_spans()
  expect_length(claims, 1L)
  expect_length(spans, 1L)
  expect_identical(claims[[1]]@evidence_span_ids, spans[[1]]@evidence_span_id)
  expect_identical(claims[[1]]@supporting_quotes, list(spans[[1]]@quote))
  expect_identical(spans[[1]]@source_id, source$id)
  expect_identical(spans[[1]]@quote, "Body text for source 1")
  expect_identical(spans[[1]]@start_offset, NA_integer_)
  expect_identical(spans[[1]]@page, NA_integer_)
  expect_identical(spans[[1]]@extracted_by, module$program_artifact_id)
})

test_that("extraction digest binds exact spans and execution provenance", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  output <- list(
    facts = list(list(
      claim = "The source contains body text.",
      sources = list(list(
        source_id = source$id,
        quote = "Body text for source 1"
      )),
      confidence = "high"
    ))
  )
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(
      stage = "extract_claims",
      research_run_id = "run-extraction"
    )
  )
  records <- list()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )

  tempest:::tempest_extract_facts_from_answer(
    chat = NULL,
    answer_text = "The source contains body text.",
    store = store,
    module = module,
    session_id = "run-extraction",
    expert_id = "expert-1",
    retrieval_step_id = "retrieval-1",
    record_stage = function(record, output = NULL) {
      records <<- tempest:::tempest_stage_records_upsert(records, record)
    }
  )

  claims <- store$list_proposed_claims()
  spans <- store$list_evidence_spans()
  terminal <- records[[1]]
  digest <- tempest:::tempest_stage_claims_output_digest(
    claims,
    terminal,
    spans
  )
  expect_identical(terminal@output_reference$content_digest, digest)
  expect_identical(terminal@trace_references$research_run_id, "run-extraction")
  expect_identical(terminal@trace_references$expert_id, "expert-1")
  expect_identical(terminal@trace_references$correlation_id, "retrieval-1")
  expect_identical(claims[[1]]@session_id, "run-extraction")
  expect_identical(claims[[1]]@expert_id, "expert-1")
  expect_identical(claims[[1]]@retrieval_step_id, "retrieval-1")

  verified_claims <- list(S7::set_props(
    claims[[1]],
    verification_status = "supported",
    support_score = 0.95,
    verifier_model = "verifier-1",
    verified_at = "2026-08-16T01:00:00Z"
  ))
  expect_identical(
    tempest:::tempest_stage_claims_output_digest(
      verified_claims,
      terminal,
      spans
    ),
    digest
  )
  changed_claim <- list(S7::set_props(
    verified_claims[[1]],
    claim_text = "Tampered extraction-owned claim text"
  ))
  expect_identical(
    identical(
      tempest:::tempest_stage_claims_output_digest(
        changed_claim,
        terminal,
        spans
      ),
      digest
    ),
    FALSE
  )

  changed_quote <- list(S7::set_props(
    spans[[1]],
    quote = "source contains body"
  ))
  expect_identical(
    identical(
      digest,
      tempest:::tempest_stage_claims_output_digest(
        claims,
        terminal,
        changed_quote
      )
    ),
    FALSE
  )
  wrong_extractor <- list(S7::set_props(
    spans[[1]],
    extracted_by = paste0("sha256:", strrep("f", 64L))
  ))
  expect_error(
    tempest:::tempest_stage_claims_output_digest(
      claims,
      terminal,
      wrong_extractor
    ),
    class = "tempest_stage_evaluator_contract_error"
  )
})

test_that("empty extraction output has one exact canonical digest", {
  record <- tempest:::tempest_stage_record_start(
    "extract_claims",
    paste0("sha256:", strrep("a", 64L)),
    attempt_id = "empty-extraction-digest"
  )

  digest <- tempest:::tempest_stage_claims_output_digest(
    list(),
    record,
    evidence_spans = list()
  )

  expect_match(digest, "^sha256:[a-f0-9]{64}$")
  expect_identical(
    digest,
    tempest:::tempest_stage_claims_output_digest(
      list(),
      record,
      evidence_spans = list()
    )
  )
})

test_that("extraction rejects forged claim provenance before provider use", {
  store <- fake_store_with_sources(1)
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(
      stage = "extract_claims",
      research_run_id = "run-authoritative"
    )
  )
  provider_calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      provider_calls <<- provider_calls + 1L
      structure(list(output = list(facts = list())), class = "dsprrr_result")
    }
  )

  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "Forged provenance",
      store = store,
      module = module,
      session_id = "run-forged"
    ),
    class = "tempest_stage_governance_error"
  )
  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "Credential provenance",
      store = store,
      module = module,
      expert_id = "sk-live-secret"
    ),
    class = "tempest_stage_governance_error"
  )
  expect_identical(provider_calls, 0L)
})

test_that("extraction binds an exact Deputy delegation tuple", {
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(
      stage = "extract_claims",
      research_run_id = "run-delegated"
    )
  )
  binding <- tempest:::tempest_extract_claims_execution_bind(
    module = module,
    session_id = "run-delegated",
    expert_id = "expert-delegated",
    retrieval_step_id = "correlation-delegated",
    perspective_id = NA_character_,
    section_id = NA_character_,
    deputy_run_id = "deputy-child-run",
    deputy_session_id = "deputy-child-session",
    parent_run_id = "deputy-parent-run",
    delegation_id = "delegation-exact",
    tool_call_id = "tool-call-exact"
  )

  expect_identical(
    binding$deputy_execution,
    list(
      deputy_run_id = "deputy-child-run",
      deputy_session_id = "deputy-child-session",
      parent_run_id = "deputy-parent-run",
      delegation_id = "delegation-exact",
      tool_call_id = "tool-call-exact"
    )
  )
  expect_error(
    tempest:::tempest_extract_claims_execution_bind(
      module = module,
      session_id = "run-delegated",
      expert_id = "expert-delegated",
      retrieval_step_id = "correlation-delegated",
      perspective_id = NA_character_,
      section_id = NA_character_,
      deputy_run_id = "deputy-child-run",
      deputy_session_id = "deputy-child-session",
      parent_run_id = "deputy-parent-run"
    ),
    class = "tempest_stage_governance_error"
  )
  expect_error(
    tempest:::tempest_extract_claims_execution_bind(
      module = module,
      session_id = "run-delegated",
      expert_id = "expert-delegated",
      retrieval_step_id = "correlation-delegated",
      perspective_id = NA_character_,
      section_id = NA_character_,
      deputy_run_id = "deputy-child-run",
      deputy_session_id = "deputy-child-session",
      parent_run_id = "deputy-child-run",
      delegation_id = "delegation-self-parent",
      tool_call_id = "tool-self-parent"
    ),
    class = "tempest_stage_governance_error"
  )
})

test_that("extraction rejects duplicate, mismatched, and fabricated sources", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  valid_source <- list(
    source_id = source$id,
    url = source$url,
    quote = "Body text for source 1"
  )
  output <- NULL
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )
  invalid_sources <- list(
    list(valid_source, valid_source),
    list(valid_source, valid_source[c("source_id", "quote")]),
    list(utils::modifyList(
      valid_source,
      list(url = "https://example.org/tampered")
    )),
    list(utils::modifyList(
      valid_source,
      list(quote = "fabricated quote")
    ))
  )

  for (sources in invalid_sources) {
    output <- list(
      facts = list(list(
        claim = "Invalid lineage claim",
        sources = sources,
        confidence = "high"
      ))
    )
    expect_error(
      tempest:::tempest_extract_facts_from_answer(
        chat = NULL,
        answer_text = "Invalid lineage claim",
        store = store,
        module = module
      ),
      class = "tempest_stage_output_validation_error"
    )
  }
  expect_length(store$list_proposed_claims(), 0L)
  expect_length(store$list_evidence_spans(), 0L)
})

test_that("terminal-record failure rolls back quote spans and claims", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  output <- list(
    facts = list(list(
      claim = "Rollback quote claim",
      sources = list(list(
        source_id = source$id,
        quote = "Body text for source 1"
      )),
      confidence = "high"
    ))
  )
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(list(output = output), class = "dsprrr_result")
    }
  )

  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "Rollback quote claim",
      store = store,
      module = module,
      record_stage = function(record, output = NULL) {
        if (!is.null(output)) {
          rlang::abort("record commit failed", class = "test_commit_error")
        }
        invisible(record)
      }
    ),
    class = "tempest_stage_commit_error"
  )
  expect_length(store$list_proposed_claims(), 0L)
  expect_length(store$list_evidence_spans(), 0L)
})
