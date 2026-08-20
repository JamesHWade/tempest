test_that("stage policies are exact and derived by stage", {
  policies <- lapply(
    tempest:::tempest_program_set_stages(),
    tempest:::tempest_stage_policy
  )

  expect_identical(
    vapply(policies, \(policy) policy$fallback_policy, character(1)),
    c(
      "exploratory_allowed",
      "exploratory_allowed",
      "exploratory_allowed",
      "fail_closed",
      "fail_closed",
      "exploratory_allowed",
      "exploratory_allowed",
      "grounded_only",
      "grounded_only",
      "grounded_only"
    )
  )
  expect_identical(
    vapply(
      policies,
      \(policy) policy$requires_verified_evidence,
      logical(1)
    ),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    "TempestStageRecord" %in% getNamespaceExports("tempest"),
    FALSE
  )
})

test_that("ProgramSet accepts only exact builtin evaluator pairs", {
  evaluators <- tempest:::tempest_program_set_default_evaluators()
  evaluators$perspectives$evaluator_version <- "999"
  expect_error(
    tempest:::tempest_program_set_evaluators(evaluators),
    class = "tempest_program_set_evaluator_error"
  )

  program_set <- tempest_program_set()
  entries <- tempest:::tempest_program_set_entries(program_set)
  entries$personas$evaluator_id <- "custom::evaluator/personas"
  expect_error(
    tempest:::tempest_program_set_validate_entries(entries),
    class = "tempest_program_set_evaluator_error"
  )

  execution <- tempest:::tempest_program_set_execution(
    program_set,
    "next_question"
  )
  expect_named(
    execution,
    c(
      "program",
      "program_artifact_id",
      "trace_context",
      "stage",
      "contract_version",
      "evaluator_id",
      "evaluator_version",
      "governed_procedure_ref"
    )
  )
})

test_that("stage records enforce lifecycle and derive publication", {
  artifact_id <- paste0("sha256:", strrep("a", 64L))
  running <- tempest:::tempest_stage_record_start(
    "verify_claim_support",
    artifact_id,
    trace_references = list(
      research_run_id = "run-1",
      min_support_score = "0.7",
      verified_at = "2026-08-16T00:59:00Z"
    ),
    attempt_id = "attempt-1",
    started_at = "2026-08-16T01:00:00Z"
  )

  expect_s7_class(running, tempest:::TempestStageRecord)
  expect_identical(running@status, "running")
  expect_identical(running@fallback_policy, "fail_closed")
  expect_identical(running@execution_path, "grounded")
  expect_identical(running@trace_references$stage_attempt_id, "attempt-1")
  expect_identical(running@trace_references$trace_id, "attempt-1")
  expect_identical(running@publication_allowed, FALSE)

  succeeded <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "claim_supports",
      paste0("sha256:", strrep("2", 64L)),
      content_digest = paste0("sha256:", strrep("1", 64L))
    ),
    support_status = "verified",
    completed_at = "2026-08-16T01:00:01Z"
  )
  expect_identical(succeeded@status, "succeeded")
  expect_identical(succeeded@publication_allowed, TRUE)
  expect_identical(succeeded@fallback_taken, FALSE)
  expect_identical(is.na(succeeded@failure_message), TRUE)

  expect_error(
    tempest:::tempest_stage_record_succeed(
      succeeded,
      tempest:::tempest_stage_output_reference(
        "claim_supports",
        paste0("sha256:", strrep("2", 64L)),
        content_digest = paste0("sha256:", strrep("1", 64L))
      ),
      support_status = "verified"
    ),
    class = "tempest_stage_lifecycle_error"
  )
})

test_that("stage records enforce closed per-stage support semantics", {
  artifact_id <- paste0("sha256:", strrep("a", 64L))
  extraction <- tempest:::tempest_stage_record_start(
    "extract_claims",
    artifact_id,
    attempt_id = "extract-support-forge",
    started_at = "2026-08-16T01:00:00Z"
  )
  expect_error(
    tempest:::tempest_stage_record_succeed(
      extraction,
      tempest:::tempest_stage_output_reference(
        "workspace_claims",
        "claim-1",
        content_digest = paste0("sha256:", strrep("1", 64L))
      ),
      support_status = "verified",
      completed_at = "2026-08-16T01:00:01Z"
    ),
    class = "tempest_stage_record_error"
  )

  writing <- tempest:::tempest_stage_record_start(
    "section_writing",
    artifact_id,
    attempt_id = "writing-support-forge",
    started_at = "2026-08-16T01:00:00Z"
  )
  expect_error(
    tempest:::tempest_stage_record_succeed(
      writing,
      tempest:::tempest_stage_content_reference("Bound section"),
      support_status = "unknown",
      completed_at = "2026-08-16T01:00:01Z"
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("fallback success preserves only controlled failure state", {
  artifact_id <- paste0("sha256:", strrep("b", 64L))
  running <- tempest:::tempest_stage_record_start(
    "personas",
    artifact_id,
    attempt_id = "fallback-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )
  provider_error <- simpleError(
    "secret provider response with sk-do-not-persist"
  )
  succeeded <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = paste0("sha256:", strrep("9", 64L))
    ),
    support_status = "unknown",
    fallback_taken = TRUE,
    primary_error = provider_error,
    completed_at = "2026-08-16T01:00:01Z"
  )

  expect_identical(
    succeeded@failure_class,
    "tempest_stage_execution_error"
  )
  expect_identical(
    succeeded@failure_message,
    "Primary stage execution failed."
  )
  expect_identical(
    succeeded@fallback_implementation,
    "tempest::fallback/personas/ellmer-structured@1"
  )
  expect_identical(succeeded@fallback_taken, TRUE)
  expect_identical(succeeded@execution_path, "exploratory")
  expect_identical(succeeded@publication_allowed, FALSE)
  expect_no_match(
    jsonlite::toJSON(tempest:::tempest_stage_record_data(succeeded)),
    "sk-do-not-persist",
    fixed = TRUE
  )
})

test_that("fail-closed stages cannot record fallback", {
  artifact_id <- paste0("sha256:", strrep("c", 64L))
  running <- tempest:::tempest_stage_record_start(
    "extract_claims",
    artifact_id,
    attempt_id = "closed-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )

  expect_error(
    tempest:::tempest_stage_record_fail(
      running,
      fallback_taken = TRUE,
      kind = "fallback",
      completed_at = "2026-08-16T01:00:01Z"
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("stage record codecs are exact and reject trust tampering", {
  artifact_id <- paste0("sha256:", strrep("d", 64L))
  record <- tempest:::tempest_stage_record_start(
    "draft_outline",
    artifact_id,
    attempt_id = "codec-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )
  data <- tempest:::tempest_stage_record_data(record)

  expect_named(data, tempest:::tempest_stage_record_fields())
  expect_null(data$output_reference)
  expect_null(data$completed_at)
  expect_identical(tempest:::tempest_stage_record_from_data(data), record)
  expect_error(
    tempest:::tempest_stage_record_from_data(data[rev(names(data))]),
    class = "tempest_stage_record_error"
  )

  tampered <- data
  tampered$publication_allowed <- TRUE
  expect_error(
    tempest:::tempest_stage_record_from_data(tampered),
    class = "tempest_stage_record_error"
  )

  raw_failure <- data
  raw_failure$status <- "failed"
  raw_failure$completed_at <- "2026-08-16T01:00:01Z"
  raw_failure$failure_class <- "simpleError"
  raw_failure$failure_message <- "provider secret"
  expect_error(
    tempest:::tempest_stage_record_from_data(raw_failure),
    class = "tempest_stage_record_error"
  )

  expect_error(
    S7::set_props(record, program_artifact_id = "sha256:not-a-digest"),
    class = "simpleError"
  )
  expect_error(
    S7::set_props(record, fallback_taken = c(FALSE, TRUE)),
    class = "simpleError"
  )

  for (field in c(
    "governed_procedure_revision_id",
    "completed_at",
    "failure_class",
    "failure_message",
    "fallback_implementation"
  )) {
    for (malformed in list(
      NA_character_,
      character(),
      list(),
      structure(list(), names = character())
    )) {
      tampered <- data
      tampered[field] <- list(malformed)
      expect_error(
        tempest:::tempest_stage_record_from_data(tampered),
        class = "tempest_stage_record_error",
        info = paste("nullable field", field)
      )
    }
  }

  tampered <- data
  tampered["output_reference"] <- list(list())
  expect_error(
    tempest:::tempest_stage_record_from_data(tampered),
    class = "tempest_stage_record_error"
  )
})

test_that("stage timestamps require anchored canonical UTC values", {
  artifact_id <- paste0("sha256:", strrep("d", 64L))
  expect_no_error(tempest:::tempest_stage_record_start(
    "draft_outline",
    artifact_id,
    started_at = "2026-08-16T01:02:03.123456Z"
  ))
  for (malformed in c(
    "2026-08-16 01:02:03 PST",
    "2026-08-16T01:02:03Zjunk",
    "2026-08-16",
    "2026-02-30T01:02:03Z",
    "2026-08-16T01:02:03.1234567Z"
  )) {
    expect_error(
      tempest:::tempest_stage_record_start(
        "draft_outline",
        artifact_id,
        started_at = malformed
      ),
      class = "tempest_stage_record_error",
      info = malformed
    )
  }
})

test_that("governed stage records fail closed without procedure proof", {
  expect_error(
    tempest:::tempest_stage_record(
      stage = "perspectives",
      attempt_id = "governed-attempt",
      status = "running",
      program_artifact_id = paste0("sha256:", strrep("d", 64L)),
      governed_procedure_revision_id = "revision-1",
      trace_references = list(),
      started_at = "2026-08-16T01:00:00Z",
      fallback_policy = "exploratory_allowed",
      execution_path = "governed",
      support_status = "unknown"
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("output references and trace references are exact", {
  expect_error(
    tempest:::tempest_stage_output_reference(
      "workspace_claims",
      c("claim-1", "claim-1")
    ),
    class = "tempest_stage_record_error"
  )
  expect_error(
    tempest:::tempest_stage_output_reference(
      "workspace_claims",
      "sk-live-secret",
      content_digest = paste0("sha256:", strrep("1", 64L))
    ),
    class = "tempest_stage_record_error"
  )
  expect_error(
    tempest:::tempest_stage_record_start(
      "query_decomposition",
      paste0("sha256:", strrep("d", 64L)),
      trace_references = list(unexpected = "value")
    ),
    class = "tempest_stage_record_error"
  )
  expect_error(
    tempest:::tempest_stage_record_start(
      "query_decomposition",
      paste0("sha256:", strrep("d", 64L)),
      trace_references = list(trace_id = list(nested = "value"))
    ),
    class = "tempest_stage_record_error"
  )
  expect_error(
    tempest:::tempest_stage_record_start(
      "query_decomposition",
      paste0("sha256:", strrep("d", 64L)),
      trace_references = list(trace_id = "different-trace"),
      attempt_id = "canonical-attempt-trace"
    ),
    class = "tempest_stage_record_error"
  )
  for (malicious in c(
    "Authorization:Bearer-sk-live-secret",
    "sk-live-secret",
    "BearerCredential",
    "api_key_live_secret",
    "eyJhbGciOiJIUzI1NiJ9.payload.signature"
  )) {
    expect_error(
      tempest:::tempest_stage_record_start(
        "query_decomposition",
        paste0("sha256:", strrep("d", 64L)),
        trace_references = list(trace_id = malicious)
      ),
      class = "tempest_stage_record_error",
      info = malicious
    )
  }
  expect_error(
    tempest:::tempest_stage_record_start(
      "section_writing",
      paste0("sha256:", strrep("d", 64L)),
      trace_references = list(
        evidence_claim_ids = list("Authorization:Bearer-sk-live-secret")
      )
    ),
    class = "tempest_stage_record_error"
  )

  running <- tempest:::tempest_stage_record_start(
    "section_writing",
    paste0("sha256:", strrep("d", 64L)),
    trace_references = list(evidence_claim_ids = list("claim-1")),
    attempt_id = "exact-array-shape"
  )
  succeeded <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_content_reference("Exact output"),
    support_status = "verified"
  )
  scalar_reference <- succeeded@output_reference
  scalar_reference$ids <- scalar_reference$ids[[1]]
  expect_error(
    S7::set_props(succeeded, output_reference = scalar_reference),
    class = "simpleError"
  )
  scalar_trace <- running@trace_references
  scalar_trace$evidence_claim_ids <- scalar_trace$evidence_claim_ids[[1]]
  expect_error(
    S7::set_props(running, trace_references = scalar_trace),
    class = "simpleError"
  )

  data <- tempest:::tempest_stage_record_data(succeeded)
  scalar_ids <- data
  scalar_ids$output_reference$ids <- scalar_ids$output_reference$ids[[1]]
  expect_error(
    tempest:::tempest_stage_record_from_data(scalar_ids),
    class = "tempest_stage_record_error"
  )
  null_ids <- data
  null_ids$output_reference$ids <- NULL
  expect_error(
    tempest:::tempest_stage_record_from_data(null_ids),
    class = "tempest_stage_record_error"
  )
  scalar_trace_data <- tempest:::tempest_stage_record_data(running)
  scalar_trace_data$trace_references$evidence_claim_ids <- "claim-1"
  expect_error(
    tempest:::tempest_stage_record_from_data(scalar_trace_data),
    class = "tempest_stage_record_error"
  )
})

test_that("succeeded records enforce the closed output-reference mapping", {
  running <- tempest:::tempest_stage_record_start(
    "section_writing",
    paste0("sha256:", strrep("d", 64L)),
    attempt_id = "reference-map-attempt"
  )

  expect_error(
    tempest:::tempest_stage_record_succeed(
      running,
      tempest:::tempest_stage_output_reference("state_field", "section"),
      support_status = "unknown"
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("stage record collections permit one immutable transition", {
  artifact_id <- paste0("sha256:", strrep("e", 64L))
  running <- tempest:::tempest_stage_record_start(
    "section_writing",
    artifact_id,
    attempt_id = "collection-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )
  records <- tempest:::tempest_stage_records_upsert(list(), running)
  succeeded <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_content_reference("Section text"),
    support_status = "verified",
    completed_at = "2026-08-16T01:00:01Z"
  )
  records <- tempest:::tempest_stage_records_upsert(records, succeeded)

  expect_identical(records[[1]], succeeded)
  expect_identical(
    tempest:::tempest_stage_records_from_data(
      tempest:::tempest_stage_records_data(records),
      allow_running = FALSE
    ),
    records
  )
  expect_error(
    tempest:::tempest_stage_records_upsert(
      records,
      tempest:::tempest_stage_record_fail(
        running,
        completed_at = "2026-08-16T01:00:02Z"
      )
    ),
    class = "tempest_stage_lifecycle_error"
  )
})

test_that("running records project to durable cancellation", {
  artifact_id <- paste0("sha256:", strrep("f", 64L))
  running <- tempest:::tempest_stage_record_start(
    "perspectives",
    artifact_id,
    attempt_id = "interrupt-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )

  expect_error(
    tempest:::tempest_stage_records_validate(
      list(running),
      allow_running = FALSE
    ),
    class = "tempest_stage_record_error"
  )
  interrupted <- tempest:::tempest_stage_records_interrupt(
    list(running),
    completed_at = "2026-08-16T01:00:01Z"
  )
  expect_identical(interrupted[[1]]@status, "cancelled")
  expect_identical(
    interrupted[[1]]@failure_message,
    "Stage execution was cancelled."
  )
  expect_no_error(
    tempest:::tempest_stage_records_validate(
      interrupted,
      allow_running = FALSE
    )
  )
})

test_that("stage records bind to exact manifest programs and run trace", {
  program_set <- tempest_program_set()
  programs <- tempest:::tempest_program_set_manifest_programs(program_set)
  manifest <- tempest_research_manifest(
    research_run_id = "record-manifest-run",
    mode = "storm",
    config = tempest_config(),
    programs = programs,
    traces = list(list(trace_id = "manifest-attempt"))
  )
  record <- tempest:::tempest_stage_record_start(
    "perspectives",
    programs$perspectives$program_artifact_id,
    trace_references = list(
      research_run_id = "record-manifest-run",
      mode = "storm",
      role = "program"
    ),
    attempt_id = "manifest-attempt"
  )

  expect_no_error(
    tempest:::tempest_stage_records_validate_manifest(list(record), manifest)
  )
  data <- tempest:::tempest_stage_record_data(record)
  data$program_artifact_id <- paste0("sha256:", strrep("0", 64L))
  mismatch <- tempest:::tempest_stage_record_from_data(data)
  expect_error(
    tempest:::tempest_stage_records_validate_manifest(list(mismatch), manifest),
    class = "tempest_stage_record_error"
  )

  missing_trace_data <- tempest:::tempest_stage_record_data(record)
  missing_trace_data$trace_references$research_run_id <- NULL
  missing_trace <- tempest:::tempest_stage_record_from_data(missing_trace_data)
  expect_error(
    tempest:::tempest_stage_records_validate_manifest(
      list(missing_trace),
      manifest
    ),
    class = "tempest_stage_record_error"
  )

  for (field in c("mode", "role")) {
    missing_bound_data <- tempest:::tempest_stage_record_data(record)
    missing_bound_data$trace_references[[field]] <- NULL
    missing_bound <- tempest:::tempest_stage_record_from_data(
      missing_bound_data
    )
    expect_error(
      tempest:::tempest_stage_records_validate_manifest(
        list(missing_bound),
        manifest
      ),
      class = "tempest_stage_record_error",
      info = field
    )
  }

  transplanted_data <- tempest:::tempest_stage_record_data(record)
  transplanted_data$trace_references$research_run_id <- "another-run"
  transplanted <- tempest:::tempest_stage_record_from_data(transplanted_data)
  expect_error(
    tempest:::tempest_stage_records_validate_manifest(
      list(transplanted),
      manifest
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("stage record collections have one canonical execution order", {
  artifact_id <- paste0("sha256:", strrep("e", 64L))
  later <- tempest:::tempest_stage_record_start(
    "query_decomposition",
    artifact_id,
    attempt_id = "attempt-b",
    started_at = "2026-08-16T01:00:01Z"
  )
  same_time_second <- tempest:::tempest_stage_record_start(
    "query_decomposition",
    artifact_id,
    attempt_id = "attempt-c",
    started_at = "2026-08-16T01:00:00Z"
  )
  same_time_first <- tempest:::tempest_stage_record_start(
    "query_decomposition",
    artifact_id,
    attempt_id = "attempt-a",
    started_at = "2026-08-16T01:00:00Z"
  )

  records <- list()
  records <- tempest:::tempest_stage_records_upsert(records, later)
  records <- tempest:::tempest_stage_records_upsert(records, same_time_second)
  records <- tempest:::tempest_stage_records_upsert(records, same_time_first)
  expect_identical(
    vapply(records, \(record) record@attempt_id, character(1)),
    c("attempt-a", "attempt-c", "attempt-b")
  )

  reordered <- rev(tempest:::tempest_stage_records_data(records))
  expect_error(
    tempest:::tempest_stage_records_from_data(reordered),
    class = "tempest_stage_record_error"
  )
  empty_named <- records
  names(empty_named) <- rep("", length(empty_named))
  expect_error(
    tempest:::tempest_stage_records_validate(empty_named),
    class = "tempest_stage_record_error"
  )
  empty_named_data <- tempest:::tempest_stage_records_data(records)
  names(empty_named_data) <- rep("", length(empty_named_data))
  expect_error(
    tempest:::tempest_stage_records_from_data(empty_named_data),
    class = "tempest_stage_record_error"
  )
})

test_that("executor callbacks are validated before provider execution", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      calls <<- calls + 1L
      list(output = list(queries = list("unused")))
    }
  )

  expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(),
      context = list(
        workspace = fake_store_with_sources(1),
        claim_context = list()
      ),
      fallback = 1
    ),
    class = "simpleError"
  )
  expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(),
      context = list(
        workspace = fake_store_with_sources(1),
        claim_context = list()
      ),
      record_stage = 1
    ),
    class = "tempest_stage_evaluator_contract_error"
  )
  expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(),
      output_reference = function(output, record, context) NULL
    ),
    class = "tempest_stage_evaluator_contract_error"
  )

  claim_execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims"
  )
  expect_error(
    tempest:::tempest_execute_stage(
      claim_execution,
      chat = NULL,
      inputs = list()
    ),
    class = "tempest_stage_evaluator_contract_error"
  )
  expect_identical(calls, 0L)
})

test_that("every stage context contract fails before sync or async providers", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim("A supported claim", source_ids = source_id)
  workspace$add_proposed_claim(claim)
  grounded_context <- list(
    workspace = workspace,
    evidence = list(),
    verified_evidence = list(),
    verified_facts = "",
    min_support_score = 2
  )
  cases <- list(
    perspectives = list(topic = "Topic", n_experts = 0L),
    personas = list(n_experts = 0L),
    query_decomposition = list(max_queries = 0L),
    extract_claims = list(
      workspace = workspace,
      known_source_ids = c(source_id, "unknown-source"),
      claim_context = list(
        claim_type = "finding",
        session_id = NA_character_,
        expert_id = NA_character_,
        retrieval_step_id = NA_character_,
        perspective_id = NA_character_,
        section_id = NA_character_
      )
    ),
    verify_claim_support = list(
      workspace = workspace,
      claim = claim,
      min_support_score = -1
    ),
    next_question = list(unexpected = "field"),
    draft_outline = list(unexpected = "field"),
    refined_outline = c(list(title = "Outline"), grounded_context),
    section_writing = grounded_context,
    lead_section = grounded_context
  )
  provider_calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("sync provider must not run")
    },
    tempest_run_dsprrr_module_async = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("async provider must not run")
    }
  )
  claim_reference <- function(output, running_record, context) {
    stop("output reference must not run")
  }

  for (stage in names(cases)) {
    execution <- tempest:::tempest_program_set_execution(
      tempest_program_set(),
      stage
    )
    output_reference <- if (
      stage %in%
        c(
          "extract_claims",
          "verify_claim_support"
        )
    ) {
      claim_reference
    } else {
      NULL
    }
    expect_error(
      tempest:::tempest_execute_stage(
        execution,
        chat = NULL,
        inputs = fake_stage_inputs(stage),
        context = cases[[stage]],
        output_reference = output_reference
      ),
      class = "tempest_stage_error",
      info = paste(stage, "sync")
    )
    expect_error(
      tempest:::tempest_execute_stage_async(
        execution,
        chat = NULL,
        inputs = fake_stage_inputs(stage),
        context = cases[[stage]],
        output_reference = output_reference
      ),
      class = "tempest_stage_error",
      info = paste(stage, "async")
    )
  }
  expect_identical(provider_calls, 0L)
})

test_that("evaluator resolution rejects drift before provider execution", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  execution$evaluator_version <- "999"
  calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      calls <<- calls + 1L
      list(output = list(queries = "unused"))
    }
  )

  expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(question = "q", topic = "t")
    ),
    class = "tempest_stage_evaluator_contract_error"
  )
  expect_identical(calls, 0L)
})

test_that("fail-closed execution records controlled failure and throws", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims"
  )
  workspace <- fake_store_with_sources(1)
  known_source_ids <- vapply(
    workspace$list_retrieved_resources(),
    \(resource) resource@resource_id,
    character(1)
  )
  records <- list()
  secret <- "Authorization: Bearer sk-live-secret"
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      stop(secret)
    }
  )

  error <- expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = fake_stage_inputs("extract_claims"),
      context = list(
        workspace = workspace,
        known_source_ids = known_source_ids,
        claim_context = list(
          claim_type = "finding",
          session_id = NA_character_,
          expert_id = NA_character_,
          retrieval_step_id = NA_character_,
          perspective_id = NA_character_,
          section_id = NA_character_
        ),
        deputy_execution = list(
          deputy_run_id = "deputy-run-failed",
          deputy_session_id = "deputy-session-failed"
        )
      ),
      output_reference = function(output, record, context) {
        tempest:::tempest_stage_output_reference(
          "workspace_claims",
          character()
        )
      },
      record_stage = function(record, output = NULL) {
        records <<- tempest:::tempest_stage_records_upsert(records, record)
      }
    ),
    class = "tempest_stage_execution_error"
  )
  terminal <- tempest:::tempest_stage_error_record(error)

  expect_identical(length(records), 1L)
  expect_identical(records[[1]]@status, "failed")
  expect_identical(terminal, records[[1]])
  expect_identical(terminal@fallback_policy, "fail_closed")
  expect_identical(terminal@fallback_taken, FALSE)
  expect_identical(
    terminal@trace_references$deputy_run_id,
    "deputy-run-failed"
  )
  expect_identical(
    terminal@trace_references$deputy_session_id,
    "deputy-session-failed"
  )
  expect_identical(terminal@failure_message, "Primary stage execution failed.")
  expect_no_match(conditionMessage(error), "sk-live-secret", fixed = TRUE)
  printed <- paste(capture.output(print(error)), collapse = "\n")
  expect_no_match(printed, "sk-live-secret", fixed = TRUE)
  expect_no_match(
    jsonlite::toJSON(tempest:::tempest_stage_record_data(terminal)),
    "sk-live-secret",
    fixed = TRUE
  )
})

test_that("exploratory fallback is evaluated and visible", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition",
    trace_context = list(
      research_run_id = "fallback-run",
      knowledge_snapshot_id = "snapshot-1",
      expert_id = "expert-1",
      correlation_id = "correlation-1",
      mode = "storm",
      role = "writer"
    )
  )
  records <- list()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) stop("primary secret")
  )

  result <- tempest:::tempest_execute_stage(
    execution,
    chat = NULL,
    inputs = list(question = "Question", topic = "Topic"),
    context = list(max_queries = 3L),
    record_stage = function(record, output = NULL) {
      records <<- tempest:::tempest_stage_records_upsert(records, record)
    }
  )

  expect_s3_class(result, "tempest_stage_result")
  expect_identical(result$output, list(queries = "Question"))
  expect_identical(result$record@fallback_taken, TRUE)
  expect_identical(result$record@execution_path, "exploratory")
  expect_identical(
    result$record@fallback_implementation,
    "tempest::fallback/query-decomposition/original-question@1"
  )
  expect_identical(result$record@publication_allowed, FALSE)
  expect_identical(
    result$record@trace_references$knowledge_snapshot_id,
    "snapshot-1"
  )
  expect_identical(result$record@trace_references$expert_id, "expert-1")
  expect_identical(
    result$record@trace_references$correlation_id,
    "correlation-1"
  )
  expect_identical(
    tempest:::tempest_stage_record_from_data(
      tempest:::tempest_stage_record_data(result$record)
    ),
    result$record
  )
  expect_identical(records[[1]], result$record)
})

test_that("Deputy execution binds StageRecords without changing module traces", {
  seen_traces <- list()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(execution, ...) {
      seen_traces[[length(seen_traces) + 1L]] <<- execution$trace_context
      list(output = list(queries = "Question"))
    }
  )
  execution <- function() {
    tempest:::tempest_program_set_execution(
      tempest_program_set(),
      "query_decomposition",
      trace_context = list(
        research_run_id = "stage-record-run",
        mode = "storm",
        role = "writer"
      )
    )
  }

  storm <- tempest:::tempest_execute_stage(
    execution(),
    chat = NULL,
    inputs = list(question = "Question", topic = "Topic"),
    context = list(max_queries = 2L)
  )
  deputy <- tempest:::tempest_execute_stage(
    execution(),
    chat = NULL,
    inputs = list(question = "Question", topic = "Topic"),
    context = list(
      max_queries = 2L,
      deputy_execution = list(
        deputy_run_id = "deputy-run-succeeded",
        deputy_session_id = "deputy-session-succeeded",
        parent_run_id = "deputy-parent-succeeded",
        delegation_id = "delegation-succeeded",
        tool_call_id = "tool-call-succeeded"
      )
    )
  )

  expect_identical(
    storm$record@output_reference$content_digest,
    deputy$record@output_reference$content_digest
  )
  expect_null(storm$record@trace_references$deputy_run_id)
  expect_null(storm$record@trace_references$deputy_session_id)
  expect_identical(
    deputy$record@trace_references$deputy_run_id,
    "deputy-run-succeeded"
  )
  expect_identical(
    deputy$record@trace_references$deputy_session_id,
    "deputy-session-succeeded"
  )
  expect_identical(
    deputy$record@trace_references$parent_run_id,
    "deputy-parent-succeeded"
  )
  expect_identical(
    deputy$record@trace_references$delegation_id,
    "delegation-succeeded"
  )
  expect_identical(
    deputy$record@trace_references$tool_call_id,
    "tool-call-succeeded"
  )
  expect_length(seen_traces, 2L)
  expect_all_true(vapply(
    seen_traces,
    function(trace) {
      !any(c("deputy_run_id", "deputy_session_id") %in% names(trace))
    },
    logical(1)
  ))
})

test_that("malformed Deputy stage context fails before provider execution", {
  calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      calls <<- calls + 1L
      list(output = list(queries = "Question"))
    }
  )
  malformed <- list(
    list(deputy_run_id = "run-only"),
    list(
      deputy_session_id = "session-reversed",
      deputy_run_id = "run-reversed"
    ),
    c(
      deputy_run_id = "run-atomic",
      deputy_session_id = "session-atomic"
    ),
    list(
      deputy_run_id = 1L,
      deputy_session_id = "session-numeric"
    ),
    list(
      deputy_run_id = "api-key-secret",
      deputy_session_id = "session-credential"
    ),
    list(
      deputy_run_id = "run-extra",
      deputy_session_id = "session-extra",
      secret = "not-allowed"
    ),
    list(
      deputy_run_id = "run-partial",
      deputy_session_id = "session-partial",
      parent_run_id = "parent-partial"
    ),
    list(
      deputy_run_id = "run-self-parent",
      deputy_session_id = "session-self-parent",
      parent_run_id = "run-self-parent",
      delegation_id = "delegation-self-parent",
      tool_call_id = "tool-self-parent"
    ),
    list(
      deputy_run_id = "run-reordered",
      deputy_session_id = "session-reordered",
      delegation_id = "delegation-reordered",
      parent_run_id = "parent-reordered",
      tool_call_id = "tool-reordered"
    )
  )

  for (deputy_execution in malformed) {
    expect_error(
      tempest:::tempest_execute_stage(
        tempest:::tempest_program_set_execution(
          tempest_program_set(),
          "query_decomposition"
        ),
        chat = NULL,
        inputs = list(question = "Question", topic = "Topic"),
        context = list(
          max_queries = 2L,
          deputy_execution = deputy_execution
        )
      ),
      class = "tempest_stage_governance_error"
    )
  }
  expect_identical(calls, 0L)
})

test_that("fallback success distinguishes primary output rejection", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(output = list(queries = list(list(bad = "shape"))))
    }
  )

  result <- tempest:::tempest_execute_stage(
    execution,
    chat = NULL,
    inputs = list(question = "Question", topic = "Topic"),
    context = list(max_queries = 2L)
  )

  expect_identical(
    result$record@failure_class,
    "tempest_stage_output_validation_error"
  )
  expect_identical(
    result$record@failure_message,
    "Primary stage output failed validation."
  )
})

test_that("malformed provider records remain output-validation failures", {
  perspectives <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "perspectives"
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) list(output = 42),
    tempest_stage_fallback_perspectives = function(chat, inputs, context) {
      list(
        title = "Fallback report",
        perspectives = list(list(
          name = "Technical",
          description = "Technical evidence",
          key_questions = list("What is known?")
        ))
      )
    }
  )

  result <- tempest:::tempest_execute_stage(
    perspectives,
    chat = NULL,
    inputs = fake_stage_inputs("perspectives"),
    context = list(topic = "Topic", n_experts = 1L)
  )
  expect_identical(result$record@fallback_taken, TRUE)
  expect_identical(
    result$record@failure_class,
    "tempest_stage_output_validation_error"
  )

  personas <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "personas"
  )
  expect_error(
    tempest:::tempest_stage_evaluate(
      personas,
      data.frame(personas = I(list(list(name = "x", title = "y"))))
    ),
    class = "tempest_stage_output_validation_error"
  )

  claims <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims"
  )
  expect_error(
    tempest:::tempest_stage_evaluate(claims, "not-a-record"),
    class = "tempest_stage_output_validation_error"
  )

  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "A claim",
    source_ids = source_id
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  evidence_span <- workspace$get_evidence_for_proposed_claim(
    claim@claim_id
  )[[1]]
  verification <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "verify_claim_support"
  )
  records <- list()
  error <- expect_error(
    tempest:::tempest_execute_stage(
      verification,
      chat = NULL,
      inputs = list(
        claim_text = claim@claim_text,
        source_excerpts = tempest:::tempest_verification_span_input(
          claim,
          evidence_span,
          workspace
        )
      ),
      context = list(
        workspace = workspace,
        claim = claim,
        evidence_span = evidence_span,
        min_support_score = 0.7,
        verified_at = claim@verified_at,
        verifier_model = claim@verifier_model
      ),
      output_reference = function(output, record, context) {
        tempest:::tempest_stage_output_reference(
          "claim_supports",
          output@claim_support_id
        )
      },
      record_stage = function(record, output = NULL) {
        records <<- tempest:::tempest_stage_records_upsert(records, record)
      }
    ),
    class = "tempest_stage_output_validation_error"
  )
  expect_identical(records[[1]]@status, "failed")
  expect_identical(
    tempest:::tempest_stage_error_record(error)@failure_message,
    "Primary stage output failed validation."
  )
})

test_that("all ProgramSet evaluators reject non-schema fields and omissions", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  evidence <- tempest:::tempest_claim(
    "A supported claim",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(evidence)
  evidence <- fake_verify_claim_supports(workspace, list(evidence))[[1]]
  evidence_span <- workspace$get_evidence_for_proposed_claim(
    evidence@claim_id
  )[[1]]
  outline <- list(
    title = "Report",
    sections = list(list(
      title = "Evidence",
      summary = "Summary",
      subsections = list(list(
        title = "Finding",
        bullets = list("Supported claim")
      ))
    ))
  )
  cases <- list(
    perspectives = list(
      output = list(
        title = "Report",
        perspectives = list(list(
          name = "Technical",
          description = "Technical evidence",
          key_questions = list("What is known?")
        ))
      ),
      required = "title",
      context = list(topic = "Topic", n_experts = 1L)
    ),
    personas = list(
      output = list(
        personas = list(list(
          name = "Dr. Ada Stone",
          title = "Researcher",
          affiliation = "Institute",
          background = "Domain specialist",
          focus_areas = list("Evidence"),
          perspective = "Tests the evidence",
          initial_questions = list("What is known?")
        ))
      ),
      required = "personas",
      context = list(n_experts = 1L)
    ),
    query_decomposition = list(
      output = list(queries = list("Targeted query")),
      required = "queries",
      context = list(max_queries = 2L)
    ),
    extract_claims = list(
      output = list(facts = list()),
      required = "facts",
      context = list()
    ),
    verify_claim_support = list(
      output = list(
        status = "supported",
        score = 0.9,
        rationale = "Direct support"
      ),
      required = "status",
      context = list(
        workspace = workspace,
        claim = evidence,
        evidence_span = evidence_span,
        min_support_score = 0.7,
        verified_at = evidence@verified_at,
        verifier_model = evidence@verifier_model
      )
    ),
    next_question = list(
      output = list(question = "What next?", done = FALSE),
      required = "question",
      context = list()
    ),
    draft_outline = list(
      output = outline,
      required = "title",
      context = list()
    ),
    refined_outline = list(
      output = outline,
      required = "title",
      context = list(
        workspace = workspace,
        evidence = list(evidence),
        min_support_score = 0.7
      )
    ),
    section_writing = list(
      output = list(
        section_text = paste0("A supported claim [", source_id, "].")
      ),
      required = "section_text",
      context = list(
        workspace = workspace,
        evidence = list(evidence),
        min_support_score = 0.7
      )
    ),
    lead_section = list(
      output = list(
        lead_section = paste0("A supported claim [", source_id, "].")
      ),
      required = "lead_section",
      context = list(
        workspace = workspace,
        evidence = list(evidence),
        min_support_score = 0.7
      )
    )
  )

  for (stage in names(cases)) {
    case <- cases[[stage]]
    execution <- tempest:::tempest_program_set_execution(
      tempest_program_set(),
      stage
    )
    unexpected <- case$output
    unexpected$unexpected <- list(nested = "ignored")
    expect_error(
      tempest:::tempest_stage_evaluate(
        execution,
        unexpected,
        case$context
      ),
      class = "tempest_stage_output_validation_error",
      info = paste(stage, "unexpected field")
    )

    missing <- case$output
    missing[[case$required]] <- NULL
    expect_error(
      tempest:::tempest_stage_evaluate(execution, missing, case$context),
      class = "tempest_stage_output_validation_error",
      info = paste(stage, "missing field")
    )
  }
})

test_that("persona state digests bind exact profile records and fingerprints", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "personas"
  )
  evaluated <- tempest:::tempest_stage_evaluate(
    execution,
    list(
      personas = list(list(
        name = "Dr. Ada Stone",
        title = "Researcher",
        affiliation = "Institute",
        background = "Domain specialist",
        focus_areas = list("Evidence"),
        perspective = "Tests the evidence",
        initial_questions = list("What is known?")
      ))
    ),
    context = list(n_experts = 1L)
  )$output
  original_records <- tempest:::tempest_expert_records(evaluated)
  original_digest <- tempest:::tempest_stage_state_output_digest(
    "personas",
    evaluated
  )
  renamed <- evaluated
  renamed[[1]] <- S7::set_props(renamed[[1]], name = "Dr. Eve Stone")
  renamed_records <- tempest:::tempest_expert_records(renamed)

  expect_identical(
    identical(
      original_records[[1]]$fingerprint,
      renamed_records[[1]]$fingerprint
    ),
    FALSE
  )
  expect_identical(
    identical(
      original_digest,
      tempest:::tempest_stage_state_output_digest("personas", renamed)
    ),
    FALSE
  )
})

test_that("nested ProgramSet records reject non-schema fields", {
  perspectives <- list(
    title = "Report",
    perspectives = list(list(
      name = "Technical",
      description = "Technical evidence",
      key_questions = list("What is known?"),
      unexpected = list(nested = "ignored")
    ))
  )
  personas <- list(
    personas = list(list(
      name = "Dr. Ada Stone",
      title = "Researcher",
      affiliation = "Institute",
      background = "Domain specialist",
      focus_areas = list("Evidence"),
      perspective = "Tests the evidence",
      initial_questions = list("What is known?"),
      unexpected = list(nested = "ignored")
    ))
  )
  outline <- list(
    title = "Report",
    sections = list(list(
      title = "Evidence",
      summary = "Summary",
      subsections = list(list(
        title = "Finding",
        bullets = list("Supported claim"),
        unexpected = list(nested = "ignored")
      ))
    ))
  )
  cases <- list(
    perspectives = list(perspectives, list(topic = "Topic", n_experts = 1L)),
    personas = list(personas, list(n_experts = 1L)),
    draft_outline = list(outline, list())
  )
  for (stage in names(cases)) {
    expect_error(
      tempest:::tempest_stage_evaluate(
        tempest:::tempest_program_set_execution(tempest_program_set(), stage),
        cases[[stage]][[1]],
        cases[[stage]][[2]]
      ),
      class = "tempest_stage_output_validation_error",
      info = stage
    )
  }
})

test_that("evidence support uses the least trustworthy cited claim", {
  make_claim <- function(status, score = NA_real_, id) {
    tempest:::tempest_claim(
      paste("Claim", id),
      source_ids = "S123456789abc",
      verification_status = status,
      support_score = score,
      claim_id = paste0("C", id)
    )
  }
  verified <- make_claim("supported", 0.9, "verified")
  low <- make_claim("supported", 0.4, "low")
  partial <- make_claim("partially_supported", 0.6, "partial")
  unsupported <- make_claim("unsupported", 0.1, "unsupported")
  unverified <- make_claim("unverified", NA_real_, "unverified")
  unverifiable <- make_claim("unverifiable", NA_real_, "unverifiable")
  contradicted <- make_claim("contradicted", 0.1, "contradicted")
  support <- \(claims) {
    tempest:::tempest_stage_evidence_support(
      claims,
      list(min_support_score = 0.7)
    )
  }

  expect_identical(support(list(verified)), "verified")
  expect_identical(support(list(verified, partial)), "partially_supported")
  expect_identical(support(list(verified, unverified)), "unknown")
  expect_identical(support(list(partial, unverifiable)), "unknown")
  expect_identical(support(list(verified, unsupported)), "unsupported")
  expect_identical(support(list(low, partial)), "unsupported")
  expect_identical(support(list(contradicted, unsupported)), "conflicted")
})

test_that("direct grounded output binds exact authoritative support", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "A verified observation",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "section_writing"
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(
        output = list(
          section_text = paste0("A verified observation [", source_id, "].")
        )
      )
    }
  )

  result <- tempest:::tempest_execute_stage(
    execution,
    chat = NULL,
    inputs = fake_stage_inputs("section_writing"),
    context = list(
      workspace = workspace,
      evidence = list(claim),
      verified_evidence = list(claim),
      verified_facts = "A verified observation",
      min_support_score = 0.7
    )
  )
  expect_identical(result$record@status, "succeeded")
  expect_identical(result$record@execution_path, "grounded")
  expect_identical(result$record@support_status, "verified")
  expect_identical(result$record@publication_allowed, TRUE)
})

test_that("a claim without exact supports fails grounded preflight", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "A manually marked observation",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- workspace$get_proposed_claim(claim@claim_id)
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "section_writing"
  )
  context <- list(
    workspace = workspace,
    evidence = list(claim),
    verified_evidence = list(claim),
    verified_facts = claim@claim_text,
    min_support_score = 0.7
  )
  provider_calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("sync provider must not run")
    },
    tempest_run_dsprrr_module_async = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("async provider must not run")
    }
  )

  expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = fake_stage_inputs("section_writing"),
      context = context
    ),
    class = "tempest_stage_governance_error"
  )
  expect_error(
    tempest:::tempest_execute_stage_async(
      execution,
      chat = NULL,
      inputs = fake_stage_inputs("section_writing"),
      context = context
    ),
    class = "tempest_stage_governance_error"
  )
  expect_identical(provider_calls, 0L)
})

test_that("grounded writing treats only closed structural headings as metadata", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "A verified observation",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "section_writing"
  )
  context <- list(
    workspace = workspace,
    evidence = list(claim),
    min_support_score = 0.7
  )

  expect_no_error(tempest:::tempest_stage_evaluate(
    execution,
    list(
      section_text = paste0(
        "## Evidence\n\nA verified observation [",
        source_id,
        "]."
      )
    ),
    context
  ))
  expect_error(
    tempest:::tempest_stage_evaluate(
      execution,
      list(
        section_text = paste0(
          "## Cancer is cured\n\nA verified observation [",
          source_id,
          "]."
        )
      ),
      context
    ),
    class = "tempest_stage_output_validation_error"
  )
})

test_that("refined outline remains non-publishable planning metadata", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "Verified planning evidence",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "refined_outline"
  )
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(
        output = list(
          title = "Outline",
          sections = list(list(
            title = "Evidence",
            summary = "Plan the evidence section.",
            subsections = list(list(
              title = "Findings",
              bullets = list("Verified planning evidence")
            ))
          ))
        )
      )
    }
  )

  result <- tempest:::tempest_execute_stage(
    execution,
    chat = NULL,
    inputs = fake_stage_inputs("refined_outline"),
    context = list(
      workspace = workspace,
      title = "Outline",
      evidence = list(claim),
      verified_evidence = list(claim),
      verified_facts = "Verified planning evidence",
      min_support_score = 0.7
    )
  )

  expect_identical(result$record@support_status, "unknown")
  expect_identical(result$record@publication_allowed, FALSE)
})

test_that("grounded writing rejects malformed and incomplete source bindings", {
  workspace <- fake_store_with_sources(2)
  source_ids <- vapply(
    workspace$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  claim <- tempest:::tempest_claim(
    "A verified observation",
    source_ids = source_ids,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "section_writing"
  )
  context <- list(
    workspace = workspace,
    evidence = list(claim),
    min_support_score = 0.7
  )

  expect_error(
    tempest:::tempest_stage_evaluate(
      execution,
      list(
        section_text = paste0(
          "A verified observation [",
          source_ids[[1]],
          "]."
        )
      ),
      context
    ),
    class = "tempest_stage_output_validation_error"
  )
  expect_error(
    tempest:::tempest_stage_evaluate(
      execution,
      list(
        section_text = paste0(
          "A verified observation [",
          source_ids[[1]],
          "] [Sforged-source]."
        )
      ),
      context
    ),
    class = "tempest_stage_output_validation_error"
  )
})

test_that("grounded writing rejects citation-free primary and fallback output", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "A verified observation",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "section_writing"
  )
  records <- list()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(output = list(section_text = "Citation-free primary output."))
    },
    tempest_stage_fallback_section_writing = function(chat, inputs, context) {
      list(section_text = "Citation-free fallback output.")
    }
  )

  error <- expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = fake_stage_inputs("section_writing"),
      context = list(
        workspace = workspace,
        evidence = list(claim),
        verified_evidence = list(claim),
        verified_facts = "A verified observation",
        min_support_score = 0.7
      ),
      record_stage = function(record, output = NULL) {
        records <<- tempest:::tempest_stage_records_upsert(records, record)
      }
    ),
    class = "tempest_stage_fallback_error"
  )

  terminal <- tempest:::tempest_stage_error_record(error)
  expect_identical(terminal@status, "failed")
  expect_identical(terminal@fallback_taken, TRUE)
  expect_identical(terminal@failure_class, "tempest_stage_fallback_error")
  expect_identical(records[[1]], terminal)
})

test_that("grounded fallback rejects absent verified evidence", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "section_writing"
  )
  records <- list()
  fallback_calls <- 0L
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "An unverified observation",
    source_ids = source_id,
    verification_status = "unverified"
  )
  workspace$add_proposed_claim(claim)
  provider_calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("primary failed")
    },
    tempest_stage_fallback_section_writing = function(chat, inputs, context) {
      fallback_calls <<- fallback_calls + 1L
      list(section_text = "Never run")
    }
  )

  error <- expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = fake_stage_inputs("section_writing"),
      context = list(
        workspace = workspace,
        evidence = list(claim),
        verified_evidence = list(claim),
        verified_facts = "An unverified observation",
        min_support_score = 0.7
      ),
      record_stage = function(record, output = NULL) {
        records <<- tempest:::tempest_stage_records_upsert(records, record)
      }
    ),
    class = "tempest_stage_governance_error"
  )

  expect_identical(fallback_calls, 0L)
  expect_identical(provider_calls, 0L)
  expect_length(records, 0L)
})

test_that("terminal recorder failure becomes a controlled commit failure", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  seen <- list()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(output = list(queries = list("One query")))
    }
  )

  error <- expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = fake_stage_inputs("query_decomposition"),
      context = list(max_queries = 1L),
      record_stage = function(record, output = NULL) {
        seen[[length(seen) + 1L]] <<- record
        if (!identical(record@status, "running")) {
          stop("database password leaked")
        }
      }
    ),
    class = "tempest_stage_commit_error"
  )
  terminal <- tempest:::tempest_stage_error_record(error)

  expect_identical(terminal@status, "failed")
  expect_identical(
    terminal@failure_message,
    "Validated stage output could not be committed."
  )
  expect_no_match(
    jsonlite::toJSON(tempest:::tempest_stage_record_data(terminal)),
    "password",
    fixed = TRUE
  )
})

test_that("output references must name the evaluated claim IDs", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "A supported claim",
    source_ids = source_id
  )
  workspace$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(workspace, list(claim))[[1]]
  evidence_span <- workspace$get_evidence_for_proposed_claim(
    claim@claim_id
  )[[1]]
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "verify_claim_support"
  )
  records <- list()
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      list(
        output = list(
          status = "supported",
          score = 0.9,
          rationale = "Direct support"
        )
      )
    }
  )

  error <- expect_error(
    tempest:::tempest_execute_stage(
      execution,
      chat = NULL,
      inputs = list(
        claim_text = claim@claim_text,
        source_excerpts = tempest:::tempest_verification_span_input(
          claim,
          evidence_span,
          workspace
        )
      ),
      context = list(
        workspace = workspace,
        claim = claim,
        evidence_span = evidence_span,
        min_support_score = 0.7,
        verified_at = claim@verified_at,
        verifier_model = claim@verifier_model
      ),
      output_reference = function(output, record, context) {
        tempest:::tempest_stage_output_reference(
          "claim_supports",
          "C-wrong-claim",
          content_digest = tempest:::tempest_stage_verification_output_digest(
            output,
            record,
            context$claim,
            context$evidence_span,
            context$workspace
          )
        )
      },
      record_stage = function(record, output = NULL) {
        records <<- tempest:::tempest_stage_records_upsert(records, record)
      }
    ),
    class = "tempest_stage_commit_error"
  )
  terminal <- tempest:::tempest_stage_error_record(error)
  expect_identical(terminal@status, "failed")
  expect_identical(
    terminal@failure_message,
    "Validated stage output could not be committed."
  )
  expect_identical(records[[1]], terminal)
})

test_that("claim support rejects spans without captured source text", {
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(fake_source(
    "https://example.org/empty-grounding-source",
    content_text = NA_character_
  ))
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  claim <- tempest:::tempest_claim(
    "An uncaptured observation",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  workspace$add_proposed_claim(claim)
  expect_error(
    fake_verify_claim_supports(workspace, list(claim)),
    class = "tempest_research_workspace_integrity_error"
  )
})

test_that("async cancellation records cancellation and rejects output", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  records <- list()
  provider_calls <- 0L
  local_mocked_bindings(
    tempest_run_dsprrr_module_async = function(...) {
      provider_calls <<- provider_calls + 1L
      promises::promise_resolve(list(queries = list("Late query")))
    }
  )

  request <- tempest:::tempest_execute_stage_async(
    execution,
    chat = NULL,
    inputs = fake_stage_inputs("query_decomposition"),
    context = list(max_queries = 1L),
    record_stage = function(record, output = NULL) {
      records <<- tempest:::tempest_stage_records_upsert(records, record)
    },
    is_current = function() FALSE
  )
  settled <- await_tempest_promise(request)

  expect_s3_class(settled$error, "tempest_stage_cancelled")
  expect_null(settled$value)
  expect_identical(records[[1]]@status, "cancelled")
  expect_identical(records[[1]]@output_reference, list())
  expect_identical(provider_calls, 0L)
})

test_that("async stage telemetry remains open and preserves the stage result", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  local_otel_opt_in()
  otel <- local_fake_otel()
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  control <- new.env(parent = emptyenv())
  local_mocked_bindings(
    tempest_run_dsprrr_module_async = function(...) {
      promises::promise(function(resolve, reject) {
        control$resolve <- resolve
      })
    }
  )

  request <- tempest:::tempest_execute_stage_async(
    execution,
    chat = NULL,
    inputs = fake_stage_inputs("query_decomposition"),
    context = list(
      max_queries = 1L,
      attempt_id = "attempt-otel-async",
      now = function() "2026-08-20T00:00:00.000000Z"
    )
  )
  span <- otel$spans[[1L]]

  expect_identical(span$name, "tempest.stage.execute")
  expect_identical(span$attributes[["tempest.stage"]], "query_decomposition")
  expect_identical(span$deactivate_count, 1L)
  expect_identical(span$end_count, 0L)

  control$resolve(list(queries = "Exact query"))
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_s3_class(settled$value, "tempest_stage_result")
  expect_identical(settled$value$record@fallback_taken, FALSE)
  expect_identical(span$attributes[["tempest.status"]], "succeeded")
  expect_identical(span$statuses, "ok")
  expect_identical(span$end_count, 1L)
})

test_that("async fallback rejection rechecks current invocation", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "query_decomposition"
  )
  records <- list()
  current <- TRUE
  fallback_calls <- 0L
  reject_fallback <- NULL
  local_mocked_bindings(
    tempest_run_dsprrr_module_async = function(...) {
      promises::promise_reject(simpleError("primary failed"))
    },
    tempest_stage_fallback_query_decomposition = function(
      chat,
      inputs,
      context
    ) {
      fallback_calls <<- fallback_calls + 1L
      promises::promise(function(resolve, reject) {
        reject_fallback <<- reject
      })
    }
  )

  request <- tempest:::tempest_execute_stage_async(
    execution,
    chat = NULL,
    inputs = list(question = "Question", topic = "Topic"),
    context = list(max_queries = 1L),
    record_stage = function(record, output = NULL) {
      records <<- tempest:::tempest_stage_records_upsert(records, record)
    },
    is_current = function() current
  )
  for (index in seq_len(20L)) {
    if (is.function(reject_fallback)) {
      break
    }
    later::run_now(timeoutSecs = 0.01)
  }
  expect_identical(fallback_calls, 1L)
  expect_type(reject_fallback, "closure")
  current <- FALSE
  reject_fallback(simpleError("late fallback failed"))
  settled <- await_tempest_promise(request)

  expect_s3_class(settled$error, "tempest_stage_cancelled")
  expect_identical(records[[1]]@status, "cancelled")
})
