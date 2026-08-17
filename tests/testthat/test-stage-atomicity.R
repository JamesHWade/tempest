test_that("claim extraction commits exact referenced claims atomically", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  output <- list(tempest_claim(
    claim_text = "Atomic claim",
    source_ids = source_id,
    confidence = "high"
  ))
  captured_reference <- NULL
  captured_claims <- NULL
  running <- tempest:::tempest_stage_record_start(
    "extract_claims",
    paste0("sha256:", strrep("a", 64L))
  )
  local_mocked_bindings(
    tempest_execute_stage = function(
      execution,
      chat,
      inputs,
      context,
      output_reference,
      record_stage,
      ...
    ) {
      record_stage(running)
      captured_reference <<- output_reference(output, running, context)
      succeeded <- tempest:::tempest_stage_record_succeed(
        running,
        captured_reference,
        support_status = "unknown"
      )
      record_stage(succeeded, output)
      list(output = output, record = succeeded)
    }
  )

  tempest:::tempest_extract_facts_from_answer(
    chat = NULL,
    answer_text = "Atomic claim",
    store = store,
    module = test_program_executions()$extract_claims,
    record_stage = function(record, output = NULL) {
      if (!is.null(output)) {
        captured_claims <<- output
      }
      invisible(record)
    }
  )

  claims <- store$list_proposed_claims()
  claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  captured_ids <- vapply(
    captured_claims,
    \(claim) claim@claim_id,
    character(1)
  )
  expect_equal(captured_reference$kind, "workspace_claims")
  expect_equal(unlist(captured_reference$ids), claim_ids)
  expect_identical(
    captured_reference$content_digest,
    tempest:::tempest_stage_claims_output_digest(output, running)
  )
  expect_equal(captured_ids, claim_ids)
})

test_that("claim extraction rolls back claims when terminal recording fails", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  output <- list(tempest_claim(
    claim_text = "Rolled-back claim",
    source_ids = source_id,
    confidence = "high"
  ))
  running <- tempest:::tempest_stage_record_start(
    "extract_claims",
    paste0("sha256:", strrep("a", 64L))
  )
  local_mocked_bindings(
    tempest_execute_stage = function(
      execution,
      chat,
      inputs,
      context,
      output_reference,
      record_stage,
      ...
    ) {
      record_stage(running)
      reference <- output_reference(output, running, context)
      succeeded <- tempest:::tempest_stage_record_succeed(
        running,
        reference,
        support_status = "unknown"
      )
      record_stage(succeeded, output)
      list(output = output, record = succeeded)
    }
  )

  expect_error(
    tempest:::tempest_extract_facts_from_answer(
      chat = NULL,
      answer_text = "Rolled-back claim",
      store = store,
      module = test_program_executions()$extract_claims,
      record_stage = function(record, output = NULL) {
        if (!is.null(output)) {
          stop("ledger write failed")
        }
        invisible(record)
      }
    ),
    class = "simpleError"
  )
  expect_length(store$list_proposed_claims(), 0L)
})

test_that("verification later failure records every aborted pair attempt", {
  store <- tempest_research_workspace()
  test_add_verifiable_claim(store, "first")
  test_add_verifiable_claim(store, "second")
  calls <- 0L
  recorded <- list()
  artifact_id <- paste0("sha256:", paste(rep("a", 64L), collapse = ""))
  local_mocked_bindings(
    tempest_verify_one_claim_span = function(
      claim,
      span,
      store,
      verifier,
      module,
      verified_at,
      verifier_model,
      min_support_score,
      record_stage
    ) {
      calls <<- calls + 1L
      running <- tempest:::tempest_stage_record_start(
        stage = "verify_claim_support",
        program_artifact_id = artifact_id,
        trace_references = list(
          min_support_score = "0.7",
          verified_at = verified_at,
          verifier_model = verifier_model
        )
      )
      record_stage(running)
      if (calls == 2L) {
        failed <- tempest:::tempest_stage_record_fail(
          running,
          error = simpleError("judge failed"),
          kind = "execution"
        )
        record_stage(failed)
        stop("judge failed")
      }
      result <- tempest_claim_support(
        claim_id = claim@claim_id,
        evidence_span_id = span@evidence_span_id,
        source_id = span@source_id,
        verification_status = "supported",
        support_score = 0.9,
        rationale = "Supported"
      )
      succeeded <- tempest:::tempest_stage_record_succeed(
        running,
        output_reference = tempest:::tempest_stage_output_reference(
          "claim_supports",
          result@claim_support_id,
          content_digest = tempest:::tempest_stage_verification_output_digest(
            result,
            running,
            claim,
            span,
            store
          )
        ),
        support_status = "verified"
      )
      record_stage(succeeded, result)
      result
    }
  )

  expect_error(
    tempest:::tempest_verify_claims_internal(
      workspace = store,
      verifier = NULL,
      policy = "claim_verified",
      verifier_model = "judge-model",
      program = "bound-program",
      min_support_score = 0.7,
      record_stage = function(record, output = NULL) {
        recorded[[length(recorded) + 1L]] <<- record
        invisible(record)
      }
    ),
    class = "simpleError"
  )

  statuses <- vapply(
    store$list_proposed_claims(),
    \(claim) claim@verification_status,
    character(1)
  )
  expect_equal(statuses, rep("unverified", 2L))
  expect_null(store$citation_audit)
  expect_length(recorded, 2L)
  expect_identical(
    vapply(recorded, \(record) record@status, ""),
    rep("failed", 2L)
  )
  expect_setequal(
    vapply(recorded, \(record) record@failure_class, ""),
    c("tempest_stage_commit_error", "tempest_stage_execution_error")
  )
  expect_identical(
    vapply(recorded, \(record) length(record@output_reference), integer(1)),
    c(0L, 0L)
  )
})

test_that("verification success ledger batch fails before any external swap", {
  store <- tempest_research_workspace()
  test_add_verifiable_claim(store, "first")
  test_add_verifiable_claim(store, "second")
  artifact_id <- paste0("sha256:", strrep("b", 64L))
  ledger <- list()
  batch_calls <- 0L
  local_mocked_bindings(
    tempest_verify_one_claim_span = function(
      claim,
      span,
      store,
      verifier,
      module,
      verified_at,
      verifier_model,
      min_support_score,
      record_stage
    ) {
      running <- tempest:::tempest_stage_record_start(
        "verify_claim_support",
        artifact_id,
        trace_references = list(
          min_support_score = "0.7",
          verified_at = verified_at,
          verifier_model = verifier_model
        )
      )
      record_stage(running)
      result <- tempest_claim_support(
        claim_id = claim@claim_id,
        evidence_span_id = span@evidence_span_id,
        source_id = span@source_id,
        verification_status = "supported",
        support_score = 0.9,
        rationale = "Supported"
      )
      reference <- tempest:::tempest_stage_output_reference(
        "claim_supports",
        result@claim_support_id,
        content_digest = tempest:::tempest_stage_verification_output_digest(
          result,
          running,
          claim,
          span,
          store
        )
      )
      record_stage(
        tempest:::tempest_stage_record_succeed(
          running,
          reference,
          support_status = "verified"
        ),
        result
      )
      result
    }
  )

  expect_error(
    tempest:::tempest_verify_claims_internal(
      workspace = store,
      verifier = NULL,
      policy = "claim_verified",
      verifier_model = "judge-model",
      program = "bound-program",
      min_support_score = 0.7,
      record_stages = function(records, outputs = NULL) {
        batch_calls <<- batch_calls + 1L
        candidate <- ledger
        for (index in seq_along(records)) {
          candidate <- tempest:::tempest_stage_records_upsert(
            candidate,
            records[[index]]
          )
          if (index == 2L) {
            stop("second record rejected")
          }
        }
        ledger <<- candidate
      }
    ),
    class = "simpleError"
  )

  expect_identical(batch_calls, 1L)
  expect_length(ledger, 0L)
  expect_null(store$citation_audit)
  expect_equal(
    vapply(
      store$list_proposed_claims(),
      \(claim) claim@verification_status,
      character(1)
    ),
    rep("unverified", 2L)
  )
})
