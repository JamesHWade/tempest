test_that("tempest_sections_to_write skips lead-style sections", {
  outline <- list(
    sections = list(
      list(title = "Introduction"),
      list(title = "Mechanisms"),
      list(title = "Summary"),
      list(title = "Applications")
    )
  )

  sections <- tempest:::tempest_sections_to_write(outline)

  expect_equal(
    vapply(sections, function(section) section$title, character(1)),
    c("Mechanisms", "Applications")
  )
})

test_that("empty section fact context does not tell the writer to call tools", {
  local_mocked_bindings(tempest_semantic_filter_facts = function(...) list())

  facts <- tempest:::tempest_section_facts_text(
    retriever = NULL,
    store = tempest_research_workspace(),
    section_title = "Mechanisms",
    max_items = 5
  )

  expect_match(facts, "no directly matched facts")
  expect_no_match(facts, "call tools")
})

test_that("tempest_run_section_job wraps section text as markdown", {
  writer <- list()
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(
        list(
          output = list(
            section_text = paste0("Section body [", source_id, "].")
          )
        ),
        class = "dsprrr_result"
      )
    }
  )
  evidence <- list(tempest_claim(
    "Section body",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  ))
  workspace$add_proposed_claim(evidence[[1]])
  evidence <- fake_verify_claim_supports(workspace, evidence)
  job <- list(
    index = 1,
    title = "Mechanisms",
    summary = "How it works",
    subsections = list(list(title = "Setup", bullets = "Fact A")),
    facts_text = paste0("- Fact [", source_id, "]"),
    workspace = workspace,
    evidence = evidence,
    verified_evidence = evidence,
    min_support_score = 0.7
  )

  result <- tempest:::tempest_run_section_job(
    job,
    writer,
    module = test_program_executions()$section_writing
  )

  expect_equal(result$title, "Mechanisms")
  expect_equal(result$section_text, paste0("Section body [", source_id, "]."))
  expect_equal(
    result$markdown,
    paste0("## Mechanisms\n\nSection body [", source_id, "].")
  )
})

test_that("section prompts reject vector subsection titles", {
  job <- list(
    index = 1,
    title = "Mechanisms",
    summary = "How it works",
    subsections = list(list(
      title = c("Setup", "Details"),
      bullets = c("Fact A", "Fact B")
    )),
    facts_text = "- Fact [S123456789abc]"
  )

  expect_error(
    tempest:::tempest_run_section_job(
      job,
      writer = list(),
      module = test_program_executions()$section_writing
    ),
    class = "tempest_stage_output_error"
  )
})

test_that("tempest_write_sections_sequential preserves job order", {
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(
      module,
      chat,
      inputs,
      step
    ) {
      body <- if (identical(inputs$section_title, "First")) {
        paste0("First body [", source_id, "].")
      } else {
        paste0("Second body [", source_id, "].")
      }
      structure(
        list(output = list(section_text = body)),
        class = "dsprrr_result"
      )
    }
  )
  evidence <- list(
    tempest_claim(
      "First body",
      source_ids = source_id,
      verification_status = "supported",
      support_score = 0.9
    ),
    tempest_claim(
      "Second body",
      source_ids = source_id,
      verification_status = "supported",
      support_score = 0.9
    )
  )
  lapply(evidence, workspace$add_proposed_claim)
  evidence <- fake_verify_claim_supports(workspace, evidence)
  jobs <- list(
    list(
      index = 1,
      title = "First",
      summary = "",
      subsections = list(list(title = "Details", bullets = "Fact")),
      facts_text = "Facts",
      workspace = workspace,
      evidence = evidence[1],
      verified_evidence = evidence[1],
      min_support_score = 0.7
    ),
    list(
      index = 2,
      title = "Second",
      summary = "",
      subsections = list(list(title = "Details", bullets = "Fact")),
      facts_text = "Facts",
      workspace = workspace,
      evidence = evidence[2],
      verified_evidence = evidence[2],
      min_support_score = 0.7
    )
  )

  results <- tempest:::tempest_write_sections_sequential(
    jobs,
    writer = list(),
    programs = test_program_executions()
  )

  expect_equal(
    vapply(results, function(result) result$title, character(1)),
    c("First", "Second")
  )
  expect_equal(
    vapply(results, function(result) result$section_text, character(1)),
    c(
      paste0("First body [", source_id, "]."),
      paste0("Second body [", source_id, "].")
    )
  )
})

test_that("tempest_parallel_workers respects option and item cap", {
  withr::local_options(tempest.parallel_workers = 4)
  expect_identical(tempest:::tempest_parallel_workers(), 4L)
  expect_identical(tempest:::tempest_parallel_workers(2), 2L)
  expect_identical(tempest:::tempest_parallel_workers(10), 4L)
})

test_that("tempest_parallel_workers defaults to a positive integer", {
  withr::local_options(tempest.parallel_workers = NULL)
  workers <- tempest:::tempest_parallel_workers()
  expect_type(workers, "integer")
  expect_gte(workers, 1L)
})

test_that("tempest_collect_parallel preserves explicit result envelopes", {
  success <- list(
    ok = TRUE,
    value = list(a = 1),
    error = NULL,
    records = list()
  )
  expect_identical(tempest:::tempest_collect_parallel(success), success)

  failure <- tempest:::tempest_collect_parallel(simpleError("boom"))
  expect_identical(failure$ok, FALSE)
  expect_null(failure$value)
  expect_s3_class(failure$error, "simpleError")
  expect_length(failure$records, 0L)

  err <- structure(
    list(message = "x"),
    class = c("miraiError", "errorValue", "try-error")
  )
  envelope <- tempest:::tempest_collect_parallel(err)
  expect_identical(envelope$ok, FALSE)
  expect_s3_class(envelope$error, "tempest_parallel_worker_error")
})

test_that("parallel section writing reports an unavailable transport", {
  local_mocked_bindings(tempest_has = function(pkg) FALSE)
  jobs <- list(
    list(index = 1, title = "First", summary = "", subsections = list()),
    list(index = 2, title = "Second", summary = "", subsections = list())
  )
  results <- tempest:::tempest_write_sections_parallel(
    jobs,
    tempest_config(),
    programs = test_program_executions()
  )
  expect_null(results)
})

test_that("unavailable parallel transport runs each stage sequentially", {
  programs <- test_program_executions()
  workspace <- fake_store_with_sources(1)
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  local_mocked_bindings(tempest_has = function(pkg) FALSE)
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(...) {
      structure(
        list(output = list(section_text = paste0("Body [", source_id, "]."))),
        class = "dsprrr_result"
      )
    }
  )
  evidence <- list(tempest_claim(
    "Body",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  ))
  workspace$add_proposed_claim(evidence[[1]])
  evidence <- fake_verify_claim_supports(workspace, evidence)
  jobs <- list(
    list(
      index = 1,
      title = "First",
      summary = "",
      subsections = list(list(title = "Details", bullets = "Fact")),
      facts_text = "Facts",
      workspace = workspace,
      evidence = evidence,
      verified_evidence = evidence,
      min_support_score = 0.7
    ),
    list(
      index = 2,
      title = "Second",
      summary = "",
      subsections = list(list(title = "Details", bullets = "Fact")),
      facts_text = "Facts",
      workspace = workspace,
      evidence = evidence,
      verified_evidence = evidence,
      min_support_score = 0.7
    )
  )

  results <- tempest:::tempest_write_section_jobs(
    jobs,
    writer = list(),
    config = tempest_config(),
    programs = programs,
    parallel = TRUE
  )

  expect_equal(
    vapply(results, function(result) result$title, character(1)),
    c("First", "Second")
  )
})

test_that("parallel writing preserves failed and retry stage records", {
  programs <- test_program_executions()
  artifact_id <- programs$section_writing$program_artifact_id
  worker_error <- simpleError("worker provider failed")
  worker_running <- tempest:::tempest_stage_record_start(
    "section_writing",
    artifact_id,
    attempt_id = "worker-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )
  worker_failed <- tempest:::tempest_stage_record_fail(
    worker_running,
    error = worker_error,
    completed_at = "2026-08-16T01:00:01Z"
  )
  retry_running <- tempest:::tempest_stage_record_start(
    "section_writing",
    artifact_id,
    attempt_id = "retry-attempt",
    started_at = "2026-08-16T01:00:02Z"
  )
  retry_succeeded <- tempest:::tempest_stage_record_succeed(
    retry_running,
    tempest:::tempest_stage_content_reference("Retry body"),
    support_status = "verified",
    completed_at = "2026-08-16T01:00:03Z"
  )
  records <- list()
  local_mocked_bindings(
    tempest_write_sections_parallel = function(...) {
      list(
        list(
          ok = FALSE,
          value = NULL,
          error = worker_error,
          records = list(worker_failed)
        ),
        list(
          ok = TRUE,
          value = list(
            index = 2L,
            title = "Existing",
            section_text = "Existing body",
            markdown = "## Existing\n\nExisting body"
          ),
          error = NULL,
          records = list()
        )
      )
    },
    tempest_run_section_job = function(
      job,
      writer,
      module,
      verbose,
      record_stage
    ) {
      record_stage(retry_running)
      record_stage(retry_succeeded, "Retry body")
      list(
        index = job$index,
        title = job$title,
        section_text = "Retry body",
        markdown = paste0("## ", job$title, "\n\nRetry body")
      )
    }
  )

  result <- tempest:::tempest_write_section_jobs(
    jobs = list(
      list(index = 1L, title = "Retry"),
      list(index = 2L, title = "Existing")
    ),
    writer = list(),
    config = tempest_config(),
    programs = programs,
    parallel = TRUE,
    record_stage = function(record, output = NULL) {
      records <<- tempest:::tempest_stage_records_upsert(records, record)
      invisible(record)
    }
  )

  expect_equal(result[[1]]$section_text, "Retry body")
  expect_equal(
    vapply(records, \(record) record@status, character(1)),
    c("failed", "succeeded")
  )
  expect_equal(
    vapply(records, \(record) record@attempt_id, character(1)),
    c("worker-attempt", "retry-attempt")
  )
})

test_that("parallel writing never retries static stage errors", {
  programs <- test_program_executions()
  artifact_id <- programs$section_writing$program_artifact_id
  static_error <- rlang::error_cnd(
    "tempest_stage_governance_error",
    message = "governance mismatch"
  )
  running <- tempest:::tempest_stage_record_start(
    "section_writing",
    artifact_id,
    attempt_id = "static-attempt",
    started_at = "2026-08-16T01:00:00Z"
  )
  failed <- tempest:::tempest_stage_record_fail(
    running,
    error = static_error,
    completed_at = "2026-08-16T01:00:01Z"
  )
  retry_calls <- 0L
  records <- list()
  local_mocked_bindings(
    tempest_write_sections_parallel = function(...) {
      list(
        list(
          ok = FALSE,
          value = NULL,
          error = static_error,
          records = list(failed)
        ),
        list(
          ok = TRUE,
          value = list(
            index = 2L,
            title = "Existing",
            section_text = "Existing body",
            markdown = "## Existing\n\nExisting body"
          ),
          error = NULL,
          records = list()
        )
      )
    },
    tempest_run_section_job = function(...) {
      retry_calls <<- retry_calls + 1L
      stop("unexpected retry")
    }
  )

  expect_error(
    tempest:::tempest_write_section_jobs(
      jobs = list(
        list(index = 1L, title = "Static"),
        list(index = 2L, title = "Existing")
      ),
      writer = list(),
      config = tempest_config(),
      programs = programs,
      parallel = TRUE,
      record_stage = function(record, output = NULL) {
        records <<- tempest:::tempest_stage_records_upsert(records, record)
        invisible(record)
      }
    ),
    class = "tempest_stage_governance_error"
  )
  expect_identical(retry_calls, 0L)
  expect_identical(records[[1]]@attempt_id, "static-attempt")
  expect_identical(records[[1]]@status, "failed")
})
