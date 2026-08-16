test_that("async warmup returns a typed result and commits in expert order", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  collector <- tempest_progress_collector(include_payload = TRUE)
  experts <- list(
    test_expert(expert_id = "expert.a", name = "Dr. A", title = "Expert"),
    test_expert(expert_id = "expert.b", name = "Dr. B", title = "Expert")
  )
  session <- fake_costorm_warmup_session(
    experts = experts,
    progress = collector$record
  )

  settled <- await_tempest_promise(tempest_session_warmup_async(
    session,
    timeout_s = 1,
    max_parallel_experts = 2
  ))

  expect_null(settled$error)
  expect_s7_class(settled$value, TempestWarmupResult)
  expect_identical(settled$value@status, "succeeded")
  expect_equal(settled$value@expert_count, 2L)
  expect_equal(settled$value@orientation_count, 2L)
  expect_equal(settled$value@failure_count, 0L)
  expect_equal(settled$value@evidence_failure_count, 0L)
  expect_equal(session$state$map_updates, 1L)
  expect_equal(
    vapply(session$state$turns, \(turn) turn$speaker, character(1)),
    c("Dr. A", "Dr. B")
  )
  labels <- vapply(
    collector$data(),
    function(event) {
      paste(event$event_type, event$stage, event$status, sep = ":")
    },
    character(1)
  )
  expect_contains(
    labels,
    c(
      "stage:warmup:started",
      "expert:warmup:started",
      "expert:warmup:succeeded",
      "stage:warmup:succeeded"
    )
  )
})

test_that("async warmup starts independent experts in parallel", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  first_resolve <- NULL
  second_started <- FALSE
  experts <- list(
    test_expert(expert_id = "expert.a", name = "Dr. A", title = "Expert"),
    test_expert(expert_id = "expert.b", name = "Dr. B", title = "Expert")
  )
  session <- fake_costorm_warmup_session(
    experts = experts,
    chat_async = function(prompt, expert) {
      if (identical(expert@expert_id, "expert.a")) {
        return(promises::promise(function(resolve, reject) {
          first_resolve <<- resolve
        }))
      }
      second_started <<- TRUE
      source_id <- session$workspace$list_retrieved_sources()[[1]]$id
      promises::promise_resolve(paste0("Second [", source_id, "]."))
    }
  )

  request <- tempest_session_warmup_async(
    session,
    timeout_s = 1,
    max_parallel_experts = 2
  )
  deadline <- Sys.time() + 1
  while ((is.null(first_resolve) || !second_started) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  expect_equal(is.null(first_resolve), FALSE)
  expect_equal(second_started, TRUE)
  source_id <- session$workspace$list_retrieved_sources()[[1]]$id
  first_resolve(paste0("First [", source_id, "]."))
  settled <- await_tempest_promise(request)
  expect_null(settled$error)
  expect_equal(settled$value@orientation_count, 2L)
})

test_that("async warmup times out one expert and ignores its late result", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  late_resolve <- NULL
  collector <- tempest_progress_collector(include_payload = TRUE)
  session <- fake_costorm_warmup_session(
    chat_async = function(prompt) {
      promises::promise(function(resolve, reject) {
        late_resolve <<- resolve
      })
    },
    progress = collector$record
  )

  settled <- await_tempest_promise(tempest_session_warmup_async(
    session,
    timeout_s = 0.02
  ))

  expect_null(settled$error)
  expect_identical(settled$value@status, "succeeded")
  expect_equal(settled$value@failure_count, 1L)
  expect_identical(settled$value@orientations[[1]]$failure_kind, "timeout")
  expect_identical(settled$value@orientations[[1]]$session_retired, TRUE)
  expect_equal(session$state$retired, 1L)
  expect_length(session$state$turns, 0L)
  late_resolve("Late response")
  later::run_now(0.05)
  expect_length(session$state$turns, 0L)
  timeout_events <- Filter(
    function(event) identical(event@payload$failure_kind, "timeout"),
    collector$events()
  )
  expect_length(timeout_events, 1L)
})

test_that("stale async warmup suppresses late mutations and events", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  resolve_chat <- NULL
  current <- TRUE
  collector <- tempest_progress_collector(include_payload = TRUE)
  session <- fake_costorm_warmup_session(
    chat_async = function(prompt) {
      promises::promise(function(resolve, reject) {
        resolve_chat <<- resolve
      })
    },
    progress = collector$record
  )
  request <- tempest_session_warmup_async(
    session,
    timeout_s = 1,
    is_current = function() current
  )
  deadline <- Sys.time() + 1
  while (is.null(resolve_chat) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  before <- length(collector$events())

  current <- FALSE
  resolve_chat("Late response")
  settled <- await_tempest_promise(request)

  expect_null(settled$error)
  expect_identical(settled$value@status, "cancelled")
  expect_length(session$state$turns, 0L)
  expect_equal(session$state$map_updates, 0L)
  expect_equal(session$state$retired, 1L)
  expect_length(collector$events(), before)
})

test_that("async warmup records evidence failures without failing the panel", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  session <- fake_costorm_warmup_session(
    extractor_async = function(...) {
      promises::promise_reject(simpleError("extractor unavailable"))
    }
  )

  settled <- await_tempest_promise(tempest_session_warmup_async(
    session,
    timeout_s = 1
  ))

  expect_null(settled$error)
  expect_identical(settled$value@status, "succeeded")
  expect_equal(settled$value@orientation_count, 1L)
  expect_equal(settled$value@evidence_failure_count, 1L)
  expect_identical(
    settled$value@orientations[[1]]$evidence_status,
    "failed"
  )
  expect_equal(session$state$map_updates, 1L)
})

test_that("async warmup defaults remain bounded and host-neutral", {
  defaults <- formals(tempest_session_warmup_async)
  withr::local_options(
    tempest.costorm.warmup_timeout_s = NULL,
    tempest.costorm.warmup_max_parallel_experts = NULL
  )
  expert <- test_expert(
    expert_id = "expert.prompt",
    name = "Dr. Prompt",
    title = "Expert",
    initial_questions = "What should the panel investigate?"
  )
  prompt <- tempest:::tempest_warmup_prompt("Test topic", expert)

  expect_equal(eval(defaults$timeout_s), 120)
  expect_equal(eval(defaults$max_parallel_experts), 3L)
  expect_match(prompt, "make exactly one web search", fixed = TRUE)
  expect_match(prompt, "do not call add_proposed_claim", fixed = TRUE)
  expect_match(prompt, "What should the panel investigate?", fixed = TRUE)
  expect_match(prompt, "no more than 250 words", fixed = TRUE)
  expect_no_match(
    paste(deparse(body(tempest_session_warmup_async)), collapse = "\n"),
    "shiny"
  )
})

test_that("synchronous warmup enrichment failures remain best effort", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  evidence_session <- fake_costorm_warmup_session(
    extractor_async = function(...) stop("synchronous extractor failure")
  )
  evidence <- await_tempest_promise(tempest_session_warmup_async(
    evidence_session,
    timeout_s = 1
  ))

  expect_null(evidence$error)
  expect_identical(evidence$value@status, "succeeded")
  expect_equal(evidence$value@evidence_failure_count, 1L)
  expect_identical(
    evidence$value@orientations[[1]]$evidence_status,
    "failed"
  )

  mindmap_session <- fake_costorm_warmup_session(
    mindmap_async = function(...) stop("synchronous mind-map failure")
  )
  mindmap <- await_tempest_promise(tempest_session_warmup_async(
    mindmap_session,
    timeout_s = 1
  ))

  expect_null(mindmap$error)
  expect_identical(mindmap$value@status, "succeeded")
  expect_identical(mindmap$value@mindmap_updated, FALSE)
})

test_that("stale warmup evidence and mind-map settlements stay silent", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  resolve_evidence <- NULL
  evidence_current <- TRUE
  evidence_events <- tempest_progress_collector(include_payload = TRUE)
  evidence_session <- fake_costorm_warmup_session(
    extractor_async = function(...) {
      promises::promise(function(resolve, reject) {
        resolve_evidence <<- resolve
      })
    },
    progress = evidence_events$record
  )
  evidence_request <- tempest_session_warmup_async(
    evidence_session,
    timeout_s = 1,
    is_current = function() evidence_current
  )
  deadline <- Sys.time() + 1
  while (is.null(resolve_evidence) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  evidence_before <- length(evidence_events$events())
  evidence_current <- FALSE
  resolve_evidence(list(facts = list()))
  evidence <- await_tempest_promise(evidence_request)

  expect_null(evidence$error)
  expect_identical(evidence$value@status, "cancelled")
  expect_length(evidence_events$events(), evidence_before)

  resolve_mindmap <- NULL
  mindmap_current <- TRUE
  mindmap_events <- tempest_progress_collector(include_payload = TRUE)
  mindmap_session <- fake_costorm_warmup_session(
    mindmap_async = function(...) {
      promises::promise(function(resolve, reject) {
        resolve_mindmap <<- resolve
      })
    },
    progress = mindmap_events$record
  )
  mindmap_request <- tempest_session_warmup_async(
    mindmap_session,
    timeout_s = 1,
    is_current = function() mindmap_current
  )
  deadline <- Sys.time() + 1
  while (is.null(resolve_mindmap) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  mindmap_before <- length(mindmap_events$events())
  mindmap_current <- FALSE
  resolve_mindmap(list(nodes = list(), edges = list()))
  mindmap <- await_tempest_promise(mindmap_request)

  expect_null(mindmap$error)
  expect_identical(mindmap$value@status, "cancelled")
  expect_length(mindmap_events$events(), mindmap_before)
})

test_that("warmup permits only one in-flight call per session", {
  skip_if_not_installed("later")
  skip_if_not_installed("promises")
  resolve_first <- NULL
  calls <- 0L
  collector <- tempest_progress_collector(include_payload = TRUE)
  session <- fake_costorm_warmup_session(
    chat_async = function(prompt) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(promises::promise(function(resolve, reject) {
          resolve_first <<- resolve
        }))
      }
      source_id <- session$workspace$list_retrieved_sources()[[1]]$id
      promises::promise_resolve(paste0("Later [", source_id, "]."))
    },
    progress = collector$record
  )
  first <- tempest_session_warmup_async(session, timeout_s = 1)
  deadline <- Sys.time() + 1
  while (is.null(resolve_first) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  before <- length(collector$events())
  busy <- tryCatch(
    tempest_session_warmup_async(session, timeout_s = 1),
    error = identity
  )

  expect_s3_class(busy, "tempest_warmup_busy")
  expect_length(collector$events(), before)

  source_id <- session$workspace$list_retrieved_sources()[[1]]$id
  resolve_first(paste0("First [", source_id, "]."))
  settled_first <- await_tempest_promise(first)
  expect_null(settled_first$error)

  settled_later <- await_tempest_promise(tempest_session_warmup_async(
    session,
    timeout_s = 1
  ))
  expect_null(settled_later$error)
  expect_identical(settled_later$value@status, "succeeded")
})

test_that("warmup result validators reject contradictory records", {
  expert <- test_expert(
    expert_id = "expert.validation",
    name = "Dr. Validation"
  )
  record <- tempest:::tempest_warmup_orientation(expert, "warmup-validation")
  record$status <- "succeeded"
  record$evidence_status <- "skipped"

  invalid_record <- record
  invalid_record$failure_kind <- "timeout"
  record_error <- tryCatch(
    tempest:::TempestWarmupResult(
      session_id = "validation-session",
      status = "succeeded",
      expert_count = 1L,
      orientation_count = 1L,
      failure_count = 0L,
      evidence_failure_count = 0L,
      source_count = 0L,
      claim_count = 0L,
      mindmap_updated = FALSE,
      orientations = list(invalid_record)
    ),
    error = identity
  )
  expect_s3_class(record_error, "error")
  expect_match(conditionMessage(record_error), "failure_kind", fixed = TRUE)

  count_error <- tryCatch(
    tempest:::TempestWarmupResult(
      session_id = "validation-session",
      status = "succeeded",
      expert_count = 1L,
      orientation_count = 0L,
      failure_count = 0L,
      evidence_failure_count = 0L,
      source_count = 0L,
      claim_count = 0L,
      mindmap_updated = FALSE,
      orientations = list(record)
    ),
    error = identity
  )
  expect_s3_class(count_error, "error")
  expect_match(conditionMessage(count_error), "orientation_count")
})
