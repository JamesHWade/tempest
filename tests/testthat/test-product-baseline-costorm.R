test_that("Co-STORM warmup and one moderator turn are frozen", {
  local_otel_opt_in()
  otel <- local_fake_otel()
  deputy_adapter_factory <- tempest_deputy_chat_adapter
  deputy_run_ids <- new.env(parent = emptyenv())
  local_mocked_bindings(
    tempest_now_utc = function() "2026-08-20T00:00:00.000000Z",
    tempest_agent_completion_registry = function(owner) {
      registry <- new.env(parent = emptyenv())
      registry$owner <- owner
      registry$entries <- new.env(hash = TRUE, parent = emptyenv())
      registry$counter <- 0L
      registry$registry_id <- "costormbaseline0000000000"
      class(registry) <- c(
        "TempestAgentCompletionRegistry",
        "environment"
      )
      registry
    },
    tempest_agent_completion_new_id = function(registry) {
      registry <- tempest_agent_completion_registry_validate(registry)
      registry$counter <- registry$counter + 1L
      paste0(
        tempest_agent_completion_id_prefix(registry),
        sprintf("%032x", registry$counter)
      )
    },
    tempest_deputy_chat_adapter = function(...) {
      adapter <- deputy_adapter_factory(...)
      private <- adapter$.__enclos_env__$private
      unlockBinding("new_run_id", private)
      private$new_run_id <- function() {
        deputy_run_ids$counter <- deputy_run_ids$counter + 1L
        paste0("run_costorm-baseline-", deputy_run_ids$counter)
      }
      lockBinding("new_run_id", private)
      adapter
    }
  )
  bundle_bytes <- function(path) {
    relative <- list.files(
      path,
      recursive = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    paths <- file.path(path, relative)
    relative <- relative[!file.info(paths)$isdir]
    paths <- file.path(path, relative)
    stats::setNames(
      lapply(paths, function(file) {
        readBin(file, what = "raw", n = file.size(file))
      }),
      relative
    )
  }

  options(tempest.otel.enabled = FALSE)
  deputy_run_ids$counter <- 0L
  baseline_local_ids()
  disabled <- costorm_product_baseline_fixture()
  disabled_snapshot <- tempest_session_snapshot(disabled$session)
  disabled_bundle <- file.path(withr::local_tempdir(), "telemetry-disabled")
  tempest_session_save(disabled$session, disabled_bundle)
  semantics <- baseline_costorm_semantics(disabled)
  definition_ids <- vapply(
    semantics$report_citations$definitions,
    `[[`,
    character(1),
    "citation_id"
  )

  expect_identical(semantics$report_citations$uses, semantics$source_ids)
  expect_identical(definition_ids, semantics$source_ids)
  expect_snapshot(baseline_snapshot_json(semantics))

  options(tempest.otel.enabled = TRUE)
  deputy_run_ids$counter <- 0L
  baseline_local_ids()
  enabled <- costorm_product_baseline_fixture()
  enabled_snapshot <- tempest_session_snapshot(enabled$session)
  enabled_bundle <- file.path(withr::local_tempdir(), "telemetry-enabled")
  tempest_session_save(enabled$session, enabled_bundle)

  expect_identical(
    serialize(baseline_costorm_semantics(enabled), NULL),
    serialize(semantics, NULL)
  )
  expect_identical(
    serialize(enabled_snapshot, NULL),
    serialize(disabled_snapshot, NULL)
  )
  expect_identical(enabled$report, disabled$report)
  expect_identical(bundle_bytes(enabled_bundle), bundle_bytes(disabled_bundle))
  expect_gt(length(otel$spans), 0L)

  progress_sequence <- function(span) {
    vapply(
      span$events,
      function(event) {
        attributes <- event$attributes
        paste(
          attributes[["tempest.event_type"]],
          attributes[["tempest.stage"]],
          attributes[["tempest.step"]],
          attributes[["tempest.status"]],
          sep = ":"
        )
      },
      character(1)
    )
  }
  warmup_spans <- Filter(
    \(span) identical(span$name, "tempest.costorm.warmup"),
    otel$spans
  )
  turn_spans <- Filter(
    \(span) identical(span$name, "tempest.costorm.turn.commit"),
    otel$spans
  )

  expect_length(warmup_spans, 1L)
  expect_identical(
    progress_sequence(warmup_spans[[1L]]),
    c(
      "stage:warmup:expert_fanout:started",
      "expert:warmup:expert_fanout:started",
      "step:evidence:fact_extraction:started",
      "step:evidence:fact_extraction:succeeded",
      "expert:warmup:expert_fanout:succeeded",
      "step:mindmap:update:started",
      "step:mindmap:update:succeeded",
      "stage:warmup:expert_fanout:succeeded"
    )
  )
  expect_length(turn_spans, 1L)
  expect_identical(
    progress_sequence(turn_spans[[1L]]),
    c(
      "stage:dialogue:turn:started",
      "step:dialogue:user_turn:succeeded",
      "step:dialogue:moderator_response:succeeded",
      "step:evidence:fact_extraction:started",
      "step:evidence:fact_extraction:succeeded",
      "step:mindmap:update:started",
      "step:mindmap:update:succeeded",
      "stage:dialogue:turn:succeeded"
    )
  )
})
