test_that("default dsprrr STORM semantic outcomes are frozen", {
  local_otel_opt_in()
  state <- local_fake_otel()
  local_mocked_bindings(
    tempest_now_utc = function() "2026-08-19T00:00:00.000000Z",
    tempest_deputy_chat_adapter = function(
      chat,
      manifest,
      deputy_session_id,
      agent_id = NULL,
      agent_name = NULL,
      stage,
      role,
      expert_id = NULL,
      completion_registry = NULL,
      on_start = function(pending_run) invisible(pending_run),
      on_run = function(trace) invisible(trace),
      on_completion = function(completion) invisible(completion),
      on_terminal = function(terminal) invisible(terminal),
      ...
    ) {
      run_context <- tempest:::tempest_deputy_run_context(
        manifest,
        stage = stage,
        role = role,
        expert_id = expert_id
      )
      agent_id <- agent_id %||%
        tempest:::tempest_deputy_adapter_agent_id(run_context)
      identity <- digest::digest(
        tempest:::tempest_research_manifest_canonical_json(run_context),
        algo = "sha256",
        serialize = FALSE
      )
      run_id <- paste0("deputy-baseline-", substr(identity, 1L, 24L))
      correlation_id <- paste0(
        "correlation-baseline-",
        substr(identity, 1L, 24L)
      )
      adapter <- structure(
        list(
          chat = function(prompt, echo = "none", ...) {
            completion_id <- tempest:::tempest_agent_completion_new_id(
              completion_registry
            )
            response <- chat$chat(prompt, echo = echo)
            provider_turn <- chat$last_turn(role = "assistant")
            trace <- tempest:::tempest_agent_completion_trace(list(
              agent_id = agent_id,
              completion_disposition = "issued",
              correlation_id = correlation_id,
              deputy_run_id = run_id,
              deputy_session_id = deputy_session_id,
              expert_id = expert_id,
              role = role,
              stage = stage,
              status = "complete",
              trace_id = run_id,
              trace_type = "deputy_run"
            ))
            on_run(trace)
            on_completion(list(
              completion_id = completion_id,
              prompt = prompt,
              response = response,
              provider_turn = provider_turn,
              deputy_execution = trace
            ))
            tempest:::tempest_agent_completion_tag(response, completion_id)
          }
        ),
        class = c("TempestDeputyChatAdapter", "Chat", "list")
      )
      adapter
    }
  )
  result_record <- function(result) {
    list(
      title = result@title,
      perspectives = result@perspectives,
      experts = tempest:::tempest_expert_records(result@experts),
      outline = result@outline,
      draft_md = result@draft_md,
      report_md = result@report_md,
      manifest = tempest:::tempest_research_manifest_record(result@manifest)
    )
  }
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
  baseline_local_ids()
  disabled <- storm_product_baseline_fixture()
  semantics <- baseline_storm_semantics(disabled)
  definition_ids <- vapply(
    semantics$citations$definitions,
    `[[`,
    character(1),
    "citation_id"
  )

  expect_identical(semantics$citations$uses, semantics$source_ids)
  expect_identical(definition_ids, semantics$source_ids)
  expect_setequal(
    intersect(
      names(tempest:::TempestResult@properties),
      c("store", "artifact_catalog", "workflow_run")
    ),
    character()
  )
  expect_identical(disabled$result@workspace, disabled$store)
  expect_identical(
    disabled$result@manifest@research_run_id,
    "storm-product-baseline"
  )
  expect_identical(disabled$result@manifest@status, "succeeded")
  expect_snapshot(baseline_snapshot_json(semantics))

  options(tempest.otel.enabled = TRUE)
  baseline_local_ids()
  enabled <- storm_product_baseline_fixture()

  expect_identical(
    serialize(result_record(enabled$result), NULL),
    serialize(result_record(disabled$result), NULL)
  )
  expect_identical(
    serialize(tempest:::tempest_storm_state_record(enabled$result@state), NULL),
    serialize(tempest:::tempest_storm_state_record(disabled$result@state), NULL)
  )
  expect_identical(
    serialize(
      tempest:::tempest_research_workspace_snapshot(enabled$result@workspace),
      NULL
    ),
    serialize(
      tempest:::tempest_research_workspace_snapshot(disabled$result@workspace),
      NULL
    )
  )
  expect_identical(enabled$result@report_md, disabled$result@report_md)
  expect_identical(
    bundle_bytes(enabled$result@output_dir),
    bundle_bytes(disabled$result@output_dir)
  )
  expect_gt(length(state$spans), 0L)
})
