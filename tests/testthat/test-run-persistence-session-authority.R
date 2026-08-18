test_that("schema 9 persists exact Deputy execution authority", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")

  moderator_source <- fake_source(
    "https://example.org/schema-9-moderator",
    content_text = "Moderator evidence is durable."
  )
  expert_source <- fake_source(
    "https://example.org/schema-9-expert",
    content_text = "Expert evidence is durable."
  )
  extractions <- list(
    list(
      facts = list(list(
        claim = "Moderator evidence is durable.",
        sources = list(list(source_id = moderator_source$id)),
        confidence = "high"
      ))
    ),
    list(
      facts = list(list(
        claim = "Expert evidence is durable.",
        sources = list(list(source_id = expert_source$id)),
        confidence = "high"
      ))
    )
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        return(fake_chat(structured = extractions))
      }
      if (identical(role, "coordinator")) {
        return(fake_chat(
          text = list(paste0(
            "Moderator evidence is durable [",
            moderator_source$id,
            "]."
          ))
        ))
      }
      if (identical(role, "expert")) {
        return(fake_chat(
          text = list(paste0(
            "Expert evidence is durable [",
            expert_source$id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )
  expert <- test_expert(expert_id = "expert.schema-9-a")
  other_expert <- test_expert(expert_id = "expert.schema-9-b")
  session <- tempest_session(
    "Schema 9 Deputy authority",
    config = cfg,
    experts = list(expert, other_expert),
    session_id = "schema-9-deputy-authority"
  )
  session$workspace$upsert_retrieved_resource(moderator_source)
  session$workspace$upsert_retrieved_resource(expert_source)
  expert_session <- tempest:::tempest_session_expert_manager(
    session
  )$get_or_create(
    expert@expert_id
  )
  other_expert_session <- tempest:::tempest_session_expert_manager(
    session
  )$get_or_create(
    other_expert@expert_id
  )
  make_trace <- function(
    target,
    run_id,
    deputy_session_id,
    role,
    correlation_id,
    expert_id = NULL,
    status = "complete",
    stage = "dialogue"
  ) {
    context <- tempest:::tempest_deputy_run_context(
      target$manifest,
      stage = "dialogue",
      role = role,
      expert_id = expert_id
    )
    trace <- list(
      agent_id = tempest:::tempest_deputy_adapter_agent_id(context),
      correlation_id = correlation_id,
      deputy_run_id = run_id,
      deputy_session_id = deputy_session_id
    )
    if (!is.null(expert_id)) {
      trace$expert_id <- expert_id
    }
    trace$role <- role
    trace$stage <- stage
    trace$status <- status
    trace$completion_disposition <- if (identical(status, "complete")) {
      "issued"
    } else {
      "terminal"
    }
    trace$trace_id <- run_id
    trace$trace_type <- "deputy_run"
    trace
  }
  moderator_completion_id <- tempest:::tempest_costorm_await(
    session$request_completion_async("Record moderator evidence.")
  )
  moderator_result <- withCallingHandlers(
    tempest:::tempest_costorm_await(tempest_session_process_turn_async(
      session,
      moderator_completion_id,
      suggest = FALSE,
      n_suggestions = 4L,
      is_current = function() TRUE
    )),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  manager <- tempest:::tempest_session_expert_manager(session)
  expert_completion_id <- manager$request_completion(
    expert@expert_id,
    "Record expert evidence.",
    stage = "dialogue",
    correlation_id = "turn-schema-9-expert"
  )
  expert_result <- withCallingHandlers(
    manager$commit_completion(
      expert_completion_id,
      expert@expert_id,
      stage = "dialogue",
      is_current = function() TRUE
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  deputy_traces <- tempest:::tempest_session_deputy_traces(session)
  moderator_trace <- deputy_traces[[match(
    moderator_result@deputy_run_id,
    vapply(deputy_traces, `[[`, character(1), "deputy_run_id")
  )]]
  expert_trace <- expert_result$deputy_execution

  snapshot <- tempest_session_snapshot(session)
  trace_types <- vapply(
    snapshot$research_manifest$traces,
    `[[`,
    character(1),
    "trace_type"
  )
  expect_identical(snapshot$schema_version, 9L)
  expect_identical(
    trace_types,
    c("stage_attempt", "stage_attempt", "deputy_run", "deputy_run")
  )
  expect_identical(
    tail(snapshot$research_manifest$traces, 2L),
    deputy_traces
  )
  expect_identical(
    snapshot$research_manifest$runtime,
    list(
      deputy_run_ids = as.list(sort(c(
        moderator_trace$deputy_run_id,
        expert_trace$deputy_run_id
      ))),
      deputy_session_ids = as.list(sort(c(
        moderator_trace$deputy_session_id,
        expert_trace$deputy_session_id
      )))
    )
  )
  expect_identical(
    snapshot$research_manifest$traces[[1L]]$expert_id,
    "moderator"
  )
  expect_null(moderator_trace$expert_id)

  contains_runtime_object <- function(value) {
    if (
      inherits(
        value,
        c(
          "Agent",
          "TempestDeputyChatAdapter",
          "TempestRuntime",
          "R6"
        )
      )
    ) {
      return(TRUE)
    }
    if (!is.list(value)) {
      return(FALSE)
    }
    any(vapply(value, contains_runtime_object, logical(1)))
  }
  expect_identical(contains_runtime_object(snapshot), FALSE)

  restored <- tempest_session_restore(snapshot, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(restored),
    deputy_traces
  )
  bundle_dir <- file.path(withr::local_tempdir(), "schema-9-deputy")
  tempest_session_save(session, bundle_dir)
  resumed <- tempest_session_resume(bundle_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(resumed),
    deputy_traces
  )
  bundle_files <- list.files(
    bundle_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  bundle_text <- paste(
    vapply(
      bundle_files,
      function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )
  expect_no_match(
    bundle_text,
    "Agent|TempestDeputyChatAdapter|TempestRuntime|R6"
  )

  continued_moderator <- make_trace(
    resumed,
    "deputy-run-a-continued-moderator",
    moderator_trace$deputy_session_id,
    "moderator",
    "turn-schema-9-continued-moderator"
  )
  continued_expert <- make_trace(
    resumed,
    "deputy-run-e-continued-expert",
    expert_trace$deputy_session_id,
    "expert",
    "turn-schema-9-continued-expert",
    expert_id = expert@expert_id
  )
  tempest:::tempest_session_record_deputy_trace(
    resumed,
    continued_moderator
  )
  tempest:::tempest_session_record_deputy_trace(resumed, continued_expert)
  continued_snapshot <- tempest_session_snapshot(resumed)
  expect_length(
    Filter(
      \(trace) identical(trace$trace_type, "deputy_run"),
      continued_snapshot$research_manifest$traces
    ),
    4L
  )
  continued_dir <- file.path(dirname(bundle_dir), "schema-9-continued")
  tempest_session_save(resumed, continued_dir)
  continued <- tempest_session_resume(continued_dir, config = cfg)
  expect_identical(
    tempest:::tempest_session_deputy_traces(continued),
    tempest:::tempest_session_deputy_traces(resumed)
  )

  historical_session <- tempest_session(
    "Retired Deputy history",
    config = cfg,
    experts = list(expert),
    session_id = "schema-9-retired-history"
  )
  retired_binding <- tempest:::tempest_session_expert_manager(
    historical_session
  )$get_or_create(
    expert@expert_id
  )
  retired_trace <- make_trace(
    historical_session,
    "deputy-run-retired-expert",
    retired_binding$session_id,
    "expert",
    "turn-schema-9-retired-expert",
    expert_id = expert@expert_id,
    status = "interrupted",
    stage = "warmup"
  )
  tempest:::tempest_session_record_deputy_trace(
    historical_session,
    retired_trace
  )
  tempest:::tempest_session_expert_manager(historical_session)$retire_session(
    retired_binding$session_id
  )
  expect_no_error(tempest_session_snapshot(historical_session))
  historical_dir <- file.path(dirname(bundle_dir), "schema-9-historical")
  tempest_session_save(historical_session, historical_dir)
  historical <- tempest_session_resume(historical_dir, config = cfg)
  replacement_binding <- tempest:::tempest_session_expert_manager(
    historical
  )$get_or_create(
    expert@expert_id
  )
  expect_identical(
    identical(
      replacement_binding$session_id,
      retired_binding$session_id
    ),
    FALSE
  )
  replacement_trace <- make_trace(
    historical,
    "deputy-run-replacement-expert",
    replacement_binding$session_id,
    "expert",
    "turn-schema-9-replacement-expert",
    expert_id = expert@expert_id
  )
  tempest:::tempest_session_record_deputy_trace(
    historical,
    replacement_trace
  )
  expect_no_error(tempest_session_snapshot(historical))
  historical_continued_dir <- file.path(
    dirname(bundle_dir),
    "schema-9-historical-continued"
  )
  tempest_session_save(historical, historical_continued_dir)
  historical_continued <- tempest_session_resume(
    historical_continued_dir,
    config = cfg
  )
  expect_identical(
    tempest:::tempest_session_deputy_traces(historical_continued),
    tempest:::tempest_session_deputy_traces(historical)
  )

  expect_rejected <- function(candidate) {
    expect_error(
      tempest_session_restore(candidate, config = cfg),
      class = "tempest_session_restore_error"
    )
  }
  stage_indexes <- which(trace_types == "stage_attempt")
  moderator_index <- which(vapply(
    snapshot$research_manifest$traces,
    \(trace) {
      identical(trace$trace_type, "deputy_run") &&
        identical(trace$role, "moderator")
    },
    logical(1)
  ))
  expert_index <- which(vapply(
    snapshot$research_manifest$traces,
    \(trace) {
      identical(trace$trace_type, "deputy_run") &&
        identical(trace$role, "expert")
    },
    logical(1)
  ))

  unknown_trace <- rlang::duplicate(snapshot, shallow = FALSE)
  unknown_trace$research_manifest$traces[[expert_index]]$trace_type <-
    "unknown_run"
  expect_rejected(unknown_trace)

  duplicate_trace <- rlang::duplicate(snapshot, shallow = FALSE)
  duplicate_trace$research_manifest$traces <- c(
    duplicate_trace$research_manifest$traces,
    list(duplicate_trace$research_manifest$traces[[expert_index]])
  )
  expect_rejected(duplicate_trace)

  reordered_traces <- rlang::duplicate(snapshot, shallow = FALSE)
  reordered_traces$research_manifest$traces <-
    rev(reordered_traces$research_manifest$traces)
  expect_rejected(reordered_traces)

  missing_run <- rlang::duplicate(snapshot, shallow = FALSE)
  missing_run$research_manifest$runtime$deputy_run_ids <-
    missing_run$research_manifest$runtime$deputy_run_ids[-1L]
  expect_rejected(missing_run)

  extra_session <- rlang::duplicate(snapshot, shallow = FALSE)
  extra_session$research_manifest$runtime$deputy_session_ids <- as.list(
    sort(c(
      unlist(
        extra_session$research_manifest$runtime$deputy_session_ids,
        use.names = FALSE
      ),
      "expert-session_ffffffffffffffff"
    ))
  )
  expect_rejected(extra_session)

  missing_terminal <- rlang::duplicate(snapshot, shallow = FALSE)
  missing_terminal$research_manifest$traces <-
    missing_terminal$research_manifest$traces[-expert_index]
  expect_rejected(missing_terminal)

  failed_terminal <- rlang::duplicate(snapshot, shallow = FALSE)
  failed_terminal$research_manifest$traces[[expert_index]]$status <-
    "provider_error"
  expect_rejected(failed_terminal)

  missing_disposition <- rlang::duplicate(snapshot, shallow = FALSE)
  missing_disposition$research_manifest$traces[[
    expert_index
  ]]$completion_disposition <- NULL
  expect_rejected(missing_disposition)

  discarded_completion <- rlang::duplicate(snapshot, shallow = FALSE)
  discarded_completion$research_manifest$traces[[
    expert_index
  ]]$completion_disposition <- "discarded"
  expect_rejected(discarded_completion)

  changed_correlation <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_correlation$research_manifest$traces[[expert_index]]$correlation_id <-
    "turn-schema-9-changed"
  expect_rejected(changed_correlation)

  changed_expert <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_expert$research_manifest$traces[[expert_index]]$expert_id <-
    other_expert@expert_id
  other_context <- tempest:::tempest_deputy_run_context(
    session$manifest,
    stage = "dialogue",
    role = "expert",
    expert_id = other_expert@expert_id
  )
  changed_expert$research_manifest$traces[[expert_index]]$agent_id <-
    tempest:::tempest_deputy_adapter_agent_id(other_context)
  expect_rejected(changed_expert)

  changed_expert_session <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_expert_session$research_manifest$traces[[
    expert_index
  ]]$deputy_session_id <-
    other_expert_session$session_id
  changed_expert_session$research_manifest$runtime$deputy_session_ids <-
    as.list(sort(c(
      moderator_trace$deputy_session_id,
      other_expert_session$session_id
    )))
  expect_rejected(changed_expert_session)

  changed_moderator_session <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_moderator_session$research_manifest$traces[[
    moderator_index
  ]]$deputy_session_id <-
    "tempest-moderator-000000000000000000000000"
  changed_moderator_session$research_manifest$runtime$deputy_session_ids <-
    as.list(sort(c(
      "tempest-moderator-000000000000000000000000",
      expert_trace$deputy_session_id
    )))
  expect_rejected(changed_moderator_session)

  changed_agent <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_agent$research_manifest$traces[[moderator_index]]$agent_id <-
    "forged-moderator-agent"
  expect_rejected(changed_agent)

  changed_trace_id <- rlang::duplicate(snapshot, shallow = FALSE)
  changed_trace_id$research_manifest$traces[[expert_index]]$trace_id <-
    "forged-trace-id"
  expect_rejected(changed_trace_id)

  credential_trace <- rlang::duplicate(snapshot, shallow = FALSE)
  credential_trace$research_manifest$traces[[expert_index]]$correlation_id <-
    "sk-proj-0123456789abcdefghijklmnopqrstuv"
  expect_rejected(credential_trace)

  runtime_values <- list(
    deputy::Agent$new(chat = fake_chat()),
    new.env(parent = emptyenv()),
    function() NULL
  )
  for (runtime_value in runtime_values) {
    runtime_snapshot <- rlang::duplicate(snapshot, shallow = FALSE)
    runtime_snapshot$research_manifest$traces[[
      moderator_index
    ]]$runtime_object <-
      runtime_value
    expect_rejected(runtime_snapshot)
  }

  expect_length(stage_indexes, 2L)
})
