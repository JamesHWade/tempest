test_that("sync and async warmups persist authoritative claim provenance", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  make_session <- function(mode) {
    topic <- paste("Persisted warmup", mode)
    session_id <- paste0("persisted-warmup-", mode)
    expert <- test_expert(
      name = paste("Persisted Warmup", mode, "Expert"),
      initial_questions = "What evidence should orient the panel?"
    )
    source <- fake_source(paste0(
      "https://example.org/persisted-warmup-",
      mode
    ))
    answer <- paste0("Warmup evidence is durable [", source@resource_id, "].")
    extraction <- list(
      facts = list(list(
        claim = "Warmup evidence is durable.",
        sources = list(list(source_id = source@resource_id)),
        confidence = "high"
      ))
    )
    mindmap <- list(
      nodes = list(
        list(
          id = "root",
          label = topic,
          parent = NULL,
          notes = "",
          source_ids = character()
        ),
        list(
          id = "warmup-evidence",
          label = "Warmup evidence",
          parent = "root",
          notes = "Durable evidence",
          source_ids = source@resource_id
        )
      ),
      edges = list()
    )
    chat_factory <- function(role, model, system_prompt, echo) {
      text <- if (identical(role, "expert")) answer else ""
      structured <- switch(
        role,
        judge = extraction,
        mindmap = mindmap,
        list()
      )
      fake_chat(
        text = list(text),
        structured = if (length(structured) > 0L) {
          list(structured)
        } else {
          list()
        }
      )
    }
    cfg <- tempest_config(chat_fn = chat_factory)
    session <- tempest_session(
      topic,
      config = cfg,
      experts = list(expert),
      session_id = session_id
    )
    session$workspace$upsert_retrieved_resource(source)
    list(
      session = session,
      config = cfg,
      session_id = session_id,
      expert_id = expert@expert_id
    )
  }

  for (mode in c("sync", "async")) {
    fixture <- make_session(mode)
    if (identical(mode, "sync")) {
      expect_no_error(withCallingHandlers(
        fixture$session$warmup(verbose = FALSE),
        dsprrr_cache_security_warning = function(condition) {
          invokeRestart("muffleWarning")
        }
      ))
    } else {
      settled <- await_tempest_promise(tempest_session_warmup_async(
        fixture$session,
        max_parallel_experts = 1
      ))
      expect_null(settled$error)
      expect_identical(settled$value@status, "succeeded")
    }

    claims <- fixture$session$workspace$list_proposed_claims()
    records <- tempest:::tempest_session_stage_records(fixture$session)
    expect_length(claims, 1L)
    expect_length(records, 1L)
    claim <- claims[[1]]
    record <- records[[1]]
    expect_identical(claim@session_id, fixture$session_id)
    expect_identical(claim@expert_id, fixture$expert_id)
    record_correlation <- record@trace_references$correlation_id
    if (is.null(record_correlation)) {
      record_correlation <- NA_character_
    }
    expect_identical(
      claim@retrieval_step_id,
      record_correlation
    )
    expect_identical(
      record@trace_references$research_run_id,
      fixture$session_id
    )
    expect_identical(record@trace_references$expert_id, fixture$expert_id)
    snapshot <- tempest_session_snapshot(fixture$session)
    expect_identical(
      snapshot$workspace$proposed_claims[[1]]$session_id,
      fixture$session_id
    )

    bundle <- file.path(withr::local_tempdir(), mode)
    tempest_session_save(fixture$session, bundle)
    restored <- tempest_session_resume(bundle, config = fixture$config)
    restored_claim <- restored$workspace$list_proposed_claims()[[1]]
    expect_identical(restored_claim@session_id, fixture$session_id)
    expect_identical(
      tempest:::tempest_stage_records_data(
        tempest:::tempest_session_stage_records(restored)
      ),
      tempest:::tempest_stage_records_data(records)
    )
  }
})

test_that("public session verification persists pair support and source proof", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  source <- fake_source("https://example.org/persisted-session-verification")
  extracted <- list(
    facts = list(list(
      claim = "Session verification commits durable proof",
      sources = list(list(
        source_id = source@resource_id,
        quote = source@content
      )),
      confidence = "high"
    ))
  )
  cfg <- tempest_config(
    citation_policy = "strict",
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        return(fake_chat(structured = list(extracted)))
      }
      if (identical(role, "coordinator")) {
        return(fake_chat(
          text = list(paste0(
            "Session verification commits durable proof [",
            source@resource_id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Persisted session verification",
    config = cfg,
    experts = list(test_expert(name = "Session Verification Expert")),
    session_id = "persisted-session-verification"
  )
  session$workspace$upsert_retrieved_resource(source)
  completion_id <- tempest:::tempest_costorm_await(
    session$request_completion_async("Record verification evidence.")
  )
  withCallingHandlers(
    tempest:::tempest_costorm_await(tempest_session_process_turn_async(
      session,
      completion_id,
      suggest = FALSE,
      n_suggestions = 4L,
      is_current = function() TRUE
    )),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.95,
      rationale = "The captured source supports the claim."
    ))
  )
  audit <- withCallingHandlers(
    tempest_verify_claims(session, verifier = judge),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(audit$verification_status, "supported")
  expect_length(tempest:::tempest_session_stage_records(session), 2L)
  report_md <- tempest_report_md(
    title = session$title,
    body = paste0(
      "Session verification commits durable proof [",
      source@resource_id,
      "]."
    ),
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  report_md <- test_persistence_commit_existing_costorm_report(
    session,
    report_md
  )
  expect_no_error(tempest_session_snapshot(session))

  forged_report <- sub(
    paste0("# ", session$title, "\n\n"),
    paste0(
      "# ",
      session$title,
      "\n\n## A false factual conclusion\n\n"
    ),
    report_md,
    fixed = TRUE
  )
  expect_error(
    tempest:::tempest_session_set_report_value(session, forged_report),
    class = "tempest_product_report_error"
  )

  bundle <- file.path(withr::local_tempdir(), "session-verification")
  tempest_session_save(session, bundle)
  restored <- tempest_session_resume(bundle, config = cfg)
  expect_identical(
    tempest_claim_supports(restored$workspace)$verification_status,
    "supported"
  )
  expect_length(tempest:::tempest_session_stage_records(restored), 2L)

  report_path <- file.path(bundle, "report.md")
  persisted_report <- tempest:::tempest_read_text(report_path)
  forged_persisted <- sub(
    paste0("# ", session$title, "\n\n"),
    paste0(
      "# ",
      session$title,
      "\n\n## A false factual conclusion\n\n"
    ),
    persisted_report,
    fixed = TRUE
  )
  tempest:::tempest_write_text(report_path, forged_persisted)
  manifest_path <- file.path(bundle, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["report.md"]] <-
    tempest:::tempest_product_bundle_checksum(bundle, "report.md")
  manifest$report_reference <-
    tempest:::tempest_product_report_reference(forged_persisted)
  bound_manifest <- tempest:::tempest_product_authority_bind_report(
    tempest:::tempest_research_manifest_from_record(
      manifest$research_manifest
    ),
    forged_persisted
  )
  manifest$research_manifest <-
    tempest:::tempest_research_manifest_record(bound_manifest)
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )

  standalone <- tempest_research_workspace()
  standalone$upsert_retrieved_resource(source)
  standalone_span_id <- standalone$add_evidence_span(tempest_evidence_span(
    source_id = source@resource_id,
    quote = source@content,
    evidence_span_id = "span.standalone-verification"
  ))
  standalone$add_proposed_claim(tempest_claim(
    claim_id = "claim.standalone-verification",
    claim_text = "Discarded records cannot become session proof.",
    source_ids = source@resource_id,
    evidence_span_ids = standalone_span_id,
    supporting_quotes = list(source@content)
  ))
  standalone_judge <- fake_chat(
    structured = list(list(
      status = "supported",
      score = 0.95,
      rationale = "Standalone verification has no session ledger."
    ))
  )
  withCallingHandlers(
    tempest_verify_claims(
      standalone,
      verifier = standalone_judge,
      verifier_model = cfg@models[["judge"]]
    ),
    dsprrr_cache_security_warning = function(condition) {
      invokeRestart("muffleWarning")
    }
  )
  expect_error(
    tempest_session(
      "Unbound standalone verification",
      config = cfg,
      experts = list(test_expert(name = "Unbound Verification Expert")),
      retriever = tempest_retriever(config = cfg, workspace = standalone),
      session_id = "unbound-standalone-verification"
    ),
    class = "tempest_research_workspace_integrity_error"
  )
})
