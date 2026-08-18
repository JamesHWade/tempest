test_that("session snapshots reject credentials outside evidence payloads", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  token <- "sk-proj-0123456789abcdefghijklmnopqrstuv"
  make_session <- function(topic = "Credential boundary") {
    tempest_session(
      topic,
      config = cfg,
      experts = list(test_expert(expert_id = "expert.credential-boundary"))
    )
  }

  session <- make_session()
  tempest:::tempest_session_restore_product_state(
    session,
    title = token,
    transcript = session$transcript,
    mindmap = session$mindmap,
    events = session$events,
    progress = NULL
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  session <- make_session()
  tempest:::tempest_session_set_suggestions(
    session,
    paste("Authorization: Bearer", token)
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  session <- make_session()
  fixture <- test_persistence_add_costorm_evidence(
    session,
    "credential-report"
  )
  test_persistence_commit_costorm_report(
    session,
    paste0(
      "# Credential boundary\n\nAuthorization: Bearer ",
      token,
      " [",
      fixture$source@resource_id,
      "].\n"
    )
  )
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  for (encoded_token in c(
    "sk\\-proj\\-0123456789abcdefghijklmnopqrstuv",
    "sk&#45;proj&#45;0123456789abcdefghijklmnopqrstuv"
  )) {
    session <- make_session()
    fixture <- test_persistence_add_costorm_evidence(
      session,
      paste0("encoded-credential-", nchar(encoded_token))
    )
    safe_report <- tempest_report_md(
      title = session$title,
      body = paste0(
        "A portable report body [",
        fixture$source@resource_id,
        "]."
      ),
      workspace = session$workspace,
      citation_policy = cfg@citation_policy,
      on_unsupported_claim = cfg@on_unsupported_claim,
      min_support_score = cfg@min_support_score
    )
    test_persistence_commit_costorm_report(
      session,
      sub(
        "A portable report body",
        encoded_token,
        safe_report,
        fixed = TRUE
      )
    )
    expect_error(
      tempest_session_snapshot(session),
      class = "tempest_session_snapshot_error"
    )
  }

  session <- make_session()
  fixture <- test_add_verifiable_claim(
    session$workspace,
    key = "credential-boundary"
  )
  support <- test_claim_support(fixture$claim, fixture$span)
  unsafe_support <- tempest:::tempest_claim_support_to_list(support)
  unsafe_support$rationale <- paste("Authorization: Bearer", token)
  workspace_private <- session$workspace$.__enclos_env__$private
  workspace_private$claim_supports_value[[support@claim_support_id]] <-
    unsafe_support
  expect_error(
    tempest_session_snapshot(session),
    class = "tempest_session_snapshot_error"
  )

  scientific_title <- "SK-BR-3, SK-N-SH, and SK-MEL-28"
  session <- make_session(scientific_title)
  tempest:::tempest_session_set_suggestions(session, scientific_title)
  fixture <- test_persistence_add_costorm_evidence(
    session,
    "scientific-title",
    claim_text = scientific_title
  )
  report_md <- tempest_report_md(
    title = scientific_title,
    body = paste0(
      scientific_title,
      " [",
      fixture$source@resource_id,
      "]."
    ),
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  test_persistence_commit_costorm_report(session, report_md)
  expect_no_error(tempest_session_snapshot(session))
})

test_that("no-reference Co reports remain canonical persistence products", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  source <- fake_source(
    "https://example.org/no-reference-session-report",
    content_text = "Captured session evidence is durable."
  )
  extracted <- list(
    facts = list(list(
      claim = "Captured session evidence is durable.",
      sources = list(list(
        source_id = source$id,
        quote = "Captured session evidence is durable."
      )),
      confidence = "high"
    ))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "writer")) {
        return(fake_chat(text = list(body)))
      }
      if (identical(role, "judge")) {
        return(fake_chat(structured = list(extracted)))
      }
      if (identical(role, "coordinator")) {
        return(fake_chat(
          text = list(paste0(
            "Captured session evidence is durable [",
            source$id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "No reference session report",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.no-reference-report")),
    session_id = "no-reference-session-report"
  )
  session$workspace$upsert_retrieved_resource(source)
  completion_id <- tempest:::tempest_costorm_await(
    session$request_completion_async("Record durable evidence.")
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
  tempest_verify_claims(
    session,
    verifier = fake_chat(
      structured = list(list(
        status = "supported",
        score = 0.95,
        rationale = "The captured source supports the claim."
      ))
    )
  )
  body <- paste0(
    "Captured session evidence is durable. [",
    source$id,
    "]."
  )

  report_md <- session$report(
    include_references = FALSE
  )
  expect_match(report_md, "^# No reference session report", perl = TRUE)
  expect_match(
    report_md,
    paste0("[", source$id, "]"),
    fixed = TRUE
  )
  expect_no_match(
    report_md,
    paste0("[^", source$id, "]"),
    fixed = TRUE
  )
  expect_no_match(report_md, "## References", fixed = TRUE)
  expect_no_error(tempest:::tempest_final_report_validate(
    report_md = report_md,
    workspace = session$workspace,
    title = session$title,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score,
    stage_records = tempest:::tempest_session_stage_records(session)
  ))
  snapshot <- tempest_session_snapshot(session)
  expect_identical(snapshot$research_manifest$status, "succeeded")
  expect_identical(
    snapshot$research_manifest$deliverables$report_md$sha256,
    snapshot$report_reference$sha256
  )

  bundle <- file.path(withr::local_tempdir(), "no-reference-session-report")
  tempest_session_save(session, bundle)
  restored <- tempest_session_resume(bundle, config = cfg)
  expect_identical(tempest_session_report_md(restored), report_md)
  expect_identical(restored$manifest@status, "succeeded")
  expect_identical(
    restored$manifest@deliverables$report_md$sha256,
    tempest:::tempest_persistence_report_reference(report_md)$sha256
  )
})

test_that("fenced package headings survive session report persistence", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Literal report headings",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.literal-headings")),
    session_id = "literal-report-headings"
  )
  fixture <- test_persistence_add_costorm_evidence(
    session,
    "literal-report-headings"
  )
  body <- paste(
    c(
      "Example:",
      "",
      "```text",
      "## Execution review",
      "literal execution content",
      "",
      "## References",
      "",
      "literal reference content",
      "```",
      "",
      paste0(
        "Durable session evidence supports this report [",
        fixture$source@resource_id,
        "]."
      )
    ),
    collapse = "\n"
  )
  report_md <- tempest_report_md(
    title = session$title,
    body = body,
    workspace = session$workspace,
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  report_md <- test_persistence_commit_costorm_report(session, report_md)

  snapshot <- tempest_session_snapshot(session)
  expect_identical(snapshot$report_md, report_md)
  bundle <- file.path(withr::local_tempdir(), "literal-headings")
  tempest_session_save(session, bundle)
  restored <- tempest_session_resume(bundle, config = cfg)
  expect_identical(tempest_session_report_md(restored), report_md)
})

test_that("public session extraction persists its exact terminal record", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  source <- fake_source("https://example.org/persisted-session-extraction")
  extracted <- list(
    facts = list(list(
      claim = "Session extraction is durably recorded.",
      sources = list(list(source_id = source$id)),
      confidence = "high"
    ))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(role, "judge")) {
        return(fake_chat(structured = list(extracted)))
      }
      if (identical(role, "coordinator")) {
        return(fake_chat(
          text = list(paste0(
            "Session extraction is durably recorded [",
            source$id,
            "]."
          ))
        ))
      }
      fake_chat()
    }
  )
  session <- tempest_session(
    "Persisted session extraction",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.session-extraction")),
    session_id = "persisted-session-extraction"
  )
  session$workspace$upsert_retrieved_resource(source)
  completion_id <- tempest:::tempest_costorm_await(
    session$request_completion_async("Record the durable extraction.")
  )
  expect_no_error(withCallingHandlers(
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
  ))
  claim <- session$workspace$list_proposed_claims()[[1]]
  expect_identical(claim@session_id, session$session_id)
  expect_identical(claim@expert_id, "moderator")
  expect_identical(is.na(claim@retrieval_step_id), FALSE)
  expect_length(claim@supporting_quotes, 0L)
  records <- tempest:::tempest_session_stage_records(session)
  expect_length(records, 1L)
  expect_identical(
    claim@retrieval_step_id,
    records[[1L]]@trace_references$correlation_id
  )
  expect_no_error(tempest_session_snapshot(session))

  bundle <- file.path(withr::local_tempdir(), "session-extraction")
  tempest_session_save(session, bundle)
  expect_r6_class(
    tempest_session_resume(bundle, config = cfg),
    "TempestSession"
  )
  records_path <- file.path(bundle, "stage_records.json")
  tempest:::tempest_write_json(records_path, list())
  manifest_path <- file.path(bundle, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["stage_records.json"]] <-
    tempest:::tempest_session_bundle_checksum(bundle, "stage_records.json")
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(bundle, config = cfg),
    class = "tempest_session_restore_error"
  )
})
