test_that("partial session recovery is explicit and skips corrupt optional data", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Partial recovery",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.recovery",
      name = "Dr. Recovery",
      title = "Recovery expert",
      description = "Partial bundle recovery.",
      instructions = "Recover only explicitly optional state."
    ))
  )
  tempest:::tempest_session_set_suggestions(session, "What remains?")
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  writeLines("{", questions_path)

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
  warnings <- character()
  restored <- withCallingHandlers(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )

  expect_r6_class(restored, "TempestSession")
  expect_identical(
    tempest:::tempest_session_suggestions(restored),
    character()
  )
  expect_match(paste(warnings, collapse = "\n"), "incomplete", fixed = TRUE)
  expect_identical(restored$manifest@status, "running")
  expect_null(tempest:::tempest_session_report_value(restored))
  authority <- tempest:::tempest_product_authority_validate(
    restored$manifest,
    tempest:::tempest_session_stage_records(restored),
    restored$workspace,
    config = cfg,
    experts = restored$experts,
    expert_sessions = tempest:::tempest_expert_sessions_snapshot(restored)
  )
  expect_identical(authority$publishable, FALSE)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  declared <- suppressWarnings(
    tempest:::tempest_session_bundle_validate_manifest(
      bundle_dir,
      manifest,
      partial_recovery = TRUE
    )
  )
  expect_false("artifacts/suggested_questions.json" %in% declared)
})

test_that("partial recovery rejects a re-signed running report splice", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Partial report splice",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.partial-report"))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)

  report_md <- "# Forged partial report"
  tempest:::tempest_session_bundle_write_text(
    bundle_dir,
    "report.md",
    report_md
  )
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  research_manifest <- tempest:::tempest_research_manifest_from_record(
    manifest$research_manifest
  )
  research_manifest <- tempest:::tempest_persistence_manifest_bind_report(
    research_manifest,
    report_md
  )
  manifest$research_manifest <- tempest_research_manifest_record(
    research_manifest
  )
  manifest$report_reference <- tempest:::tempest_persistence_report_reference(
    report_md
  )
  manifest$files <- as.list(sort(c(
    unlist(manifest$files, use.names = FALSE),
    "report.md"
  )))
  manifest$checksums[["report.md"]] <-
    tempest:::tempest_session_bundle_checksum(bundle_dir, "report.md")
  tempest:::tempest_write_json(manifest_path, manifest)

  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("partial recovery rejects missing and unsafe suggestion files", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Suggestion recovery",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.suggestion-recovery",
      name = "Suggestion Recovery Expert",
      title = "Recovery analyst",
      description = "Tests optional suggestion recovery.",
      instructions = "Keep durable state strict."
    ))
  )
  tempest:::tempest_session_set_suggestions(session, "What remains?")
  root <- withr::local_tempdir()
  bundle_dir <- file.path(root, "missing-suggestions")

  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  unlink(questions_path)
  manifest <- tempest:::tempest_read_json_strict(
    file.path(bundle_dir, "session.json")
  )
  expect_error(
    tempest:::tempest_session_bundle_validate_manifest(
      bundle_dir,
      manifest,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- file.path(root, "symlinked-suggestions")
  tempest_session_save(session, bundle_dir)
  questions_path <- file.path(
    bundle_dir,
    "artifacts/suggested_questions.json"
  )
  external_path <- tempfile("tempest-external-suggestions-")
  writeLines('["Outside bundle"]', external_path)
  unlink(questions_path)
  linked <- file.symlink(external_path, questions_path)
  if (isTRUE(linked)) {
    manifest <- tempest:::tempest_read_json_strict(
      file.path(bundle_dir, "session.json")
    )
    expect_error(
      tempest:::tempest_session_bundle_validate_manifest(
        bundle_dir,
        manifest,
        partial_recovery = TRUE
      ),
      class = "tempest_session_restore_error"
    )
  }
})

test_that("partial recovery rejects every non-presentation integrity failure", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Strict durable recovery",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.strict-recovery",
      name = "Strict Recovery Expert",
      title = "Integrity analyst",
      description = "Rejects damage to durable session state.",
      instructions = "Never recover corrupted durable state."
    ))
  )
  root <- withr::local_tempdir()
  critical_files <- c(
    "experts.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "stage_records.json",
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json"
  )

  for (index in seq_along(critical_files)) {
    critical_file <- critical_files[[index]]
    bundle_dir <- file.path(root, paste0("critical-", index))
    tempest_session_save(session, bundle_dir)
    writeLines("tampered durable state", file.path(bundle_dir, critical_file))
    expect_error(
      tempest_session_resume(
        bundle_dir,
        config = cfg,
        partial_recovery = TRUE
      ),
      class = "tempest_session_restore_error"
    )
  }

  bundle_dir <- file.path(root, "malformed-claims")
  tempest_session_save(session, bundle_dir)
  manifest_path <- file.path(bundle_dir, "session.json")
  claims_path <- file.path(bundle_dir, "workspace/proposed_claims.json")
  writeLines("{", claims_path)
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$checksums[["workspace/proposed_claims.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "workspace/proposed_claims.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )

  bundle_dir <- file.path(root, "workflow-sidecar")
  tempest_session_save(session, bundle_dir)
  manifest_path <- file.path(bundle_dir, "session.json")
  workflow_path <- file.path(bundle_dir, "workflow_run.json")
  tempest:::tempest_write_json(workflow_path, list(schema_version = 2L))
  manifest <- tempest:::tempest_read_json_strict(manifest_path)
  manifest$files <- sort(c(
    unlist(manifest$files, use.names = FALSE),
    "workflow_run.json"
  ))
  manifest$checksums[["workflow_run.json"]] <-
    tempest:::tempest_session_bundle_checksum(
      bundle_dir,
      "workflow_run.json"
    )
  tempest:::tempest_write_json(manifest_path, manifest)
  writeLines("tampered workflow state", workflow_path)
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("session resume rejects files that its manifest does not declare", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Declared inventory",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.inventory",
      name = "Dr. Inventory",
      title = "Inventory expert",
      description = "Manifest-scoped bundle loading.",
      instructions = "Load only files declared by the manifest."
    ))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)
  tempest:::tempest_write_json(
    file.path(bundle_dir, "artifacts/suggested_questions.json"),
    "undeclared"
  )

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})
