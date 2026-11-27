test_that("session resume fails closed for corrupt optional data", {
  fixture <- test_persistence_costorm_session(
    topic = "Strict recovery",
    expert_id = "expert.strict-recovery"
  )
  cfg <- fixture$config
  session <- fixture$session
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
  expect_error(
    tempest_session_resume(
      bundle_dir,
      config = cfg,
      partial_recovery = TRUE
    ),
    class = "simpleError",
    regexp = "unused argument"
  )
})

test_that("optional presentation readers propagate malformed data", {
  path <- withr::local_tempfile(fileext = ".json")
  writeLines("{", path)

  expect_error(
    tempest:::tempest_session_bundle_optional_json(
      path,
      what = "optional product"
    ),
    class = "tempest_session_restore_error"
  )
})

test_that("session resume rejects every damaged bundle file", {
  fixture <- test_persistence_costorm_session(
    topic = "Strict durable recovery",
    expert_id = "expert.durable-recovery"
  )
  cfg <- fixture$config
  session <- fixture$session
  critical_files <- c(
    "experts.json",
    "retired_expert_ids.json",
    "expert_sessions.json",
    "transcript.json",
    "mindmap.json",
    "stage_records.json",
    "workspace/retrieved_resources.json",
    "workspace/proposed_claims.json",
    "workspace/evidence_spans.json",
    "workspace/disputes.json",
    "workspace/claim_supports.json",
    "workspace/claim_reviews.json"
  )
  root <- withr::local_tempdir()

  for (index in seq_along(critical_files)) {
    bundle_dir <- file.path(root, paste0("critical-", index))
    tempest_session_save(session, bundle_dir)
    writeLines(
      "tampered durable state",
      file.path(
        bundle_dir,
        critical_files[[index]]
      )
    )
    expect_error(
      tempest_session_resume(bundle_dir, config = cfg),
      class = "tempest_session_restore_error",
      info = critical_files[[index]]
    )
  }
})

test_that("session bundles require the exact retirement sidecar", {
  fixture <- test_persistence_costorm_session(
    topic = "Exact retirement sidecar",
    expert_id = "expert.retirement-sidecar"
  )
  cfg <- fixture$config
  session <- fixture$session
  root <- withr::local_tempdir()

  missing <- file.path(root, "missing")
  tempest_session_save(session, missing)
  manifest_path <- file.path(missing, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$files <- as.list(setdiff(
    unlist(manifest$files, use.names = FALSE),
    "retired_expert_ids.json"
  ))
  manifest$checksums[["retired_expert_ids.json"]] <- NULL
  unlink(file.path(missing, "retired_expert_ids.json"))
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest_session_resume(missing, config = cfg),
    class = "tempest_session_restore_error"
  )

  extra <- file.path(root, "extra")
  tempest_session_save(session, extra)
  tempest:::tempest_product_write_json(
    file.path(extra, "retired_expert_ids-copy.json"),
    list()
  )
  expect_error(
    tempest_session_resume(extra, config = cfg),
    class = "tempest_session_restore_error"
  )
})

test_that("session resume rejects files that its manifest does not declare", {
  fixture <- test_persistence_costorm_session(
    topic = "Declared inventory",
    expert_id = "expert.inventory"
  )
  cfg <- fixture$config
  session <- fixture$session
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)
  tempest:::tempest_product_write_json(
    file.path(bundle_dir, "artifacts/suggested_questions.json"),
    "undeclared"
  )

  expect_error(
    tempest_session_resume(bundle_dir, config = cfg),
    class = "tempest_session_restore_error"
  )
})
