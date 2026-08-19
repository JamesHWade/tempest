test_that("session bundles exclude process-local and generic registries", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Customer objective",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.customer-context",
      name = "Customer Context Expert",
      title = "Customer context analyst",
      description = "Interprets customer objectives and constraints.",
      instructions = "Use the selected customer-context procedure."
    ))
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")

  snapshot <- tempest_session_snapshot(session)
  tempest_session_save(session, bundle_dir)
  bundle_files <- list.files(bundle_dir, recursive = TRUE)
  bundle_text <- paste(
    vapply(
      file.path(bundle_dir, bundle_files),
      \(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )

  expect_setequal(
    intersect(
      names(snapshot),
      c(
        "skills",
        "connection_refs",
        "connection_permissions",
        "capability_grants"
      )
    ),
    character()
  )
  expect_setequal(
    intersect(
      bundle_files,
      c(
        "skills.json",
        "connection_refs.json",
        "connection_permissions.json",
        "capability_grants.json"
      )
    ),
    character()
  )
  expect_no_match(bundle_text, "capability_grants", fixed = TRUE)
  expect_no_match(bundle_text, "connection_permissions", fixed = TRUE)
})

test_that("expert-session writer rejects incomplete live bindings", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Exact expert-session writer",
    config = cfg,
    experts = list(test_expert(expert_id = "expert.exact-writer"))
  )
  manager <- tempest:::tempest_session_expert_manager(session)
  created <- manager$get_or_create("expert.exact-writer")
  binding <- manager$session_profile(created$session_id)

  expect_no_error(
    tempest:::tempest_expert_session_snapshot_record(binding)
  )
  missing <- binding[names(binding) != "grants"]
  expect_error(
    tempest:::tempest_expert_session_snapshot_record(missing),
    class = "tempest_session_snapshot_error"
  )
  null <- binding
  null["grants"] <- list(NULL)
  expect_error(
    tempest:::tempest_expert_session_snapshot_record(null),
    class = "tempest_session_snapshot_error"
  )
})

test_that("session restore rejects contract and expert-binding tampering", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  expert <- tempest_expert(
    expert_id = "expert.tamper-check",
    name = "Integrity Expert",
    title = "Integrity analyst",
    description = "Checks persisted profile bindings.",
    instructions = "Reject changed profile definitions."
  )
  session <- tempest_session(
    "Integrity check",
    config = cfg,
    experts = list(expert)
  )
  tempest:::tempest_session_expert_manager(session)$get_or_create(
    expert@expert_id
  )
  bundle_dir <- file.path(withr::local_tempdir(), "bundle")
  tempest_session_save(session, bundle_dir)

  experts_path <- file.path(bundle_dir, "experts.json")
  experts <- tempest:::tempest_product_read_json(experts_path)
  experts[[1]]$fingerprint <- strrep("0", 64)
  tempest:::tempest_product_write_json(experts_path, experts)
  manifest_path <- file.path(bundle_dir, "session.json")
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["experts.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      bundle_dir,
      "experts.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)

  expect_error(
    tempest:::tempest_session_resume_internal(
      bundle_dir,
      config = cfg
    ),
    class = "tempest_session_restore_error"
  )

  runtime_object <- tempest_session_snapshot(session)
  runtime_object$expert_sessions[[1]]$runtime <- new.env(parent = emptyenv())
  expect_error(
    tempest:::tempest_session_restore_internal(
      runtime_object,
      config = cfg
    ),
    class = "tempest_session_restore_error"
  )

  tamper_expert_sessions <- function(mutate) {
    tempest_session_save(session, bundle_dir, overwrite = TRUE)
    sessions_path <- file.path(bundle_dir, "expert_sessions.json")
    sessions <- tempest:::tempest_product_read_json(sessions_path)
    sessions[[1]] <- mutate(sessions[[1]])
    tempest:::tempest_product_write_json(sessions_path, sessions)
    manifest_path <- file.path(bundle_dir, "session.json")
    manifest <- tempest:::tempest_product_read_json(manifest_path)
    manifest$checksums[["expert_sessions.json"]] <-
      tempest:::tempest_product_bundle_checksum(
        bundle_dir,
        "expert_sessions.json"
      )
    tempest:::tempest_product_write_json(manifest_path, manifest)
    expect_error(
      tempest:::tempest_session_resume_internal(
        bundle_dir,
        config = cfg
      ),
      class = "tempest_session_restore_error"
    )
  }

  tamper_expert_sessions(function(binding) {
    binding$model_role <- NULL
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$unexpected <- "runtime"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$model_role <- "writer"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$allowed_connection_ref_ids <- "connection.foreign"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$allowed_connection_ref_ids <- list(list("connection.nested"))
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$created_at <- "not-a-timestamp"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$grants <- list(
      "capability.invalid" = list(status = "granted")
    )
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding["grants"] <- list(NULL)
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$grants <- list(
      "capability.nested" = list(
        capability_id = "capability.nested",
        capability_version = "1",
        operation_id = NULL,
        operation_version = NULL,
        required = TRUE,
        status = "denied",
        connection_ref_ids = list(list("connection.nested")),
        reason_code = NULL,
        reason = NULL,
        metadata = list()
      )
    )
    binding
  })

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  sessions_path <- file.path(bundle_dir, "expert_sessions.json")
  sessions <- tempest:::tempest_product_read_json(sessions_path)
  duplicate <- sessions[[1]]
  duplicate$session_id <- "session.duplicate"
  sessions[[2]] <- duplicate
  tempest:::tempest_product_write_json(sessions_path, sessions)
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["expert_sessions.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      bundle_dir,
      "expert_sessions.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)
  expect_error(
    tempest:::tempest_session_resume_internal(
      bundle_dir,
      config = cfg
    ),
    class = "tempest_session_restore_error"
  )

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  sessions_path <- file.path(bundle_dir, "expert_sessions.json")
  sessions <- tempest:::tempest_product_read_json(sessions_path)
  sessions[[1]]$expert_fingerprint <- strrep("f", 64)
  tempest:::tempest_product_write_json(sessions_path, sessions)
  manifest <- tempest:::tempest_product_read_json(manifest_path)
  manifest$checksums[["expert_sessions.json"]] <-
    tempest:::tempest_product_bundle_checksum(
      bundle_dir,
      "expert_sessions.json"
    )
  tempest:::tempest_product_write_json(manifest_path, manifest)

  expect_error(
    tempest:::tempest_session_resume_internal(
      bundle_dir,
      config = cfg
    ),
    class = "tempest_session_restore_error"
  )
})
