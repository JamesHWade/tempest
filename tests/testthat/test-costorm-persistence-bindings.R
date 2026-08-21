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
  expert <- test_expert(expert_id = "expert.exact-writer")
  session <- tempest_session(
    "Exact expert-session writer",
    config = cfg,
    experts = list(expert)
  )
  manager <- tempest:::tempest_session_expert_manager(session)
  created <- manager$get_or_create(expert@expert_id)
  binding <- manager$session_profile(created$session_id)

  expect_identical(
    names(binding),
    c(
      "session_id",
      "expert_id",
      "expert_version",
      "expert_fingerprint",
      "created_at"
    )
  )
  expect_no_error(
    tempest:::tempest_expert_session_snapshot_record(binding)
  )
  for (field in names(binding)) {
    missing <- binding
    missing[[field]] <- NULL
    expect_error(
      tempest:::tempest_expert_session_snapshot_record(missing),
      class = "tempest_session_snapshot_error",
      info = field
    )
    null <- binding
    null[field] <- list(NULL)
    expect_error(
      tempest:::tempest_expert_session_snapshot_record(null),
      class = "tempest_session_snapshot_error",
      info = field
    )
  }
  extra <- binding
  extra$runtime <- "process-local"
  expect_error(
    tempest:::tempest_expert_session_snapshot_record(extra),
    class = "tempest_session_snapshot_error"
  )
  expect_error(
    tempest:::tempest_expert_session_snapshot_record(rev(binding)),
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
    name = "Integrity Expert",
    title = "Integrity analyst",
    description = "Checks persisted profile bindings.",
    instructions = "Reject changed profile definitions."
  )
  other_expert <- tempest_expert(
    name = "Second Integrity Expert",
    title = "Independent integrity analyst",
    description = "Checks duplicate persisted profile bindings.",
    instructions = "Reject ambiguous expert-session identities."
  )
  session <- tempest_session(
    "Integrity check",
    config = cfg,
    experts = list(expert, other_expert)
  )
  tempest:::tempest_session_expert_manager(session)$get_or_create(
    expert@expert_id
  )
  tempest:::tempest_session_expert_manager(session)$get_or_create(
    other_expert@expert_id
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
  expect_true(all(vapply(
    runtime_object$expert_sessions,
    \(binding) {
      identical(
        names(binding),
        tempest:::tempest_expert_session_record_fields()
      )
    },
    logical(1)
  )))
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

  for (field in tempest:::tempest_expert_session_record_fields()) {
    tamper_expert_sessions(function(binding) {
      binding[[field]] <- NULL
      binding
    })
  }
  tamper_expert_sessions(function(binding) {
    binding$unexpected <- "runtime"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$expert_id <- "expert.unknown"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$expert_version <- "sha256:changed"
    binding
  })
  tamper_expert_sessions(function(binding) {
    binding$created_at <- "not-a-timestamp"
    binding
  })

  tempest_session_save(session, bundle_dir, overwrite = TRUE)
  sessions_path <- file.path(bundle_dir, "expert_sessions.json")
  sessions <- tempest:::tempest_product_read_json(sessions_path)
  sessions[[2]]$session_id <- sessions[[1]]$session_id
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
  sessions <- tempest:::tempest_product_read_json(sessions_path)
  sessions[[2]]$expert_id <- sessions[[1]]$expert_id
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
