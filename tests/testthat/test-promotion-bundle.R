test_that("promotion bundle contains only exact promotable research records", {
  fixture <- test_promotion_bundle()
  bundle <- fixture$bundle

  expect_s7_class(bundle, TempestPromotionBundle)
  expect_named(
    formals(tempest_promotion_bundle),
    c("research", "claim_ids")
  )
  expect_identical(bundle@claim_ids, fixture$claim@claim_id)
  expect_identical(bundle@min_support_score, 0.7)
  expect_identical(
    vapply(bundle@records, length, integer(1)),
    c(
      Source = 1L,
      Claim = 1L,
      EvidenceSpan = 1L,
      ClaimSupport = 1L,
      ProgramArtifact = 2L
    )
  )
  expect_identical(
    bundle@records$ClaimSupport[[1L]]$pair_verification_status,
    "supported"
  )
  expect_identical(
    bundle@records$EvidenceSpan[[1L]]$tempest_evidence_span_id,
    fixture$span@evidence_span_id
  )
  expect_identical(
    bundle@records$ClaimSupport[[1L]]$evidence_span_id,
    bundle@records$EvidenceSpan[[1L]]$id
  )
  expect_identical(
    bundle@records$Source[[1L]]$content_hash,
    paste0("sha256:", fixture$resource@content_hash)
  )
  expect_identical(
    bundle@records$ClaimSupport[[1L]]$source_content_hash,
    bundle@records$Source[[1L]]$content_hash
  )
  expect_identical(
    vapply(bundle@records$ProgramArtifact, `[[`, character(1), "id"),
    sort(
      c(
        fixture$programs$extract_claims$program_artifact_id,
        fixture$programs$verify_claim_support$program_artifact_id
      ),
      method = "radix"
    )
  )
  expect_identical(
    bundle@bundle_id,
    tempest_promotion_bundle(fixture$research)@bundle_id
  )
  data <- tempest:::tempest_promotion_bundle_data(bundle)
  repeated_data <- tempest:::tempest_promotion_bundle_data(
    tempest_promotion_bundle(fixture$research)
  )
  expect_identical(
    tempest:::tempest_product_canonical_json(data),
    tempest:::tempest_product_canonical_json(repeated_data)
  )
  expect_named(
    data,
    c(
      "bundle_id",
      "schema_version",
      "schema_build_digest",
      "research_run_id",
      "min_support_score",
      "research_manifest",
      "stage_records",
      "proof",
      "claim_ids",
      "records"
    )
  )
  expect_length(
    intersect(c("report_md", "report_reference", "product_state"), names(data)),
    0L
  )
})

test_that("promotion accepts an exact succeeded Co-STORM session", {
  fixture <- test_promotion_bundle("costorm")
  bundle <- fixture$bundle
  deputy_traces <- tempest:::tempest_session_deputy_traces(fixture$research)

  expect_s7_class(bundle, TempestPromotionBundle)
  expect_identical(bundle@research_run_id, fixture$research$session_id)
  expect_identical(bundle@claim_ids, fixture$claim@claim_id)
  expect_identical(
    bundle@bundle_id,
    tempest_promotion_bundle(fixture$research)@bundle_id
  )
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(fixture$workspace),
    "sealed"
  )
  expect_identical(
    tempest:::tempest_session_deputy_traces(fixture$research),
    deputy_traces
  )
})

test_that("promotion requires the exact live Co-STORM Deputy trace ledger", {
  session <- test_promotion_fixture("costorm")$research
  private <- session$.__enclos_env__$private
  original_traces <- private$deputy_traces_value
  withr::defer(private$deputy_traces_value <- original_traces)
  expect_gt(length(original_traces), 0L)

  private$deputy_traces_value <- list(list(malformed = "trace"))
  expect_error(
    tempest_promotion_bundle(session),
    class = "tempest_promotion_error"
  )

  private$deputy_traces_value <- list()
  expect_identical(tempest:::tempest_session_deputy_traces(session), list())
  expect_error(
    tempest_promotion_bundle(session),
    class = "tempest_promotion_error"
  )

  mismatched_traces <- original_traces
  mismatched_traces[[1L]]$correlation_id <-
    "correlation.promotion-costorm-mismatch"
  private$deputy_traces_value <- mismatched_traces
  expect_no_error(tempest:::tempest_session_deputy_traces(session))
  expect_error(
    tempest_promotion_bundle(session),
    class = "tempest_promotion_error"
  )
})

test_that("promotion rejects loose and incomplete STORM inputs", {
  fixture <- test_promotion_fixture()
  loose <- list(
    workspace = fixture$workspace,
    manifest = fixture$manifest,
    stage_records = fixture$stage_records
  )
  expect_error(
    tempest_promotion_bundle(loose),
    class = "tempest_promotion_error"
  )

  incomplete <- fixture$research
  incomplete$state$completed_stages <- head(
    incomplete$state$completed_stages,
    -1L
  )
  expect_error(
    tempest_promotion_bundle(incomplete),
    class = "tempest_promotion_error"
  )
})

test_that("promotion rejects tampered and cross-product STORM results", {
  fixture <- test_promotion_fixture()
  other <- test_promotion_fixture("costorm")

  tampered <- fixture$research
  tampered$report_md <- paste0(tampered$report_md, "\n\nTampered.")
  expect_error(
    tempest_promotion_bundle(tampered),
    class = "tempest_promotion_error"
  )

  wrong_mode <- fixture$research
  wrong_mode$manifest <- other$manifest
  expect_error(
    tempest_promotion_bundle(wrong_mode),
    class = "tempest_promotion_error"
  )

  cross_product <- fixture$research
  cross_product$workspace <- other$workspace
  expect_error(
    tempest_promotion_bundle(cross_product),
    class = "tempest_promotion_error"
  )
})

test_that("promotion rejects unsealed STORM products", {
  fixture <- test_promotion_fixture()
  private <- fixture$workspace$.__enclos_env__$private
  original_state <- private$mutation_state_value
  withr::defer(private$mutation_state_value <- original_state)
  private$mutation_state_value <- "open"

  expect_error(
    tempest_promotion_bundle(fixture$research),
    class = "tempest_promotion_error"
  )
})

test_that("promotion rejects invalid and nonquiescent Co-STORM sessions", {
  tampered <- test_promotion_fixture("costorm")$research
  private <- tampered$.__enclos_env__$private
  original_report <- private$report_md_value
  withr::defer(private$report_md_value <- original_report)
  private$report_md_value <- paste0(
    tempest_session_report_md(tampered),
    "\n\nTampered."
  )
  expect_error(
    tempest_promotion_bundle(tampered),
    class = "tempest_promotion_error"
  )
  private$report_md_value <- original_report

  active <- test_promotion_fixture("costorm")$research
  work_id <- tempest:::tempest_session_async_work_start(
    active,
    "dialogue",
    work_id = "promotion-active-work"
  )
  withr::defer(tempest:::tempest_session_async_work_finish(active, work_id))
  expect_error(
    tempest_promotion_bundle(active),
    class = "tempest_promotion_error"
  )
  tempest:::tempest_session_async_work_finish(active, work_id)

  unsealed <- test_promotion_fixture("costorm")$research
  workspace_private <- unsealed$workspace$.__enclos_env__$private
  original_state <- workspace_private$mutation_state_value
  withr::defer(workspace_private$mutation_state_value <- original_state)
  workspace_private$mutation_state_value <- "open"
  expect_error(
    tempest_promotion_bundle(unsealed),
    class = "tempest_promotion_error"
  )
  workspace_private$mutation_state_value <- original_state
})

test_that("promotion rejects cross-mode Co-STORM state", {
  costorm <- test_promotion_fixture("costorm")$research
  storm <- test_promotion_fixture()$research
  private <- costorm$.__enclos_env__$private
  original_manifest <- private$manifest_value
  withr::defer(private$manifest_value <- original_manifest)
  private$manifest_value <- storm$manifest

  expect_error(
    tempest_promotion_bundle(costorm),
    class = "tempest_promotion_error"
  )
  private$manifest_value <- original_manifest
})

test_that("promotion fails closed without exact pair support", {
  fixture <- test_promotion_fixture()
  private <- fixture$workspace$.__enclos_env__$private
  original_supports <- private$claim_supports_value
  withr::defer(private$claim_supports_value <- original_supports)
  private$claim_supports_value <- new.env(parent = emptyenv())

  expect_error(
    tempest_promotion_bundle(fixture$research),
    class = "tempest_promotion_error"
  )
})

test_that("promotion requires a durable succeeded report binding", {
  fixture <- test_promotion_fixture()
  research <- fixture$research
  research$manifest <- tempest_research_manifest_update(
    fixture$manifest,
    deliverables = list()
  )

  expect_error(
    tempest_promotion_bundle(research),
    class = "tempest_promotion_error"
  )
})

test_that("promotion selection is closed over retained stage outputs", {
  fixture <- test_promotion_fixture()
  stages <- vapply(
    fixture$stage_records,
    function(record) record@stage,
    character(1)
  )
  extraction_index <- which(stages == "extract_claims")
  extraction <- fixture$stage_records[[extraction_index]]
  extraction@output_reference <- tempest:::tempest_stage_output_reference(
    "workspace_claims",
    c(fixture$claim@claim_id, "claim-unselected"),
    content_digest = extraction@output_reference$content_digest
  )

  records <- fixture$stage_records
  records[[extraction_index]] <- extraction
  expect_error(
    tempest:::tempest_promotion_stage_selection(
      records,
      fixture$claim@claim_id,
      fixture$support@claim_support_id,
      c(fixture$claim@claim_id, "claim-unselected"),
      fixture$support@claim_support_id
    ),
    class = "tempest_promotion_error"
  )
})

test_that("promotion eligibility is derived only from complete pair support", {
  partial_low <- tempest_claim_support(
    "claim-pairs",
    "span-low",
    "source-pairs",
    "partially_supported",
    0.6,
    "Partial support below the promotion threshold."
  )
  partial_high <- tempest_claim_support(
    "claim-pairs",
    "span-high",
    "source-pairs",
    "partially_supported",
    0.8,
    "Partial support at or above the promotion threshold."
  )
  supported_high <- tempest_claim_support(
    "claim-pairs",
    "span-supported",
    "source-pairs",
    "supported",
    0.9,
    "Direct support above the promotion threshold."
  )
  contradicted <- tempest_claim_support(
    "claim-pairs",
    "span-contradicted",
    "source-pairs",
    "contradicted",
    0.9,
    "This span contradicts the claim."
  )

  expect_identical(
    tempest:::tempest_promotion_claim_promotable(
      list(partial_low),
      0.7
    ),
    FALSE
  )
  expect_identical(
    tempest:::tempest_promotion_claim_promotable(
      list(partial_high),
      0.7
    ),
    TRUE
  )
  expect_identical(
    tempest:::tempest_promotion_claim_promotable(
      list(partial_high, contradicted),
      0.7
    ),
    FALSE
  )
  expect_identical(
    tempest:::tempest_promotion_claim_promotable(
      list(partial_low, supported_high),
      0.7
    ),
    TRUE
  )
  expect_identical(
    tempest:::tempest_promotion_support_summary(
      list(partial_low, supported_high)
    ),
    list(status = "partially_supported", score = 0.6)
  )
  for (supports in list(
    list(partial_low),
    list(partial_high),
    list(partial_high, contradicted),
    list(partial_low, supported_high)
  )) {
    expect_identical(
      tempest:::tempest_promotion_support_summary(supports),
      tempest:::tempest_claim_support_aggregate(supports)
    )
  }
})

test_that("promotion bundle persistence is closed and hard-cut", {
  fixture <- test_promotion_bundle()
  path <- file.path(withr::local_tempdir(), "promotion")

  saved <- tempest_save_promotion_bundle(fixture$bundle, path)
  restored <- tempest_read_promotion_bundle(
    saved,
    expected_bundle_id = fixture$bundle@bundle_id
  )
  expect_identical(
    tempest:::tempest_promotion_bundle_data(restored),
    tempest:::tempest_promotion_bundle_data(fixture$bundle)
  )
  expect_error(
    tempest_save_promotion_bundle(fixture$bundle, path),
    class = "tempest_promotion_persistence_error"
  )

  writeLines("legacy", file.path(path, "legacy.json"))
  expect_error(
    tempest_read_promotion_bundle(
      path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
  expect_error(
    tempest_read_promotion_bundle(path),
    class = "tempest_promotion_persistence_error"
  )
  expect_error(
    tempest_read_promotion_bundle(
      path,
      expected_bundle_id = paste0("sha256:", strrep("0", 64L))
    ),
    class = "tempest_promotion_persistence_error"
  )
})

test_that("stage-bound proof rejects self-signed evidence tampering", {
  fixture <- test_promotion_bundle()
  original <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  changes <- list(
    claim = function(data) {
      data$proof$claims[[1L]]$claim_text <- "A forged scientific claim."
      data$records$Claim[[1L]]$statement_text <- "A forged scientific claim."
      data
    },
    support = function(data) {
      data$proof$claim_supports[[1L]]$rationale <- "A forged rationale."
      data$records$ClaimSupport[[1L]]$rationale <- "A forged rationale."
      data
    },
    span = function(data) {
      data$proof$evidence_spans[[1L]]$quote <- "A forged excerpt."
      data$proof$claims[[1L]]$supporting_quotes <- list("A forged excerpt.")
      data$records$EvidenceSpan[[1L]]$excerpt <- "A forged excerpt."
      data$records$ClaimSupport[[1L]]$excerpt <- "A forged excerpt."
      data
    },
    source = function(data) {
      resource <- data$proof$resources[[1L]]
      resource$title <- "A forged source title"
      fingerprint_data <- resource
      fingerprint_data$fingerprint <- NULL
      resource$fingerprint <- tempest:::tempest_resource_fingerprint(
        fingerprint_data
      )
      data$proof$resources[[1L]] <- resource
      data$records$Source[[1L]]$title <- resource$title
      data
    }
  )

  for (change in changes) {
    tampered <- test_promotion_resign_data(change(original))
    expect_error(
      tempest:::tempest_promotion_bundle_from_data(tampered),
      class = "tempest_promotion_error"
    )
  }
})

test_that("promotion rows reject re-signed credential content", {
  fixture <- test_promotion_bundle()
  data <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  secret <- "api_key=sk-proj-12345678901234567890"
  data$records$ClaimSupport[[1L]]$rationale <- secret
  data <- test_promotion_resign_data(data)

  error <- expect_error(
    tempest:::tempest_promotion_bundle_from_data(data),
    class = "tempest_promotion_error"
  )
  rendered <- paste(
    conditionMessage(error),
    paste(capture.output(error), collapse = "\n")
  )
  expect_identical(grepl(secret, rendered, fixed = TRUE), FALSE)
})

test_that("stage proof rejects credentials outside evidence fields", {
  fixture <- test_promotion_bundle()
  original <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  secret <- "api_key=sk-proj-12345678901234567890"
  changes <- list(
    function(data) {
      data$proof$claims[[1L]]$contradiction_note <- secret
      data
    },
    function(data) {
      data$proof$claims[[1L]]$retrieval_query <- secret
      data
    },
    function(data) {
      data$proof$evidence_spans[[1L]]$section_heading <- secret
      data
    }
  )

  for (change in changes) {
    tampered <- change(original)
    expect_error(
      tempest:::tempest_promotion_restore_proof(tampered$proof),
      class = "tempest_promotion_error"
    )
    expect_error(
      tempest:::tempest_promotion_bundle_from_data(
        test_promotion_resign_data(tampered)
      ),
      class = "tempest_promotion_error"
    )
  }
})

test_that("stage proof rejects defaulting and scalar coercion", {
  fixture <- test_promotion_bundle()
  original <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  changes <- list(
    function(data) {
      data$proof$resources[[1L]]["schema_version"] <- list(NULL)
      data
    },
    function(data) {
      data$proof$resources[[1L]]$schema_version <- as.double(
        data$proof$resources[[1L]]$schema_version
      )
      data
    }
  )

  for (change in changes) {
    tampered <- change(original)
    expect_error(
      tempest:::tempest_promotion_restore_proof(tampered$proof),
      class = "tempest_promotion_error"
    )
    expect_error(
      tempest:::tempest_promotion_bundle_from_data(
        test_promotion_resign_data(tampered)
      ),
      class = "tempest_promotion_error"
    )
  }
})

test_that("promotion decoding requires exact current JSON scalar shapes", {
  fixture <- test_promotion_bundle()
  original <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  malformed <- list(
    function(data) {
      data$schema_version <- "1"
      data
    },
    function(data) {
      data$schema_version <- as.double(data$schema_version)
      data
    },
    function(data) {
      data$schema_version <- 0
      data
    },
    function(data) {
      data$min_support_score <- "0.7"
      data
    },
    function(data) {
      data$claim_ids <- data$claim_ids[[1L]]
      data
    },
    function(data) {
      data["claim_ids"] <- list(NULL)
      data
    },
    function(data) {
      data$claim_ids <- list(1)
      data
    }
  )

  for (change in malformed) {
    expect_error(
      tempest:::tempest_promotion_bundle_from_data(change(original)),
      class = "tempest_promotion_error"
    )
  }

  shuffled <- original[c(
    names(original)[2L],
    names(original)[1L],
    names(original)[-(1:2)]
  )]
  shuffled <- test_promotion_resign_data(shuffled)
  expect_error(
    tempest:::tempest_promotion_bundle_from_data(shuffled),
    class = "tempest_promotion_error"
  )
})

test_that("promotion persistence rejects forged current-format payloads", {
  fixture <- test_promotion_bundle()
  root <- withr::local_tempdir()

  old_path <- file.path(root, "old-schema")
  tempest_save_promotion_bundle(fixture$bundle, old_path)
  old_data <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  old_data$schema_version <- 0
  test_promotion_rewrite_saved(old_path, old_data)
  expect_error(
    tempest_read_promotion_bundle(
      old_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  ref_path <- file.path(root, "forged-ref")
  tempest_save_promotion_bundle(fixture$bundle, ref_path)
  ref_data <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  ref_data$records$ClaimSupport[[1L]]$evidence_span_id <- "tempest:forged"
  test_promotion_rewrite_saved(ref_path, ref_data)
  expect_error(
    tempest_read_promotion_bundle(
      ref_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  credential_path <- file.path(root, "credential")
  tempest_save_promotion_bundle(fixture$bundle, credential_path)
  credential_data <- tempest:::tempest_promotion_bundle_data(fixture$bundle)
  credential_data$records$Source[[1L]]$title <-
    "api_key=sk-proj-12345678901234567890"
  test_promotion_rewrite_saved(credential_path, credential_data)
  expect_error(
    tempest_read_promotion_bundle(
      credential_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
})

test_that("promotion persistence rejects unsafe roots, links, and inventory", {
  skip_on_os("windows")
  fixture <- test_promotion_bundle()
  root <- withr::local_tempdir()
  real <- file.path(root, "real")
  dir.create(real)
  bundle_path <- file.path(real, "promotion")
  tempest_save_promotion_bundle(fixture$bundle, bundle_path)

  root_link <- file.path(root, "root-link")
  expect_identical(file.symlink(bundle_path, root_link), TRUE)
  expect_error(
    tempest_read_promotion_bundle(
      root_link,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  ancestor_link <- file.path(root, "ancestor-link")
  expect_identical(file.symlink(real, ancestor_link), TRUE)
  expect_error(
    tempest_read_promotion_bundle(
      file.path(ancestor_link, "promotion"),
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
  expect_error(
    tempest_save_promotion_bundle(
      fixture$bundle,
      file.path(ancestor_link, "new-promotion")
    ),
    class = "tempest_promotion_persistence_error"
  )

  outside <- file.path(root, "outside.json")
  expect_identical(
    file.rename(file.path(bundle_path, "bundle.json"), outside),
    TRUE
  )
  expect_identical(
    file.symlink(outside, file.path(bundle_path, "bundle.json")),
    TRUE
  )
  expect_error(
    tempest_read_promotion_bundle(
      bundle_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  sidecar_path <- file.path(root, "sidecar")
  tempest_save_promotion_bundle(fixture$bundle, sidecar_path)
  sidecar_target <- file.path(root, "sidecar-target")
  writeLines("sidecar", sidecar_target)
  expect_identical(
    file.symlink(sidecar_target, file.path(sidecar_path, "extra")),
    TRUE
  )
  expect_error(
    tempest_read_promotion_bundle(
      sidecar_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  child <- file.path(real, "child")
  dir.create(child)
  traversal_link <- file.path(root, "traversal-link")
  expect_identical(file.symlink(child, traversal_link), TRUE)
  traversal <- file.path(traversal_link, "..", "escape")
  expect_error(
    tempest_save_promotion_bundle(fixture$bundle, traversal),
    class = "tempest_promotion_persistence_error"
  )
  actual_escape <- file.path(real, "escape")
  tempest_save_promotion_bundle(fixture$bundle, actual_escape)
  expect_error(
    tempest_read_promotion_bundle(
      traversal,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
})

test_that("promotion persistence rejects unsafe file sizes and types", {
  fixture <- test_promotion_bundle()
  root <- withr::local_tempdir()

  empty_path <- file.path(root, "empty")
  tempest_save_promotion_bundle(fixture$bundle, empty_path)
  writeLines(character(), file.path(empty_path, "bundle.json"))
  expect_error(
    tempest_read_promotion_bundle(
      empty_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  oversized_path <- file.path(root, "oversized")
  tempest_save_promotion_bundle(fixture$bundle, oversized_path)
  writeBin(
    rep(as.raw(0), 1024L * 1024L + 1L),
    file.path(oversized_path, "manifest.json")
  )
  expect_error(
    tempest_read_promotion_bundle(
      oversized_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  nonregular_path <- file.path(root, "nonregular")
  tempest_save_promotion_bundle(fixture$bundle, nonregular_path)
  unlink(file.path(nonregular_path, "bundle.json"))
  dir.create(file.path(nonregular_path, "bundle.json"))
  expect_error(
    tempest_read_promotion_bundle(
      nonregular_path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
})

test_that("failed promotion installation leaves no destination", {
  fixture <- test_promotion_bundle()
  path <- file.path(withr::local_tempdir(), "failed-install")
  testthat::local_mocked_bindings(
    tempest_promotion_install = function(staging, path) FALSE
  )

  expect_error(
    tempest_save_promotion_bundle(fixture$bundle, path),
    class = "tempest_promotion_persistence_error"
  )
  expect_identical(file.exists(path), FALSE)
})

test_that("promotion persistence errors do not expose malformed JSON", {
  fixture <- test_promotion_bundle()
  path <- file.path(withr::local_tempdir(), "malformed")
  tempest_save_promotion_bundle(fixture$bundle, path)
  secret <- "api_key=sk-proj-12345678901234567890"
  writeLines(
    paste0('{"broken":"', secret),
    file.path(path, "bundle.json")
  )
  manifest <- tempest:::tempest_promotion_manifest_core(
    fixture$bundle@bundle_id,
    tempest:::tempest_promotion_file_checksum(
      file.path(path, "bundle.json")
    )
  )
  manifest$manifest_digest <- tempest:::tempest_promotion_digest(manifest)
  tempest:::tempest_promotion_write_json(
    file.path(path, "manifest.json"),
    manifest
  )

  error <- expect_error(
    tempest_read_promotion_bundle(
      path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
  rendered <- paste(
    conditionMessage(error),
    capture.output(error),
    collapse = "\n"
  )
  expect_identical(grepl(secret, rendered, fixed = TRUE), FALSE)
})

test_that("promotion manifest requires an exact numeric current version", {
  fixture <- test_promotion_bundle()
  root <- withr::local_tempdir()
  values <- list("1", 0, list(1), NULL)

  for (index in seq_along(values)) {
    path <- file.path(root, paste0("manifest-version-", index))
    tempest_save_promotion_bundle(fixture$bundle, path)
    manifest <- jsonlite::fromJSON(
      file.path(path, "manifest.json"),
      simplifyVector = FALSE
    )
    manifest["schema_version"] <- list(values[[index]])
    core <- manifest[setdiff(names(manifest), "manifest_digest")]
    manifest$manifest_digest <- tempest:::tempest_promotion_digest(core)
    tempest:::tempest_promotion_write_json(
      file.path(path, "manifest.json"),
      manifest
    )
    expect_error(
      tempest_read_promotion_bundle(
        path,
        expected_bundle_id = fixture$bundle@bundle_id
      ),
      class = "tempest_promotion_persistence_error"
    )
  }

  path <- file.path(root, "manifest-double-version")
  tempest_save_promotion_bundle(fixture$bundle, path)
  manifest_path <- file.path(path, "manifest.json")
  manifest_json <- readLines(manifest_path, warn = FALSE)
  manifest_json <- sub(
    '"schema_version": 1,',
    '"schema_version": 1.0,',
    manifest_json,
    fixed = TRUE
  )
  writeLines(manifest_json, manifest_path)
  expect_error(
    tempest_read_promotion_bundle(
      path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )

  path <- file.path(root, "manifest-shuffled-fields")
  tempest_save_promotion_bundle(fixture$bundle, path)
  manifest_path <- file.path(path, "manifest.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest <- manifest[c(2L, 1L, seq.int(3L, length(manifest)))]
  core <- manifest[setdiff(names(manifest), "manifest_digest")]
  manifest$manifest_digest <- tempest:::tempest_promotion_digest(core)
  tempest:::tempest_promotion_write_json(manifest_path, manifest)
  expect_error(
    tempest_read_promotion_bundle(
      path,
      expected_bundle_id = fixture$bundle@bundle_id
    ),
    class = "tempest_promotion_persistence_error"
  )
})
