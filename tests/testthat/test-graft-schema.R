test_that("compiled Tempest research schema has the exact typed contracts", {
  schema <- tempest_graft_schema()

  expect_identical(
    schema@build_digest,
    tempest:::tempest_promotion_schema_build_digest
  )
  expect_identical(
    names(schema@classes),
    c(
      "Claim",
      "ClaimSupport",
      "EvidenceSpan",
      "GovernedProcedure",
      "ProgramArtifact",
      "Source"
    )
  )
  classes <- schema@manifest$classes
  expect_identical(classes$Source$id_policy, "deterministic")
  expect_identical(classes$Claim$id_policy, "deterministic")
  expect_identical(classes$ClaimSupport$id_policy, "deterministic")
  expect_identical(classes$EvidenceSpan$id_format, "linkml")
  expect_identical(
    classes$EvidenceSpan$slots$tempest_evidence_span_id$range,
    "string"
  )
  expect_identical(classes$ProgramArtifact$id_format, "linkml")
  expect_identical(
    classes$ClaimSupport$slots$evidence_span_id$range,
    "EvidenceSpan"
  )
  expect_identical(
    classes$GovernedProcedure$slots$program_artifact_id$range,
    "ProgramArtifact"
  )
  expect_identical(
    classes$GovernedProcedure$slots$stage$range,
    "TempestStage"
  )
  expect_identical(
    classes$GovernedProcedure$origin_key_slots,
    list("tempest_governed_procedure_id")
  )
  expect_identical(
    vapply(
      schema@manifest$enums$TempestStage$permissible_values,
      `[[`,
      character(1),
      "value"
    ),
    c(
      "draft_outline",
      "extract_claims",
      "lead_section",
      "next_question",
      "personas",
      "perspectives",
      "query_decomposition",
      "refined_outline",
      "section_writing",
      "verify_claim_support"
    )
  )
  expect_identical(
    sort(names(classes$GovernedProcedure$slots)),
    sort(c(
      "id",
      "created_at",
      "updated_at",
      "tempest_governed_procedure_id",
      "stage",
      "program_artifact_id",
      "contract_version",
      "evaluator_id",
      "evaluator_version"
    ))
  )
})

test_that("schema runtime assumption names the approved Graft accessor commit", {
  expect_identical(
    tempest:::tempest_graft_accessor_commit,
    "81bd3f83a3c8ee2bee22b61ff09b475f58b4f0e5"
  )
  expect_contains(
    tempest:::tempest_graft_required_exports(),
    "graft_view_snapshot"
  )
  expect_identical(
    tempest:::tempest_graft_behavior_fingerprint(),
    tempest:::tempest_graft_behavior_digest
  )
})

test_that("schema runtime rejects a mismatched Graft RemoteSha", {
  testthat::local_mocked_bindings(
    tempest_graft_remote_sha = function() strrep("0", 40L)
  )

  expect_error(
    tempest:::tempest_graft_require(),
    class = "tempest_graft_schema_error"
  )
})

test_that("schema runtime rejects an unpinned local Graft build", {
  testthat::local_mocked_bindings(
    tempest_graft_remote_sha = function() NULL,
    tempest_graft_behavior_fingerprint = function() {
      paste0("sha256:", strrep("0", 64L))
    }
  )

  expect_error(
    tempest:::tempest_graft_require(),
    class = "tempest_graft_schema_error"
  )
})

test_that("schema compiler reuses the exact runtime Graft pin", {
  compiler <- readLines(
    testthat::test_path(
      "..",
      "..",
      "dev",
      "schema",
      "compile-tempest-research-schema.R"
    ),
    warn = FALSE
  )

  expect_match(compiler, "sys.source", all = FALSE)
  expect_match(compiler, "tempest_graft_pin_valid", all = FALSE)
  expect_match(compiler, "tempest_graft_remote_sha", all = FALSE)
})
