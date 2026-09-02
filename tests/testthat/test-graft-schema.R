test_that("compiled Tempest research schema has the exact typed contracts", {
  skip_if_not_installed("graft")
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
      "Source",
      "GraftDefinition"
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

test_that("schema runtime pins the Graft consumer contract", {
  skip_if_not_installed("graft")
  expect_match(
    tempest:::tempest_graft_contract_version,
    "^[0-9]+\\.[0-9]+\\.[0-9]+$"
  )
  expect_contains(
    tempest:::tempest_graft_required_exports(),
    c("graft_view_snapshot", "graft_contract_version", "graft_changes")
  )
  expect_identical(
    tempest:::tempest_graft_pin_valid(graft::graft_contract_version()),
    TRUE
  )
  required <- numeric_version(tempest:::tempest_graft_contract_version)
  patched <- paste(
    required[[1L, 1L]],
    required[[1L, 2L]],
    required[[1L, 3L]] + 1L,
    sep = "."
  )
  minor <- paste(required[[1L, 1L]], required[[1L, 2L]] + 1L, 0L, sep = ".")
  major <- paste(required[[1L, 1L]] + 1L, 0L, 0L, sep = ".")
  expect_identical(
    tempest:::tempest_graft_pin_valid(list(contract = patched)),
    TRUE
  )
  expect_identical(
    tempest:::tempest_graft_pin_valid(list(contract = minor)),
    FALSE
  )
  expect_identical(
    tempest:::tempest_graft_pin_valid(list(contract = major)),
    FALSE
  )
  expect_identical(
    tempest:::tempest_graft_pin_valid(list(contract = "0.0.1")),
    FALSE
  )
  expect_identical(
    tempest:::tempest_graft_pin_valid(list(contract = "nope")),
    FALSE
  )
  expect_identical(tempest:::tempest_graft_pin_valid(NULL), FALSE)
})

test_that("schema runtime rejects an incompatible Graft contract", {
  skip_if_not_installed("graft")
  testthat::local_mocked_bindings(
    tempest_graft_contract_call = function() list(contract = "99.0.0")
  )

  expect_snapshot(tempest:::tempest_graft_require(), error = TRUE)
})

test_that("schema runtime rejects a Graft build without a contract", {
  skip_if_not_installed("graft")
  testthat::local_mocked_bindings(
    tempest_graft_contract_call = function() stop("no contract")
  )

  expect_error(
    tempest:::tempest_graft_require(),
    class = "tempest_graft_schema_error"
  )
})

test_that("schema compiler reuses the exact runtime Graft pin", {
  compiler_path <- testthat::test_path(
    "..",
    "..",
    "dev",
    "schema",
    "compile-tempest-research-schema.R"
  )
  testthat::skip_if_not(file.exists(compiler_path))

  compiler <- readLines(
    compiler_path,
    warn = FALSE
  )

  expect_match(compiler, "sys.source", all = FALSE)
  expect_match(compiler, "tempest_graft_pin_valid", all = FALSE)
  expect_match(compiler, "graft_contract_version", all = FALSE)
})
