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

test_that("schema runtime accepts verified Graft contracts and rejects other ranges", {
  for (version in c("0.2.0", "0.2.1", "0.3.0", "0.4.0", "0.5.0", "0.5.9")) {
    expect_identical(
      tempest_graft_pin_valid(list(contract = version)),
      TRUE,
      info = version
    )
  }
  for (version in c("0.1.9", "0.6.0", "1.0.0", "nope", "")) {
    expect_identical(
      tempest_graft_pin_valid(list(contract = version)),
      FALSE,
      info = version
    )
  }
  expect_identical(tempest_graft_pin_valid(NULL), FALSE)
})

test_that("schema runtime rejects a changed store format within an accepted contract", {
  skip_if_not_installed("graft")
  local_mocked_bindings(
    tempest_graft_contract_call = function() {
      list(contract = "0.5.0", store_format = "4.0.0")
    }
  )
  expect_error(tempest_graft_require(), class = "tempest_graft_schema_error")
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
