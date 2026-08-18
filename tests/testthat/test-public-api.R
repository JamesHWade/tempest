test_that("the 0.2 public export surface is exact", {
  expected <- readLines(
    test_path("fixtures", "public-exports-0.2.0.txt"),
    warn = FALSE
  )
  actual <- sort(getNamespaceExports("tempest"), method = "radix")

  expect_length(expected, 62L)
  expect_identical(expected, sort(unique(expected), method = "radix"))
  expect_identical(actual, expected)
})

test_that("the generic kernel is physically absent from the namespace", {
  retired_public <- c(
    "tempest_artifact",
    "tempest_artifact_catalog",
    "tempest_artifact_catalog_restore",
    "tempest_artifact_codec",
    "tempest_artifact_codec_definition",
    "tempest_artifact_codec_registry",
    "tempest_artifact_representation",
    "tempest_artifact_store",
    "tempest_builtin_operation_registry",
    "tempest_builtin_workflow_operation_registry",
    "tempest_capability_resolver",
    "tempest_capability_spec",
    "tempest_connection_provider",
    "tempest_connection_ref",
    "tempest_costorm_workflow_adapter",
    "tempest_costorm_workflow_run",
    "tempest_costorm_workflow_spec",
    "tempest_deliverable_spec",
    "tempest_generate_deliverable",
    "tempest_memory_artifact_store",
    "tempest_objective",
    "tempest_operation_registry",
    "tempest_run_approvals",
    "tempest_run_artifact",
    "tempest_run_artifacts",
    "tempest_run_capability_grants",
    "tempest_run_events",
    "tempest_run_record_approval",
    "tempest_run_request_cancel",
    "tempest_run_save",
    "tempest_run_snapshot",
    "tempest_run_status",
    "tempest_run_workflow",
    "tempest_runtime",
    "tempest_skill",
    "tempest_skill_registry",
    "tempest_storm_workflow_adapter",
    "tempest_storm_workflow_run",
    "tempest_storm_workflow_spec",
    "tempest_workflow_spec",
    "tempest_workflow_step"
  )
  removed_internal <- c(
    "DiscourseManager",
    "ExpertSessionManager",
    "TempestArtifact",
    "TempestArtifactCatalog",
    "TempestArtifactCodecRegistry",
    "TempestCancelToken",
    "TempestCapabilityResolver",
    "TempestCapabilitySpec",
    "TempestConnectionProvider",
    "TempestConnectionRef",
    "TempestDeliverableSpec",
    "TempestObjective",
    "TempestOperationRegistry",
    "TempestRun",
    "TempestRuntime",
    "TempestSkill",
    "TempestSkillRegistry",
    "TempestWorkflowSpec",
    "TempestWorkflowStep",
    "tempest_claim_provenance_abort",
    "tempest_claim_provenance_projection",
    "tempest_claim_provenance_projection_impl",
    "tempest_claim_provenance_terminal_trace",
    "tempest_costorm_artifact_catalog",
    "tempest_costorm_report_plan",
    "tempest_create_expert_delegation_tool",
    "tempest_generic_kernel_cutover_abort",
    "tempest_generic_kernel_cutover_function",
    "tempest_generic_kernel_cutover_message",
    "tempest_generic_kernel_exports",
    "tempest_mindmap_expand_node",
    "tempest_mindmap_node_sizes",
    "tempest_mindmap_oversized_nodes",
    "tempest_run_restore",
    "tempest_run_resume",
    "tempest_type_node_expansion",
    "tempest_type_turn_policy"
  )
  namespace <- asNamespace("tempest")
  removed <- c(retired_public, removed_internal)
  present <- removed[vapply(
    removed,
    exists,
    logical(1),
    envir = namespace,
    inherits = FALSE
  )]

  expect_length(retired_public, 41L)
  expect_disjoint(getNamespaceExports("tempest"), retired_public)
  expect_identical(present, character())
})

test_that("the pre-0.2 public export fixture remains historical", {
  baseline <- readLines(
    test_path("fixtures", "public-exports-0.1.0.txt"),
    warn = FALSE
  )

  expect_length(baseline, 96L)
  expect_identical(baseline, sort(unique(baseline), method = "radix"))
})

test_that("the pre-0.2 S3 registration baseline is exact", {
  methods <- getNamespaceInfo(asNamespace("tempest"), "S3methods")
  registrations <- paste(methods[, 1], methods[, 2], sep = ".")

  expect_setequal(
    registrations,
    c("print.tempest_okf_bundle", "print.tempest_okf_context")
  )
})
