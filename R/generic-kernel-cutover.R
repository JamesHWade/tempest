tempest_generic_kernel_cutover_message <- paste0(
  "Tempest 0.2 supports only the STORM and Co-STORM product APIs; ",
  "the experimental generic kernel is unavailable."
)

tempest_generic_kernel_cutover_abort <- function(symbol) {
  rlang::abort(
    tempest_generic_kernel_cutover_message,
    class = c(
      "tempest_generic_kernel_cutover_error",
      "tempest_error"
    ),
    symbol = symbol,
    call = NULL
  )
}

tempest_generic_kernel_cutover_function <- function(symbol, implementation) {
  force(symbol)
  cutover <- function(...) {
    tempest_generic_kernel_cutover_abort(symbol)
  }
  formals(cutover) <- formals(implementation)
  cutover
}

tempest_artifact <- tempest_generic_kernel_cutover_function(
  "tempest_artifact",
  tempest_artifact
)
tempest_artifact_catalog <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_catalog",
  tempest_artifact_catalog
)
tempest_artifact_catalog_restore <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_catalog_restore",
  tempest_artifact_catalog_restore
)
tempest_artifact_codec <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_codec",
  tempest_artifact_codec
)
tempest_artifact_codec_definition <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_codec_definition",
  tempest_artifact_codec_definition
)
tempest_artifact_codec_registry <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_codec_registry",
  tempest_artifact_codec_registry
)
tempest_artifact_representation <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_representation",
  tempest_artifact_representation
)
tempest_artifact_store <- tempest_generic_kernel_cutover_function(
  "tempest_artifact_store",
  tempest_artifact_store
)
tempest_builtin_operation_registry <- tempest_generic_kernel_cutover_function(
  "tempest_builtin_operation_registry",
  tempest_builtin_operation_registry
)
tempest_builtin_workflow_operation_registry <-
  tempest_generic_kernel_cutover_function(
    "tempest_builtin_workflow_operation_registry",
    tempest_builtin_workflow_operation_registry
  )
tempest_capability_resolver <- tempest_generic_kernel_cutover_function(
  "tempest_capability_resolver",
  tempest_capability_resolver
)
tempest_capability_spec <- tempest_generic_kernel_cutover_function(
  "tempest_capability_spec",
  tempest_capability_spec
)
tempest_connection_provider <- tempest_generic_kernel_cutover_function(
  "tempest_connection_provider",
  tempest_connection_provider
)
tempest_connection_ref <- tempest_generic_kernel_cutover_function(
  "tempest_connection_ref",
  tempest_connection_ref
)
tempest_costorm_workflow_adapter <- tempest_generic_kernel_cutover_function(
  "tempest_costorm_workflow_adapter",
  tempest_costorm_workflow_adapter
)
tempest_costorm_workflow_run <- tempest_generic_kernel_cutover_function(
  "tempest_costorm_workflow_run",
  tempest_costorm_workflow_run
)
tempest_costorm_workflow_spec <- tempest_generic_kernel_cutover_function(
  "tempest_costorm_workflow_spec",
  tempest_costorm_workflow_spec
)
tempest_deliverable_spec <- tempest_generic_kernel_cutover_function(
  "tempest_deliverable_spec",
  tempest_deliverable_spec
)
tempest_generate_deliverable <- tempest_generic_kernel_cutover_function(
  "tempest_generate_deliverable",
  tempest_generate_deliverable
)
tempest_memory_artifact_store <- tempest_generic_kernel_cutover_function(
  "tempest_memory_artifact_store",
  tempest_memory_artifact_store
)
tempest_objective <- tempest_generic_kernel_cutover_function(
  "tempest_objective",
  tempest_objective
)
tempest_operation_registry <- tempest_generic_kernel_cutover_function(
  "tempest_operation_registry",
  tempest_operation_registry
)
tempest_run_approvals <- tempest_generic_kernel_cutover_function(
  "tempest_run_approvals",
  tempest_run_approvals
)
tempest_run_artifact <- tempest_generic_kernel_cutover_function(
  "tempest_run_artifact",
  tempest_run_artifact
)
tempest_run_artifacts <- tempest_generic_kernel_cutover_function(
  "tempest_run_artifacts",
  tempest_run_artifacts
)
tempest_run_capability_grants <- tempest_generic_kernel_cutover_function(
  "tempest_run_capability_grants",
  tempest_run_capability_grants
)
tempest_run_events <- tempest_generic_kernel_cutover_function(
  "tempest_run_events",
  tempest_run_events
)
tempest_run_record_approval <- tempest_generic_kernel_cutover_function(
  "tempest_run_record_approval",
  tempest_run_record_approval
)
tempest_run_request_cancel <- tempest_generic_kernel_cutover_function(
  "tempest_run_request_cancel",
  tempest_run_request_cancel
)
tempest_run_restore <- tempest_generic_kernel_cutover_function(
  "tempest_run_restore",
  tempest_run_restore
)
tempest_run_resume <- tempest_generic_kernel_cutover_function(
  "tempest_run_resume",
  tempest_run_resume
)
tempest_run_save <- tempest_generic_kernel_cutover_function(
  "tempest_run_save",
  tempest_run_save
)
tempest_run_snapshot <- tempest_generic_kernel_cutover_function(
  "tempest_run_snapshot",
  tempest_run_snapshot
)
tempest_run_status <- tempest_generic_kernel_cutover_function(
  "tempest_run_status",
  tempest_run_status
)
tempest_run_workflow <- tempest_generic_kernel_cutover_function(
  "tempest_run_workflow",
  tempest_run_workflow
)
tempest_runtime <- tempest_generic_kernel_cutover_function(
  "tempest_runtime",
  tempest_runtime
)
tempest_skill <- tempest_generic_kernel_cutover_function(
  "tempest_skill",
  tempest_skill
)
tempest_skill_registry <- tempest_generic_kernel_cutover_function(
  "tempest_skill_registry",
  tempest_skill_registry
)
tempest_storm_workflow_adapter <- tempest_generic_kernel_cutover_function(
  "tempest_storm_workflow_adapter",
  tempest_storm_workflow_adapter
)
tempest_storm_workflow_run <- tempest_generic_kernel_cutover_function(
  "tempest_storm_workflow_run",
  tempest_storm_workflow_run
)
tempest_storm_workflow_spec <- tempest_generic_kernel_cutover_function(
  "tempest_storm_workflow_spec",
  tempest_storm_workflow_spec
)
tempest_workflow_spec <- tempest_generic_kernel_cutover_function(
  "tempest_workflow_spec",
  tempest_workflow_spec
)
tempest_workflow_step <- tempest_generic_kernel_cutover_function(
  "tempest_workflow_step",
  tempest_workflow_step
)
tempest_costorm_artifact_catalog <- tempest_generic_kernel_cutover_function(
  "tempest_costorm_artifact_catalog",
  tempest_costorm_artifact_catalog
)
tempest_costorm_report_plan <- tempest_generic_kernel_cutover_function(
  "tempest_costorm_report_plan",
  tempest_costorm_report_plan
)
tempest_create_expert_delegation_tool <-
  tempest_generic_kernel_cutover_function(
    "tempest_create_expert_delegation_tool",
    tempest_create_expert_delegation_tool
  )
