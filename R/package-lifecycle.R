tempest_generic_kernel_exports <- c(
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
  "tempest_run_restore",
  "tempest_run_resume",
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

#' Retirement of Tempest's experimental generic kernel
#'
#' `r lifecycle::badge("experimental")`
#'
#' Tempest 0.2 becomes a focused scientific-research product. The experimental
#' application-neutral workflow, runtime, capability, connection, skill,
#' deliverable, and artifact APIs are frozen and scheduled for removal in
#' Tempest 0.2.0. They remain available in the 0.1 development line only to
#' preserve the product baseline while replacement seams are proven.
#'
#' No compatibility framework or generic-kernel migration layer will be added.
#' STORM and Co-STORM product bundles will receive direct, product-specific
#' replacements before the generic implementation is deleted.
#'
#' @section Retirement:
#' This experimental API is frozen and scheduled for removal in Tempest 0.2.0.
#' No compatibility shim is planned.
#'
#' @section Scheduled API families:
#'
#' - Generic workflows: `tempest_workflow_spec()`, `tempest_workflow_step()`,
#'   `tempest_run_workflow()`, and the `tempest_storm_workflow_*()` and
#'   `tempest_costorm_workflow_*()` families.
#' - Generic runs: `tempest_run_status()`, `tempest_run_events()`,
#'   `tempest_run_approvals()`, `tempest_run_capability_grants()`,
#'   `tempest_run_artifact()`, `tempest_run_artifacts()`,
#'   `tempest_run_record_approval()`, `tempest_run_request_cancel()`,
#'   `tempest_run_snapshot()`, `tempest_run_restore()`, `tempest_run_save()`,
#'   and `tempest_run_resume()`. The shared `tempest_execution_events()` query
#'   remains a product seam and will be narrowed to STORM and Co-STORM state.
#' - Runtime contracts: `tempest_operation_registry()`,
#'   `tempest_builtin_operation_registry()`,
#'   `tempest_builtin_workflow_operation_registry()`, `tempest_runtime()`,
#'   `tempest_skill()`, `tempest_skill_registry()`,
#'   `tempest_capability_spec()`, `tempest_capability_resolver()`,
#'   `tempest_connection_ref()`, and `tempest_connection_provider()`.
#' - Generic outputs: `tempest_objective()`, `tempest_deliverable_spec()`,
#'   `tempest_generate_deliverable()`, and the `tempest_artifact*()` and
#'   `tempest_*artifact_store()` families. Product validation results remain
#'   available for report bundles.
#'
#' @section Replacement direction:
#'
#' Fixed scientific transformations become dsprrr program references;
#' open-ended agent work uses Deputy; accepted knowledge uses graft snapshots
#' and commits; provisional scientific evidence remains in Tempest; and reports
#' and promotion plans become product-specific bundles.
#'
#' @name tempest-generic-kernel-retirement
NULL
