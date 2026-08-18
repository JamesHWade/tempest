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

#' Tempest 0.2 generic-kernel cutover
#'
#' Tempest 0.2 supports the STORM and Co-STORM scientific-research products
#' through [tempest_run()] and [tempest_session()]. The former experimental
#' application-neutral workflow, runtime, capability, connection, skill,
#' deliverable, and artifact APIs are unavailable. Every retained symbol fails
#' immediately with class `tempest_generic_kernel_cutover_error` and identifies
#' the symbol that was called.
#'
#' This is an immediate cutover. There is no compatibility framework, fallback,
#' or generic-kernel migration layer. Retained names exist only as cutover
#' sentinels until T8 removes them physically.
#'
#' @section Unavailable API families:
#'
#' - Generic workflows: `tempest_workflow_spec()`, `tempest_workflow_step()`,
#'   `tempest_run_workflow()`, and the `tempest_storm_workflow_*()` and
#'   `tempest_costorm_workflow_*()` families.
#' - Generic runs: `tempest_run_status()`, `tempest_run_events()`,
#'   `tempest_run_approvals()`, `tempest_run_capability_grants()`,
#'   `tempest_run_artifact()`, `tempest_run_artifacts()`,
#'   `tempest_run_record_approval()`, `tempest_run_request_cancel()`,
#'   `tempest_run_snapshot()`, and `tempest_run_save()`. The shared
#'   `tempest_execution_events()` query remains a Co-STORM-only product seam
#'   for `TempestSession` event history.
#' - Runtime contracts: `tempest_operation_registry()`,
#'   `tempest_builtin_operation_registry()`,
#'   `tempest_builtin_workflow_operation_registry()`, `tempest_runtime()`,
#'   `tempest_skill()`, `tempest_skill_registry()`,
#'   `tempest_capability_spec()`, `tempest_capability_resolver()`,
#'   `tempest_connection_ref()`, and `tempest_connection_provider()`.
#' - Generic outputs: `tempest_objective()`, `tempest_deliverable_spec()`,
#'   `tempest_generate_deliverable()`, and the `tempest_artifact*()` and
#'   `tempest_*artifact_store()` families.
#'
#' @section Supported product boundary:
#'
#' Use [tempest_run()] for scripted STORM research and [tempest_session()] for
#' interactive Co-STORM research. Fixed scientific transformations carry dsprrr
#' program identity, open-ended agent work carries Deputy execution identity,
#' and accepted knowledge uses graft snapshots and commits. Execution identity
#' supports correlation and audit joins only; it does not claim that an agent
#' execution caused, authored, or validated report content.
#' @seealso [tempest_run()] and [tempest_session()] for the supported research
#'   product APIs.
#' @aliases tempest_artifact
#' @aliases tempest_artifact_catalog
#' @aliases tempest_artifact_catalog_restore
#' @aliases tempest_artifact_codec
#' @aliases tempest_artifact_codec_definition
#' @aliases tempest_artifact_codec_registry
#' @aliases tempest_artifact_representation
#' @aliases tempest_artifact_store
#' @aliases tempest_builtin_operation_registry
#' @aliases tempest_builtin_workflow_operation_registry
#' @aliases tempest_capability_resolver
#' @aliases tempest_capability_spec
#' @aliases tempest_connection_provider
#' @aliases tempest_connection_ref
#' @aliases tempest_costorm_workflow_adapter
#' @aliases tempest_costorm_workflow_run
#' @aliases tempest_costorm_workflow_spec
#' @aliases tempest_deliverable_spec
#' @aliases tempest_generate_deliverable
#' @aliases tempest_memory_artifact_store
#' @aliases tempest_objective
#' @aliases tempest_operation_registry
#' @aliases tempest_run_approvals
#' @aliases tempest_run_artifact
#' @aliases tempest_run_artifacts
#' @aliases tempest_run_capability_grants
#' @aliases tempest_run_events
#' @aliases tempest_run_record_approval
#' @aliases tempest_run_request_cancel
#' @aliases tempest_run_save
#' @aliases tempest_run_snapshot
#' @aliases tempest_run_status
#' @aliases tempest_run_workflow
#' @aliases tempest_runtime
#' @aliases tempest_skill
#' @aliases tempest_skill_registry
#' @aliases tempest_storm_workflow_adapter
#' @aliases tempest_storm_workflow_run
#' @aliases tempest_storm_workflow_spec
#' @aliases tempest_workflow_spec
#' @aliases tempest_workflow_step
#' @name tempest-generic-kernel-retirement
NULL
