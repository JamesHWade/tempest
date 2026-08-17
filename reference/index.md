# Package index

## Agent integrations

- [`tempest_agent_skills()`](https://jameshwade.github.io/tempest/reference/tempest_agent_skills.md)
  [`tempest_install_agent_skills()`](https://jameshwade.github.io/tempest/reference/tempest_agent_skills.md)
  : Discover and install bundled Agent Skills

## Core workflows

Run scripted STORM or persistent Deputy-backed Co-STORM research.

- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  : Run the STORM pipeline
- [`tempest_run_async()`](https://jameshwade.github.io/tempest/reference/tempest_run_async.md)
  : Run STORM asynchronously (Shiny-friendly)
- [`tempest_run_cancel()`](https://jameshwade.github.io/tempest/reference/tempest_run_cancel.md)
  : Cancel an asynchronous STORM run
- [`tempest_task()`](https://jameshwade.github.io/tempest/reference/tempest_task.md)
  : Create a vitals Task for tempest
- [`tempest_costorm_task()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_task.md)
  : Create a Co-STORM evaluation task using SimulatedUser
- [`tempest_execution_events()`](https://jameshwade.github.io/tempest/reference/tempest_execution_events.md)
  **\[experimental\]** : Query events from a Tempest execution
- [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  : Create a Co-STORM session
- [`tempest_session_process_turn_async()`](https://jameshwade.github.io/tempest/reference/tempest_session_process_turn_async.md)
  **\[experimental\]** : Process a completed Co-STORM turn
  asynchronously
- [`tempest_session_warmup_async()`](https://jameshwade.github.io/tempest/reference/tempest_session_warmup_async.md)
  **\[experimental\]** : Warm up a Co-STORM session asynchronously
- [`SimulatedUser`](https://jameshwade.github.io/tempest/reference/SimulatedUser.md)
  : SimulatedUser
- [`run_app()`](https://jameshwade.github.io/tempest/reference/run_app.md)
  : Run the tempest Shiny chat application

## Configuration and retrieval

- [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  : Create a STORM configuration
- [`tempest_retriever()`](https://jameshwade.github.io/tempest/reference/tempest_retriever.md)
  : Create a TempestRetriever
- [`TempestRetriever`](https://jameshwade.github.io/tempest/reference/TempestRetriever.md)
  : TempestRetriever
- [`tempest_create_ragnar_store()`](https://jameshwade.github.io/tempest/reference/tempest_create_ragnar_store.md)
  : Create a ragnar store with tempest metadata schema
- [`tempest_cache_clear()`](https://jameshwade.github.io/tempest/reference/tempest_cache_clear.md)
  : Clear the Tempest cache

## Research identity and workspace

- [`tempest_research_manifest()`](https://jameshwade.github.io/tempest/reference/tempest_research_manifest.md)
  : Create a Tempest research manifest
- [`tempest_research_workspace()`](https://jameshwade.github.io/tempest/reference/tempest_research_workspace.md)
  : Create a provisional research workspace
- [`ResearchWorkspace`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  : ResearchWorkspace (provisional scientific evidence ledger)

## Evidence and reports

- [`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md)
  : Return claims as a tibble
- [`tempest_claim_support()`](https://jameshwade.github.io/tempest/reference/tempest_claim_support.md)
  : Create an explicit claim-support assessment
- [`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md)
  : List explicit claim-support assessments
- [`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md)
  : Return evidence resources as a tibble
- [`tempest_resource()`](https://jameshwade.github.io/tempest/reference/tempest_resource.md)
  **\[experimental\]** : Create a typed evidence resource
- [`tempest_verify_claims()`](https://jameshwade.github.io/tempest/reference/tempest_verify_claims.md)
  : Verify claim citations against their sources
- [`tempest_validation_result()`](https://jameshwade.github.io/tempest/reference/tempest_validation_result.md)
  **\[experimental\]** : Create a Tempest validation result
- [`tempest_report_md()`](https://jameshwade.github.io/tempest/reference/tempest_report_md.md)
  : Assemble a Markdown report with footnotes
- [`tempest_session_report_md()`](https://jameshwade.github.io/tempest/reference/tempest_session_report_md.md)
  : Assemble a Markdown report from a Co-STORM session
- [`tempest_suggest_questions()`](https://jameshwade.github.io/tempest/reference/tempest_suggest_questions.md)
  : Suggest follow-up research questions for a topic

## Open knowledge

- [`tempest_read_okf()`](https://jameshwade.github.io/tempest/reference/tempest_read_okf.md)
  : Read an Open Knowledge Format bundle
- [`tempest_okf_concepts()`](https://jameshwade.github.io/tempest/reference/tempest_okf_concepts.md)
  : Inspect concepts in an Open Knowledge Format bundle
- [`tempest_okf_resources()`](https://jameshwade.github.io/tempest/reference/tempest_okf_resources.md)
  : Convert Open Knowledge Format concepts to typed Tempest resources
- [`tempest_okf_context()`](https://jameshwade.github.io/tempest/reference/tempest_okf_context.md)
  : Assemble bounded agent context from an Open Knowledge Format bundle

## Scientific experts

- [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  **\[experimental\]** : Create a Tempest expert profile
- [`tempest_generate_experts()`](https://jameshwade.github.io/tempest/reference/tempest_generate_experts.md)
  : Generate expert profiles for a topic

## Research UI

- [`tempest_shiny_store()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_store.md)
  **\[experimental\]** : Create a shared Tempest Shiny store
- [`tempest_shiny_ui()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_ui.md)
  **\[experimental\]** : Embed Tempest panels in a Shiny UI
- [`tempest_shiny_server()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_server.md)
  **\[experimental\]** : Run embedded Tempest Shiny panels

## Retiring experimental generic kernel

Deletion-owned baseline scheduled for the section-10 PR.

- [`tempest-generic-kernel-retirement`](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md)
  **\[experimental\]** : Retirement of Tempest's experimental generic
  kernel
- [`tempest_objective()`](https://jameshwade.github.io/tempest/reference/tempest_objective.md)
  **\[experimental\]** : Create a Tempest objective
- [`tempest_deliverable_spec()`](https://jameshwade.github.io/tempest/reference/tempest_deliverable_spec.md)
  **\[experimental\]** : Create a Tempest deliverable specification
- [`tempest_artifact()`](https://jameshwade.github.io/tempest/reference/tempest_artifact.md)
  **\[experimental\]** : Create a typed Tempest artifact
- [`tempest_artifact_representation()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_representation.md)
  **\[experimental\]** : Describe one renderer-produced artifact
  representation
- [`tempest_artifact_catalog()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_catalog.md)
  **\[experimental\]** : Create a typed Tempest artifact catalog
- [`tempest_artifact_catalog_restore()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_catalog_restore.md)
  **\[experimental\]** : Restore a typed Tempest artifact catalog
- [`tempest_artifact_codec_definition()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_codec_definition.md)
  [`tempest_artifact_codec()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_codec_definition.md)
  **\[experimental\]** : Define a typed artifact codec
- [`tempest_artifact_codec_registry()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_codec_registry.md)
  **\[experimental\]** : Create a typed artifact codec registry
- [`tempest_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_store.md)
  **\[experimental\]** : Create a Tempest artifact store adapter
- [`tempest_memory_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_memory_artifact_store.md)
  **\[experimental\]** : Create an in-memory Tempest artifact store
- [`tempest_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_operation_registry.md)
  **\[experimental\]** : Create a Tempest runtime operation registry
- [`tempest_builtin_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_builtin_operation_registry.md)
  **\[experimental\]** : Create a registry containing Tempest's built-in
  deliverable operations
- [`tempest_workflow_step()`](https://jameshwade.github.io/tempest/reference/tempest_workflow_step.md)
  **\[experimental\]** : Create a Tempest workflow step
- [`tempest_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_workflow_spec.md)
  **\[experimental\]** : Create a Tempest workflow specification
- [`tempest_storm_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_spec.md)
  **\[experimental\]** : Create the built-in STORM workflow
  specification
- [`tempest_costorm_workflow_spec()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_spec.md)
  **\[experimental\]** : Create the built-in Co-STORM workflow
  specification
- [`tempest_builtin_workflow_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_builtin_workflow_operation_registry.md)
  **\[experimental\]** : Create a registry for Tempest's built-in
  research workflows
- [`tempest_generate_deliverable()`](https://jameshwade.github.io/tempest/reference/tempest_generate_deliverable.md)
  **\[experimental\]** : Generate and finalize a Tempest deliverable
- [`tempest_run_workflow()`](https://jameshwade.github.io/tempest/reference/tempest_run_workflow.md)
  **\[experimental\]** : Execute an application-neutral Tempest workflow
- [`tempest_storm_workflow_run()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_run.md)
  **\[experimental\]** : Run STORM through the generic Tempest workflow
  kernel
- [`tempest_costorm_workflow_run()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_run.md)
  **\[experimental\]** : Attach and start the generic workflow for a
  Co-STORM session
- [`tempest_run_status()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_events()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_approvals()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_artifacts()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_capability_grants()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_artifact()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_record_approval()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  [`tempest_run_request_cancel()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
  **\[experimental\]** : Inspect and control a generic Tempest run
- [`tempest_run_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_run_snapshot.md)
  **\[experimental\]** : Snapshot generic Tempest run state
- [`tempest_run_save()`](https://jameshwade.github.io/tempest/reference/tempest_run_save.md)
  **\[experimental\]** : Save a generic Tempest run bundle
- [`tempest_skill()`](https://jameshwade.github.io/tempest/reference/tempest_skill.md)
  **\[experimental\]** : Create a Tempest skill specification
- [`tempest_skill_registry()`](https://jameshwade.github.io/tempest/reference/tempest_skill_registry.md)
  **\[experimental\]** : Create a Tempest skill registry
- [`tempest_capability_spec()`](https://jameshwade.github.io/tempest/reference/tempest_capability_spec.md)
  **\[experimental\]** : Create a Tempest capability specification
- [`tempest_capability_resolver()`](https://jameshwade.github.io/tempest/reference/tempest_capability_resolver.md)
  **\[experimental\]** : Create a Tempest capability resolver
- [`tempest_connection_ref()`](https://jameshwade.github.io/tempest/reference/tempest_connection_ref.md)
  **\[experimental\]** : Create an opaque Tempest connection reference
- [`tempest_connection_provider()`](https://jameshwade.github.io/tempest/reference/tempest_connection_provider.md)
  **\[experimental\]** : Create a Tempest runtime connection provider
- [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
  **\[experimental\]** : Create a Tempest runtime
- [`tempest_storm_workflow_adapter()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_adapter.md)
  **\[experimental\]** : Create an adapter for the built-in STORM
  workflow
- [`tempest_costorm_workflow_adapter()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_adapter.md)
  **\[experimental\]** : Create an adapter for the built-in Co-STORM
  workflow

## Session persistence

- [`tempest_session_snapshot()`](https://jameshwade.github.io/tempest/reference/tempest_session_snapshot.md)
  **\[experimental\]** : Snapshot a Co-STORM session
- [`tempest_session_restore()`](https://jameshwade.github.io/tempest/reference/tempest_session_restore.md)
  **\[experimental\]** : Restore a Co-STORM session from a snapshot
- [`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
  **\[experimental\]** : Save a Co-STORM session bundle
- [`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md)
  : Resume a saved Co-STORM session bundle

## Progress events

- [`tempest_progress_event()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event.md)
  **\[experimental\]** : Create a Tempest progress event
- [`tempest_progress_event_data()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event_data.md)
  : Convert a Tempest progress event to a list
- [`tempest_progress_collector()`](https://jameshwade.github.io/tempest/reference/tempest_progress_collector.md)
  **\[experimental\]** : Create an in-memory Tempest progress event
  collector
- [`tempest_progress_filter()`](https://jameshwade.github.io/tempest/reference/tempest_progress_filter.md)
  **\[experimental\]** : Filter Tempest progress events
- [`tempest_progress_replay()`](https://jameshwade.github.io/tempest/reference/tempest_progress_replay.md)
  **\[experimental\]** : Replay Tempest progress events to a callback
- [`tempest_progress_state()`](https://jameshwade.github.io/tempest/reference/tempest_progress_state.md)
  **\[experimental\]** : Reduce Tempest progress events to workflow
  state
- [`tempest_progress_labels()`](https://jameshwade.github.io/tempest/reference/tempest_progress_labels.md)
  : Progress labels for Tempest workflows

## Governed programs

- [`tempest_program_set()`](https://jameshwade.github.io/tempest/reference/tempest_program_set.md)
  : Create a validated Tempest program set
- [`tempest_compile_programs()`](https://jameshwade.github.io/tempest/reference/tempest_compile_programs.md)
  : Compile programs in a Tempest program set
- [`tempest_save_program_set()`](https://jameshwade.github.io/tempest/reference/tempest_save_program_set.md)
  : Save a Tempest program set
- [`tempest_load_program_set()`](https://jameshwade.github.io/tempest/reference/tempest_load_program_set.md)
  : Load and verify a Tempest program set
- [`tempest_governed_procedure_ref()`](https://jameshwade.github.io/tempest/reference/tempest_governed_procedure_ref.md)
  : Reference an accepted governed procedure

## Graft review and promotion

- [`tempest_graft_schema()`](https://jameshwade.github.io/tempest/reference/tempest_graft_schema.md)
  : Load Tempest's compiled scientific Graft schema
- [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  : Build a deterministic proposal for reviewed Graft promotion
- [`tempest_save_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_save_promotion_bundle.md)
  : Save a Tempest promotion bundle atomically
- [`tempest_read_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_read_promotion_bundle.md)
  : Read and validate a current Tempest promotion bundle
- [`tempest_graft_plan()`](https://jameshwade.github.io/tempest/reference/tempest_graft_plan.md)
  : Plan a Tempest research promotion without accepting it
- [`tempest_promotion_receipt()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_receipt.md)
  : Record exact accepted revisions for a committed promotion plan
