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
- [`tempest_execution_events()`](https://jameshwade.github.io/tempest/reference/tempest_execution_events.md)
  **\[experimental\]** : Query events from a Tempest product execution
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
  : Run the Tempest research application

## Product evaluation

Evaluate complete STORM and Co-STORM research products with vitals.

- [`tempest_task()`](https://jameshwade.github.io/tempest/reference/tempest_task.md)
  : Create a vitals Task for tempest
- [`tempest_costorm_task()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_task.md)
  : Create a Co-STORM evaluation task using SimulatedUser

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

Inspect evidence, render Markdown, or read a committed product report.

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
  : Render non-authoritative Markdown with footnotes
- [`tempest_session_report_md()`](https://jameshwade.github.io/tempest/reference/tempest_session_report_md.md)
  : Read the committed Markdown report from a Co-STORM session
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

Embed product-specific, asynchronous Tempest research panels in Shiny.

- [`tempest_shiny_store()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_store.md)
  **\[experimental\]** : Create a shared Tempest Shiny store
- [`tempest_shiny_ui()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_ui.md)
  **\[experimental\]** : Embed Tempest panels in a Shiny UI
- [`tempest_shiny_server()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_server.md)
  **\[experimental\]** : Run embedded Tempest Shiny panels

## Co-STORM persistence

Snapshot, save, restore, or resume the exact current session product.

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
