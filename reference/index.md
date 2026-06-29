# Package index

## Core workflows

- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  : Run the STORM pipeline
- [`tempest_run_async()`](https://jameshwade.github.io/tempest/reference/tempest_run_async.md)
  : Run STORM asynchronously (Shiny-friendly)
- [`tempest_task()`](https://jameshwade.github.io/tempest/reference/tempest_task.md)
  : Create a vitals Task for tempest
- [`tempest_costorm_task()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_task.md)
  : Create a Co-STORM evaluation task using SimulatedUser
- [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  : Create a Co-STORM session
- [`TempestSession`](https://jameshwade.github.io/tempest/reference/TempestSession.md)
  : TempestSession
- [`SimulatedUser`](https://jameshwade.github.io/tempest/reference/SimulatedUser.md)
  : SimulatedUser
- [`run_app()`](https://jameshwade.github.io/tempest/reference/run_app.md)
  : Run the tempest Shiny chat application

## Configuration and retrieval

- [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  : Create a STORM configuration
- [`tempest_generate_personas()`](https://jameshwade.github.io/tempest/reference/tempest_generate_personas.md)
  : Generate Expert Personas for a Topic
- [`tempest_retriever()`](https://jameshwade.github.io/tempest/reference/tempest_retriever.md)
  : Create a TempestRetriever
- [`TempestRetriever`](https://jameshwade.github.io/tempest/reference/TempestRetriever.md)
  : TempestRetriever
- [`tempest_create_ragnar_store()`](https://jameshwade.github.io/tempest/reference/tempest_create_ragnar_store.md)
  : Create a ragnar store with tempest metadata schema
- [`tempest_cache_clear()`](https://jameshwade.github.io/tempest/reference/tempest_cache_clear.md)
  : Clear the Tempest cache
- [`SourceStore`](https://jameshwade.github.io/tempest/reference/SourceStore.md)
  : SourceStore (evidence ledger)

## Evidence and reports

- [`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md)
  : Return claims as a tibble
- [`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md)
  : Return sources as a tibble
- [`tempest_verify_claims()`](https://jameshwade.github.io/tempest/reference/tempest_verify_claims.md)
  : Verify claim citations against their sources
- [`tempest_report_md()`](https://jameshwade.github.io/tempest/reference/tempest_report_md.md)
  : Assemble a Markdown report with footnotes
- [`tempest_session_report_md()`](https://jameshwade.github.io/tempest/reference/tempest_session_report_md.md)
  : Assemble a Markdown report from a Co-STORM session
- [`tempest_suggest_questions()`](https://jameshwade.github.io/tempest/reference/tempest_suggest_questions.md)
  : Suggest follow-up research questions for a topic

## Host app extension points

- [`tempest_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_store.md)
  **\[experimental\]** : Create a Tempest artifact store adapter
- [`tempest_memory_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_memory_artifact_store.md)
  **\[experimental\]** : Create an in-memory Tempest artifact store
- [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  **\[experimental\]** : Create a Co-STORM expert definition
- [`tempest_shiny_store()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_store.md)
  **\[experimental\]** : Create a shared Tempest Shiny store
- [`tempest_shiny_ui()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_ui.md)
  **\[experimental\]** : Embed Tempest panels in a Shiny UI
- [`tempest_shiny_server()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_server.md)
  **\[experimental\]** : Run embedded Tempest Shiny panels

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

## dsprrr modules

- [`tempest_load_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_load_dsprrr_modules.md)
  **\[experimental\]** : Load compiled dsprrr modules
- [`tempest_optimize_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_optimize_dsprrr_modules.md)
  **\[experimental\]** : Optimize STORM dsprrr modules
- [`tempest_save_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_save_dsprrr_modules.md)
  **\[experimental\]** : Save compiled dsprrr modules
