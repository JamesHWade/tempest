# Changelog

## tempest 0.1.0

- [`tempest_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_store.md),
  [`tempest_memory_artifact_store()`](https://jameshwade.github.io/tempest/reference/tempest_memory_artifact_store.md),
  and
  [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  provide initial host-app extension points for capturing report
  artifacts and supplying validated Co-STORM experts.
  [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  now accepts an `artifact_store`, and
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  accepts a shared retriever with a `SourceStore`.
- [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  now defaults to `openai/gpt-5.4` for coordinator and writer roles, and
  `openai/gpt-5.4-mini` for expert, mind map, and judge roles.
- The Chat tab now suggests follow-up questions as clickable cards. A
  set appears when the expert panel assembles and refreshes after each
  answer; clicking a card sends that question to the Moderator. Toggle
  it off with “Suggest follow-up questions” in the sidebar. New exported
  helper
  [`tempest_suggest_questions()`](https://jameshwade.github.io/tempest/reference/tempest_suggest_questions.md).
- [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  gains `max_search_queries_per_turn` and `retrieve_top_k` controls to
  mirror the upstream STORM runner’s query and section-retrieval limits.
- `tempest_config(search_provider = )` now accepts upstream-style
  retrievers for You.com, Bing, DuckDuckGo, SearXNG, Google Custom
  Search, and Azure AI Search in addition to the existing native,
  Wikipedia, Serper, Brave, and Tavily providers.
- `tempest` now imports dsprrr directly for STORM structured extraction
  and generation modules.
- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  now executes STORM structured steps through dsprrr modules, with
  ellmer fallbacks for module creation or runtime failures.
- [`tempest_optimize_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_optimize_dsprrr_modules.md),
  [`tempest_save_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_save_dsprrr_modules.md),
  and
  [`tempest_load_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_load_dsprrr_modules.md)
  support explicit dsprrr compilation and reuse of optimized STORM
  module sets.
- [`tempest_progress_event()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event.md)
  and
  [`tempest_progress_event_data()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event_data.md)
  define a host-neutral STORM/Co-STORM progress event contract for
  package and host-app integrations (g7wt).
- [`tempest_progress_collector()`](https://jameshwade.github.io/tempest/reference/tempest_progress_collector.md),
  [`tempest_progress_filter()`](https://jameshwade.github.io/tempest/reference/tempest_progress_filter.md),
  and
  [`tempest_progress_replay()`](https://jameshwade.github.io/tempest/reference/tempest_progress_replay.md)
  provide host-neutral in-memory progress sinks with filtering and
  replay helpers (wpt9).
- [`tempest_progress_state()`](https://jameshwade.github.io/tempest/reference/tempest_progress_state.md)
  reduces recorded STORM and Co-STORM progress events to compact
  host-neutral workflow state for UI and telemetry adapters (7f9q).
- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  gains `output_dir`, `resume`, and `run_id` arguments for persistent
  staged runs with JSON and Markdown artifacts.
- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  gains `parallel_writing` for upstream-style concurrent section writing
  with mirai.
- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  gains a `progress` callback that emits host-neutral STORM workflow
  events for stages, persistence, verification, final artifacts, and
  terminal failures (4fn5).
- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  gains `remove_duplicate` for upstream-style duplicate removal during
  the polish step.
- `tempest_run(parallel_research = )` and `parallel_writing` now start
  and stop the mirai daemons they require, fall back to sequential
  execution when workers are unavailable, and retry any failed
  perspective or section so no research or content is silently dropped.
  A transient error during sequential research is also caught and
  skipped rather than aborting the whole run.
- [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  gains a `progress` callback that emits Co-STORM session, warmup,
  expert/tool, dialogue, fact extraction, mind-map, suggestion, report,
  artifact, and failure events (qngb).
- `tempest_config(search_provider = )` searches now apply a request
  timeout and retry on transient HTTP errors, and a single missing or
  non-public result URL no longer discards every other result for the
  query.
- Co-STORM sessions now route turns, update the mind map, and summarise
  using the most recent dialogue turns instead of the oldest.
- Persisted runs now write artifacts atomically and write the run
  manifest last, so an interrupted save cannot corrupt artifacts or
  leave `resume` pointing at a stage whose output is missing.
- The bundled Shiny app now targets shinychat’s development
  `chat_server()` API for streaming, cancellation, greetings, and client
  state management.
- The bundled Shiny app now renders STORM and Co-STORM workflow progress
  from host-neutral progress events and reducer state (e08d).
- The bundled Shiny app now carries provider-native source context into
  Co-STORM fact extraction so warmup and chat turns populate Facts and
  Sources when sources were attached to the answer turn (b77g).
- The bundled Shiny app now invalidates Co-STORM progress output from
  asynchronous warmup callbacks so progress icons render while warmup is
  still running (2zbg).
- The bundled Shiny app no longer errors when async chat callbacks
  refresh the shared session store outside a reactive consumer.
- The bundled Shiny app’s Co-STORM warmup now runs independent experts
  in bounded parallel batches, shows compact progress without streaming
  every warmup answer into the chat, delays suggested questions until
  warmup finishes, times out stalled research calls, and ignores late
  callbacks from closed sessions.
- The bundled Shiny app was rebuilt around Shiny modules (one per tab)
  with a shared reactive store, runs the STORM pipeline as a background
  `ExtendedTask` bound to its task button, and adds a knowledge-stats
  value-box strip on the Mind Map tab, a landing welcome message, and an
  “About” popover linking the STORM, Co-STORM, and DSPy papers and
  upstream repositories.
