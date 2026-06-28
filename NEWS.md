# tempest 0.1.0

* `tempest_artifact_store()`, `tempest_memory_artifact_store()`, and
  `tempest_expert()` provide initial host-app extension points for capturing
  report artifacts and supplying validated Co-STORM experts. `tempest_config()`
  now accepts an `artifact_store`, and `tempest_session()` accepts a shared
  retriever with a `SourceStore`.
* The Chat tab now suggests follow-up questions as clickable cards. A set appears
  when the expert panel assembles and refreshes after each answer; clicking a card
  sends that question to the Moderator. Toggle it off with "Suggest follow-up
  questions" in the sidebar. New exported helper `tempest_suggest_questions()`.
* `tempest_config()` gains `max_search_queries_per_turn` and `retrieve_top_k`
  controls to mirror the upstream STORM runner's query and section-retrieval
  limits.
* `tempest_config(search_provider = )` now accepts upstream-style retrievers
  for You.com, Bing, DuckDuckGo, SearXNG, Google Custom Search, and Azure AI
  Search in addition to the existing native, Wikipedia, Serper, Brave, and
  Tavily providers.
* `tempest` now imports dsprrr directly for STORM structured extraction and
  generation modules.
* `tempest_run()` now executes STORM structured steps through dsprrr modules,
  with ellmer fallbacks for module creation or runtime failures.
* `tempest_optimize_dsprrr_modules()`, `tempest_save_dsprrr_modules()`, and
  `tempest_load_dsprrr_modules()` support explicit dsprrr compilation and
  reuse of optimized STORM module sets.
* `tempest_run()` gains `output_dir`, `resume`, and `run_id` arguments for
  persistent staged runs with JSON and Markdown artifacts.
* `tempest_run()` gains `parallel_writing` for upstream-style concurrent
  section writing with mirai.
* `tempest_run()` gains `remove_duplicate` for upstream-style duplicate
  removal during the polish step.
* `tempest_run(parallel_research = )` and `parallel_writing` now start and
  stop the mirai daemons they require, fall back to sequential execution when
  workers are unavailable, and retry any failed perspective or section so no
  research or content is silently dropped. A transient error during sequential
  research is also caught and skipped rather than aborting the whole run.
* `tempest_config(search_provider = )` searches now apply a request timeout
  and retry on transient HTTP errors, and a single missing or non-public
  result URL no longer discards every other result for the query.
* Co-STORM sessions now route turns, update the mind map, and summarise using
  the most recent dialogue turns instead of the oldest.
* Persisted runs now write artifacts atomically and write the run manifest
  last, so an interrupted save cannot corrupt artifacts or leave `resume`
  pointing at a stage whose output is missing.
* The bundled Shiny app now targets shinychat's development `chat_server()`
  API for streaming, cancellation, greetings, and client state management.
* The bundled Shiny app no longer errors when async chat callbacks refresh the
  shared session store outside a reactive consumer.
* The bundled Shiny app's Co-STORM warmup now runs independent experts in
  bounded parallel batches, shows compact progress without streaming every
  warmup answer into the chat, delays suggested questions until warmup finishes,
  times out stalled research calls, and ignores late callbacks from closed
  sessions.
* The bundled Shiny app was rebuilt around Shiny modules (one per tab) with a
  shared reactive store, runs the STORM pipeline as a background `ExtendedTask`
  bound to its task button, and adds a knowledge-stats value-box strip on the
  Mind Map tab, a landing welcome message, and an "About" popover linking the
  STORM, Co-STORM, and DSPy papers and upstream repositories.
