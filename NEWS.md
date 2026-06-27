# tempest 0.1.0

* `tempest_config()` gains `max_search_queries_per_turn` and `retrieve_top_k`
  controls to mirror the upstream STORM runner's query and section-retrieval
  limits.
* `tempest_config(search_provider = )` now accepts upstream-style retrievers
  for You.com, Bing, DuckDuckGo, SearXNG, Google Custom Search, and Azure AI
  Search in addition to the existing native, Wikipedia, Serper, Brave, and
  Tavily providers.
* `tempest_run()` now executes STORM structured steps through dsprrr modules
  when dsprrr is installed, with ellmer fallbacks for module creation or runtime
  failures.
* `tempest_optimize_dsprrr_modules()`, `tempest_save_dsprrr_modules()`, and
  `tempest_load_dsprrr_modules()` support explicit dsprrr compilation and
  reuse of optimized STORM module sets.
* `tempest_run()` gains `output_dir`, `resume`, and `run_id` arguments for
  persistent staged runs with JSON and Markdown artifacts.
* `tempest_run()` gains `parallel_writing` for upstream-style concurrent
  section writing with mirai.
* `tempest_run()` gains `remove_duplicate` for upstream-style duplicate
  removal during the polish step.
* The bundled Shiny app now targets shinychat's development `chat_server()`
  API for streaming, cancellation, greetings, and client state management.
