# tempest 0.1.0

* Persistence bundles from before the reusable workflow-kernel cutover are intentionally unsupported: Co-STORM restore requires schema version 4, and staged STORM restore requires schema version 3 with typed artifact indexes; completed perspective stages persist fingerprinted expert profiles (vtvt, wgr8).
* The bundled Shiny app now escapes untrusted model and source content, rejects unsafe links, isolates browser session storage behind upload/download archives with quotas and private permissions, provides cancellable STORM workers, runs Co-STORM expert generation, enrichment, suggestions, and reports asynchronously with provider-correct requests and ordered stale-safe commits, quarantines timed-out expert chats, and exposes an accessible keyboard-operable mind-map outline (da4c, 16dv, pyxm, qx4q, t593, gcg1).
* The bundled Shiny app now presents Co-STORM setup as a one-time native shinychat greeting with consistently sized controls, lets users replace generated panels with up to five named expert perspectives, and reserves the footer for active-session status and actions.
* `run_app()` now inherits the package-level `tempest.chat` default while its model fields remain untouched; editing any model field switches the app to explicit per-role models. Tempest once again declares the shinychat development dependency required by the app and reports how to update an incompatible loaded version before launching.
* `SourceStore` now validates source, claim, evidence-span, and dispute mutations, rejects orphan references and source-budget overflow, and keeps reverse indexes correct when claims are replaced; the unused parallel S7 source representation was removed (67h9, y2kw).
* `SourceStore` and `tempest_resource()` now support fingerprinted web, file, message, database, and host-defined evidence resources with opaque locators, connection provenance, redaction and retention metadata, and durable claim lineage without requiring public URLs (fr54).
* `tempest_agent_skills()` and `tempest_install_agent_skills()` expose and install five bundled Agent Skills for using Tempest STORM and Co-STORM, conducting the portable research protocol in other hosts, and designing, building, or verifying custom Tempest workflows (cheg).
* `tempest_artifact()`, `tempest_artifact_catalog()`, `tempest_artifact_store()`, `tempest_deliverable_spec()`, `tempest_generate_deliverable()`, `tempest_objective()`, and `tempest_operation_registry()` add an application-neutral output lifecycle with versioned operation references, validation, evidence lineage, typed multi-format artifacts, approval-before-export enforcement, safe invalid-output retries, host storage adapters, and checksummed persistence shared by STORM, Co-STORM, and host-defined outcomes (xbwy).
* `tempest_artifact_codec_registry()` and `tempest_artifact_store()` provide versioned runtime codecs and complete typed storage adapters for inline or external custom artifact representations without serializing executable codec functions (fr54).
* `tempest_config()` now validates scalar model, logical, search, source, expert, query, and retrieval budgets immediately with classed errors, and provider tools cannot exceed configured search limits (y2kw).
* `tempest_config()` now honors the `tempest.chat` R option for a default provider/model string or cloneable ellmer Chat; explicit `models` and `chat_fn` arguments still take precedence, and role-specific prompts are preserved (serz).
* `tempest_config()` now creates its built-in OpenAI clients with `ellmer::chat_openai(auth = "codex")`, reusing file-backed ChatGPT subscription authentication managed by Codex CLI; custom chat options and factories remain available for API-key and alternative-provider access (tidyverse/ellmer#1067).
* `tempest_create_ragnar_store()` preserves and validates compatible persistent stores by default; destructive replacement now requires `reset = TRUE` (yb8s).
* `tempest_costorm_workflow_run()` and `tempest_storm_workflow_run()` expose the built-in research workflows as executable `TempestRun` specifications with shared evidence, typed checkpoints, selected expert teams, scoped connections, and an approval-gated Co-STORM dialogue boundary (vtvt).
* `tempest_execution_events()` provides one cursor-based event query for `TempestRun` and `TempestSession`; Co-STORM sessions now own normalized event history directly, and persistence migrates legacy progress history out of the auxiliary artifact environment.
* `tempest_report_md()` now applies strict citation actions to complete matched assertions, removes unsupported assertions under `drop`, gives `revise` distinct behavior, and no longer lets unrelated claims sharing a source determine sentence status (e9kn).
* `tempest_run_async()` now executes in a real Mirai worker, preserves result and error semantics, returns without blocking, and can be stopped with `tempest_run_cancel()` (4303).
* `tempest_session_save()` now replaces bundles atomically from a complete sibling staging directory, records SHA-256 checksums for every declared file, and `tempest_session_resume()` verifies manifest completeness and integrity with explicit `partial_recovery` support (arqh).
* Retriever URL fetching now parses and resolves destinations structurally, rejects non-public and obfuscated IPv4/IPv6 targets, revalidates redirects, and enforces timeout, redirect, content-type, and body-size limits; cache writes are atomic, corrupt entries are quarantined, and transient fetch failures are retried instead of cached indefinitely (vbg7, cax6).
* Search-provider, evaluation-task, app-startup, asynchronous-run, and UI integration boundaries now have deterministic fixture-based contract coverage without API keys or network access (kqgx).
* `tempest_expert()`, `tempest_skill()`, `tempest_capability_spec()`, `tempest_connection_ref()`, and `tempest_runtime()` add stable expert profiles, reusable skill contracts, per-context least-privilege capability resolution, opaque host connection bindings, exact expert-session restoration, and durable per-step grant records; global all-chat tool registration and the `tempest_config(tools = )` argument have been removed, so hosts should attach tool implementations through scoped runtime capabilities (wgr8).
* `tempest_artifact_store()`, `tempest_expert()`, `tempest_progress_event()`, `tempest_session_save()`, `tempest_shiny_ui()`, and related host-app APIs now carry explicit experimental lifecycle badges, and the pkgdown reference groups separate core workflows, evidence, host extension points, persistence, progress, and dsprrr modules (2nzw).
* `tempest_claims()` now preserves support scores from structured fact extraction, and `tempest_sources()` now includes derived `context_text` plus fallback snippets for source rows backed by content or provider-native citation context (w3fm, fmbv).
* `tempest_config()` now defaults to `openai/gpt-5.6-sol` for coordinator and writer roles, and `openai/gpt-5.6-luna` for expert, mind map, and judge roles. Built-in subscription clients use lower reasoning effort for auxiliary mind-map and judge calls, and `run_app()` bounds provider requests to 120 seconds by default.
* `tempest_config()`, `tempest_retriever()`, and session persistence now use catchable cli/rlang condition classes for invalid providers, missing search-provider environment variables, unsafe URLs, chat setup failures, invalid artifact stores, and invalid session objects, all sharing a common `tempest_error` base class (and a `tempest_persistence_error` base for save/load failures) so callers can catch them with a single handler (9c6a).
* The Chat tab now suggests follow-up questions as clickable cards using shinychat's recognized suggestion markup. A set appears when the expert panel assembles and refreshes after each answer; clicking a card sends that question to the Moderator. Toggle it off with "Suggest follow-up questions" in the sidebar. New exported helper `tempest_suggest_questions()`.
* `tempest_config()` gains `max_search_queries_per_turn` and `retrieve_top_k`
  controls to mirror the upstream STORM runner's query and section-retrieval
  limits.
* `tempest_config(search_provider = )` now accepts upstream-style retrievers
  for You.com, Bing, DuckDuckGo, SearXNG, Google Custom Search, and Azure AI
  Search in addition to the existing native, Wikipedia, Serper, Brave, and
  Tavily providers.
* `tempest_config()` gains `cache_enabled` and `cache_ttl` controls for retriever search/fetch caching, `TempestRetriever$cache_stats()` reports local cache counters, and `tempest_cache_clear()` clears all or stale cache entries (ryfx).
* `tempest` now imports dsprrr directly for STORM structured extraction and
  generation modules.
* `tempest_run()` now executes STORM structured steps through dsprrr modules,
  with ellmer fallbacks for module creation or runtime failures.
* `tempest_run_save()`, `tempest_run_resume()`, and the `tempest_run_*()` accessors give host applications checksummed generic run bundles plus stable status, event, approval, per-attempt capability-grant, artifact, cancellation, strict restore-integrity, permission-narrowing, and explicit partial-recovery controls without reaching into mutable R6 internals (vtvt).
* `tempest_run_workflow()`, `tempest_workflow_spec()`, and `tempest_workflow_step()` add a generic deterministic run shell with expert assignments, bounded retry history, ordered events, typed artifacts, requested-output completion checks, pre-execution policy gates, post-generation output approvals, cooperative cancellation, process-local runtime services, and explicit runtime snapshot restoration (vtvt).
* `tempest_run()` and `tempest_session()` now pass source context into dsprrr claim extraction for provider-native citations, so optimized extraction works for native web-search turns as well as inline `[S...]` citations (c8jk).
* `tempest_run()` and `tempest_session()` now expose evidence review tools for agents to inspect claims, cited sources, evidence spans, and unsupported claims without requiring write access (my3y).
* `tempest_run()` and `tempest_session()` now expose claim-oriented `add_claim` and `list_claims` agent tools while keeping `add_fact` and `list_facts` as transitional aliases (msg3).
* `tempest_okf_concepts()`, `tempest_okf_context()`, `tempest_okf_resources()`, and `tempest_read_okf()` read bounded Open Knowledge Format bundles as typed evidence resources and explicitly untrusted agent context without executing referenced code or granting capabilities.
* `tempest_optimize_dsprrr_modules()`, `tempest_save_dsprrr_modules()`, and `tempest_load_dsprrr_modules()` now expose per-module dsprrr optimizer controls and agentic teleprompter arguments, persist modules as atomic checksummed bundles of versioned dsprrr program artifacts instead of raw RDS lists, and remove the obsolete `fact_extraction` module alias.
* `tempest_progress_event()` and `tempest_progress_event_data()` define a host-neutral STORM/Co-STORM progress event contract for package and host-app integrations (g7wt).
* `tempest_progress_labels()` provides compact host-neutral STORM and Co-STORM progress labels for stage chips and current-step displays (a3rg).
* `tempest_progress_collector()`, `tempest_progress_filter()`, and `tempest_progress_replay()` provide host-neutral in-memory progress sinks with filtering and replay helpers (wpt9).
* `tempest_progress_state()` reduces recorded STORM and Co-STORM progress events to compact host-neutral workflow state for UI and telemetry adapters (7f9q).
* `tempest_run()` gains `output_dir`, `resume`, and `run_id` arguments for
  persistent staged runs with JSON and Markdown artifacts.
* `tempest_run()` gains `parallel_writing` for upstream-style concurrent
  section writing with mirai.
* `tempest_run()` gains a `progress` callback that emits host-neutral STORM workflow events for stages, persistence, verification, final artifacts, and terminal failures (4fn5).
* `tempest_run()` gains `remove_duplicate` for upstream-style duplicate
  removal during the polish step.
* `tempest_run(parallel_research = )` and `parallel_writing` now start and
  stop the mirai daemons they require, fall back to sequential execution when
  workers are unavailable, and retry any failed perspective or section so no
  research or content is silently dropped. A transient error during sequential
  research is also caught and skipped rather than aborting the whole run.
* `tempest_session()` gains a `progress` callback that emits Co-STORM session, warmup, expert/tool, dialogue, fact extraction, mind-map, suggestion, report, artifact, and failure events (qngb).
* `tempest_session()` now accepts `session_id` so host apps can align Co-STORM progress, persistence, and artifacts with their own project/session identity (z0e0).
* `tempest_session_process_turn_async()` records completed user and moderator turns and performs evidence, mind-map, and suggestion enrichment asynchronously, returning a typed stale-safe result with host-presentable notices.
* `tempest_session_save()`, `tempest_session_resume()`, `tempest_session_snapshot()`, and `tempest_session_restore()` provide schema-versioned Co-STORM session persistence bundles and in-memory snapshot/restore helpers (zj62). `tempest_session_save(overwrite = TRUE)` only replaces directories that already look like a Tempest session bundle, so a mistyped path cannot recursively delete unrelated files.
* `tempest_session_warmup_async()` runs bounded expert orientations in parallel and commits transcript, evidence, and one mind-map update in deterministic order with per-expert timeouts and stale-result suppression.
* `tempest_shiny_ui()`, `tempest_shiny_server()`, and `tempest_shiny_store()` provide an experimental embeddable Shiny adapter for host apps, with fake-chat example code under `inst/examples/shiny-host` (z0e0).
* `tempest_shiny_server()` now exposes reactive generic run, event, approval, assignment, capability-grant, artifact, and evidence state; the example host app demonstrates a custom objective, selected expert, scoped connection policy, approval checkpoint, and non-report JSON deliverable (fr54).
* `tempest_config(search_provider = )` searches now apply a request timeout
  and retry on transient HTTP errors, and a single missing or non-public
  result URL no longer discards every other result for the query.
* Co-STORM sessions now route turns, update the mind map, and summarise using
  the most recent dialogue turns instead of the oldest.
* Co-STORM evidence now records expert session ids, expert ids, and progress correlation ids on claims from expert tools, warmup, chat, and STORM research runs so Facts and Sources can be traced back to the agent turn that produced them (vtz9).
* Co-STORM moderators now receive the exact live expert roster and the real `delegate_to_expert()` contract, must delegate substantive research questions before answering, preserve returned source and claim IDs, and surface an explicit evidence gap when a turn cites no inspected source. Each moderator turn delegates at most one narrow question, and experts reuse shared evidence before a bounded search, preventing exhaustive research loops from blocking the chat. Moderator answers avoid generic end-of-answer next-step menus, while suggestion cards focus on topic-specific research questions tied to evidence gaps, uncertainty, and mind-map expansion (svyx).
* Persisted runs now write artifacts atomically and write the run manifest
  last, so an interrupted save cannot corrupt artifacts or leave `resume`
  pointing at a stage whose output is missing.
* The bundled Shiny app now targets shinychat's `chat_server()`
  API for streaming, cancellation, greetings, and client state management.
* The bundled Shiny app now saves, loads, and autosaves Co-STORM session bundles from the Chat sidebar so restored sessions repopulate the chat, sources, mind map, transcript, and report views (n64q).
* The bundled Shiny app now renders STORM and Co-STORM workflow progress from host-neutral progress events and reducer state (e08d).
* The bundled Shiny app now streams STORM workflow progress from the background worker so stage chips update while a run is still in flight (1fxn).
* The bundled Shiny app now carries provider-native source context into Co-STORM fact extraction so warmup and chat turns populate Facts and Sources when sources were attached to the answer turn (b77g).
* The bundled Shiny app now invalidates Co-STORM progress output from asynchronous warmup callbacks so progress icons render while warmup is still running (2zbg).
* The bundled Shiny app now delegates its greeting, cancellation, suggestion cards, footer container, slash commands, attachments, and streamed tool and thinking displays to `shinychat`; app-generated suggestion markup crosses a narrow typed trusted boundary while model output remains escaped, native card titles and attachment types are used, restored moderator turns are preserved, session controls use `bslib` accordions and switches, and turn-only chat history remains disabled until it can restore complete Tempest experts, evidence, maps, progress, and reports (pkd5).
* The bundled Shiny app now renders Tempest source citations as numbered inline links with cited-source reference panels in reports, transcript answers, and HTML report downloads (k67p, pgp9, eq7b, dq0v).
* The bundled Shiny app now registers slash commands for `/new`, `/new-session`, `/experts`, `/sources`, `/facts`, `/claims`, `/report`, `/system`, and `/tools`, backed by normal chat-visible responses (t5zn).
* The bundled Shiny app now uses a Tempest assistant icon in chat and transcript views plus deterministic expert icons for Co-STORM experts in the panel and workflow progress (q8zc, zb9y).
* The bundled Shiny app no longer errors when async chat callbacks refresh the
  shared session store outside a reactive consumer.
* The bundled Shiny app's Co-STORM warmup now asks each capability-scoped expert for one bounded evidence-backed orientation in parallel, requires a targeted search when session evidence is absent, commits cited sources and claims in stable order before updating the shared mind map once, distinguishes evidence from scoping-only context in its summary, shows compact progress without streaming orientations into the chat, and retires timed-out expert sessions before later dialogue.
* The bundled Shiny app was rebuilt around Shiny modules (one per tab) with a
  shared reactive store, runs the STORM pipeline as a background `ExtendedTask`
  bound to its task button, and adds a knowledge-stats value-box strip on the
  Mind Map tab, a landing welcome message, and an "About" popover linking the
  STORM, Co-STORM, and DSPy papers and upstream repositories.
