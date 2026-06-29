# API lifecycle and style modernization

This first pass keeps existing behavior intact while making the public package
surface easier for host apps to depend on intentionally.

## Lifecycle story

The default package surface remains available but pre-1.0. Functions that are
explicitly intended as host or adapter contracts now carry experimental
lifecycle badges:

- Host extension points: `tempest_artifact_store()`,
  `tempest_memory_artifact_store()`, and `tempest_expert()`.
- Embeddable Shiny adapter: `tempest_shiny_store()`, `tempest_shiny_ui()`, and
  `tempest_shiny_server()`.
- Host-neutral progress contracts: `tempest_progress_event()`,
  `tempest_progress_collector()`, `tempest_progress_filter()`,
  `tempest_progress_replay()`, and `tempest_progress_state()`.
- Session persistence: `tempest_session_snapshot()`,
  `tempest_session_restore()`, `tempest_session_save()`, and
  `tempest_session_resume()`.
- Explicit dsprrr optimization IO: `tempest_optimize_dsprrr_modules()`,
  `tempest_save_dsprrr_modules()`, and `tempest_load_dsprrr_modules()`.

No exported function or argument was renamed in this pass, so no deprecation
shim is needed. Future breaking renames should use lifecycle deprecations
instead of abrupt removals unless the change is limited to an undocumented
internal helper.

## Condition modernization

The highest-impact user-facing failure surfaces are now classed and catchable:

- Configuration validation: `tempest_config_error`,
  `tempest_search_provider_error`, and `tempest_artifact_store_error`.
- Chat setup and tool registration: `tempest_chat_error`.
- Retriever setup and URL safety: `tempest_retriever_error`,
  `tempest_missing_envvar_error`, and `tempest_retriever_url_error`.
- Optional dependency checks: `tempest_missing_package_error`.
- Session persistence object validation: `tempest_session_error`,
  `tempest_session_snapshot_error`, and `tempest_session_save_error`.

Provider HTTP failures, full STORM step failures, and lower-level ledger S7
validators are intentionally deferred. They need a broader pass so condition
classes remain coherent across scripted STORM, Co-STORM, and host app adapters.

## Reference structure

The pkgdown reference index now groups exported APIs by workflow, retrieval,
evidence, host extension, persistence, progress, and dsprrr module concerns.
This replaces the previous single catch-all list and is the canonical map for
which exports are intended package surfaces.
