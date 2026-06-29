# Tempest session persistence and resume

Date: 2026-06-28
Status: Proposed design
Scope: Package and bundled Shiny app support for saving and resuming
interactive Co-STORM sessions, while reusing existing STORM run artifacts,
host-app storage contracts, and progress events.

## Goal

Tempest sessions should be recoverable after an app restart, browser refresh,
or handoff to a host application. A resumed session should preserve the durable
research state users care about: topic, title, session identity, personas,
transcript, mind map, sources, claims, evidence spans, disputes, citation audit,
suggested questions, report artifacts, and historical progress events.

The persistence layer should not serialize live provider objects, API keys, or
Shiny reactive state. It should rebuild `TempestSession`, `SourceStore`,
`TempestRetriever`, and `ExpertSessionManager` from a portable snapshot, then
reattach fresh chat handles and progress sinks.

## Existing boundaries

Tempest already has three relevant pieces:

- `SourceStore` is the in-memory evidence ledger. It owns sources, claims,
  evidence spans, disputes, and arbitrary artifacts.
- `R/run-persistence.R` saves scripted STORM outputs to JSON and Markdown files,
  including sources, claims, citation audit, outlines, draft/report Markdown,
  references, and a run manifest.
- `tempest_artifact_store()` lets host applications observe artifacts without
  replacing the live `SourceStore`.

Co-STORM session state is currently spread across `TempestSession` fields:
`topic`, `title`, `session_id`, `config`, `store`, `retriever`, `personas`,
`expert_session_manager`, `chats`, `transcript`, `mindmap`, `artifacts`, and an
optional `discourse_manager`. Only some of these are durable data. `chats`,
registered tool closures, Shiny reactive stores, and provider-native chat
handles must be recreated.

This design reconciles with:

- `c3dw`: host apps provide identity, permissions, storage, and UI shell.
- `wedz`: progress and lifecycle state should be observable through structured
  package events, not Shiny-specific callbacks.
- `tc8q`: artifact stores mirror or persist state, but do not replace
  `SourceStore`.

## Snapshot model

Use a directory bundle as the first durable format. It is easy to inspect,
works with local files and host-managed storage, and reuses the existing STORM
artifact style.

```text
session.json
config.json
personas.json
expert_sessions.json
transcript.json
mindmap.json
progress_events.json
store/
  sources.json
  claims.json
  evidence_spans.json
  disputes.json
artifacts/
  citation_audit.json
  report.md
  report_body.md
  mindmap.md
  suggested_questions.json
  references.json
```

`session.json` is the manifest. It records:

- `schema_version`
- package version
- `session_id`
- topic and title
- creation and update timestamps
- save status, such as `complete`
- manifest of files included in the bundle
- optional run/event cursors for host apps

The manifest is written last. A bundle without a complete manifest is treated
as partial and should not be resumed without an explicit recovery path.

## Durable state

### Source store

Add small package helpers that can snapshot and restore `SourceStore`:

- `tempest_source_store_snapshot(store)`
- `tempest_source_store_restore(snapshot, store = SourceStore$new())`

The snapshot includes sources, claims, evidence spans, disputes, and selected
named artifacts. Restore must rebuild private indexes by calling public mutators
such as `upsert_source()` and `add_claim()` rather than copying environments.

Claims already have `tempest_claim_to_list()` and `tempest_claim_from_list()`.
Implementation should add equivalent list conversion helpers for evidence
spans and disputes before they are included in persisted bundles.

Unknown source IDs in restored claims are a data-quality error, not a reason to
create placeholder sources. The loader should report the issue and either abort
or return a partial restore object according to the caller's recovery policy.

### Session shell

The session snapshot includes:

- `topic`, `title`, and `session_id`
- validated personas, including retired experts
- transcript turns with speaker, role, text, and timestamp
- mind map data and derived Markdown
- suggested questions from the latest generation pass
- report artifacts and citation audit
- expert-session metadata: persona id, persona name, expert session id, and
  creation/update timestamps
- progress-event data from `tempest_progress_event_data()`

`TempestSession` should gain an internal restore path that accepts a supplied
`session_id`, restored `SourceStore`, personas, transcript, mind map, artifact
list, and expert-session metadata. Public construction can remain through
`tempest_session_resume()` rather than adding every restore argument to
`tempest_session()`.

### Configuration

Persist only a serializable config summary:

- model names
- provider names
- retrieval limits
- cache settings
- citation policy and verification thresholds
- feature flags such as discourse manager and unseen-source surfacing

Do not persist functions, API keys, `ragnar_store`, live artifact-store
closures, or provider credentials. On resume, the caller may pass a fresh
`config`. The loader overlays durable config values onto that runtime config
only when doing so is safe and explicit.

### Retriever cache

The retriever cache is not part of the default session bundle. It can be large,
host-specific, and derived from source URLs. A snapshot records cache policy and
cache directory metadata, but the durable source ledger remains the replayable
truth. Cached fetches can still accelerate future retrieval through
`tempest_config(cache_dir = , cache_enabled = )`.

### Progress events

Persist historical event data as lists produced by
`tempest_progress_event_data()`. On resume:

1. Load historical events for display and debugging.
2. Recompute reducer state with `tempest_progress_state()`.
3. Attach the new progress callback for future work.
4. Re-emit historical events only when a caller explicitly requests replay with
   `tempest_progress_replay()`.

This keeps Shiny and host apps from treating old events as newly running work.

## Resume semantics

A resumed session is faithful for durable research state, not for live provider
handles.

Restore should:

1. Read and validate the bundle manifest.
2. Rebuild a `SourceStore` from persisted source, claim, evidence, dispute, and
   artifact state.
3. Create a fresh `TempestRetriever` using the restored store and runtime
   config.
4. Recreate `TempestSession` with the original `session_id`, topic, title,
   personas, transcript, mind map, artifacts, and progress history.
5. Recreate role chats and register the standard retrieval, claim, and expert
   tools.
6. Recreate expert-session metadata and allow future `ask_*` calls to reuse the
   same expert `session_id` for attribution.
7. Record a resume progress event with bundle path, schema version, and restore
   diagnostics.

Restore should not:

- serialize or unserialize ellmer chat objects
- assume provider thread handles are still valid
- store credentials or raw API request bodies
- emit historical progress events as if they just happened
- mutate a host artifact store before the bundle is validated

If exact provider-level chat continuity is needed later, it should be a
provider adapter layered on top of this snapshot model. The portable baseline
uses transcript, mind map, evidence, and persona prompts as context for fresh
chat handles.

## Public API direction

Initial package API:

```r
tempest_session_save(session, path, overwrite = FALSE)
tempest_session_resume(path, config = tempest_config(), progress = NULL)
tempest_session_snapshot(session)
tempest_session_restore(snapshot, config = tempest_config(), progress = NULL)
```

`tempest_session_save()` writes a bundle directory. `tempest_session_snapshot()`
returns an in-memory list using the same schema. The list API gives host apps
and tests a path that does not require local files.

The save API should also mirror named artifacts through
`config@artifact_store` when present, but the local bundle is the first
complete implementation target.

## Shiny app behavior

The bundled Shiny app should consume the package API instead of owning a
separate session format.

First app implementation:

- Manual save and load controls in the existing session flow.
- Debounced autosave after `new_session_store()$touch()` when a session exists.
- Clear restore diagnostics when a bundle is missing critical files or uses an
  unsupported schema.
- Restored Chat, Sources, Mind Map, and Report tabs render from the resumed
  `TempestSession` and `SourceStore`.
- Running async work is not resumed automatically. The app restores completed
  state and lets the user start the next turn or report generation.

Host apps can later replace the local directory with their own storage policy,
but the bundled app should prove the package contract first.

## Versioning and migration

Use integer `schema_version`, starting at 1. The loader supports:

- current schema
- one previous schema where practical
- explicit migration helpers for additive renames

Unknown optional files are ignored. Unknown required manifest fields abort with
a classed persistence error. Missing optional artifacts produce restore
diagnostics. Missing critical files such as `session.json`, `personas.json`, or
`store/sources.json` abort unless the caller requests a partial audit mode.

Every bundle should include the package version that wrote it. This is
diagnostic metadata, not the schema version.

## Failure policy

| Failure | Behavior |
|---|---|
| Manifest missing or incomplete | Abort resume; offer audit details. |
| Unsupported schema version | Abort with a classed persistence error. |
| Optional artifact missing | Restore session and report a diagnostic. |
| Corrupt optional artifact | Skip it and report a diagnostic. |
| Corrupt source/claim/persona data | Abort by default; allow explicit audit-only loading. |
| Claim cites unknown source id | Abort or mark partial restore; never create placeholder sources. |
| Runtime config lacks required provider/chat factory | Restore durable state but fail before starting new provider work. |
| Artifact-store write fails after local save | Keep local bundle and warn with parent condition. |

This failure model aligns with the evidence-ledger rule that weak or malformed
support should be visible rather than silently dropped.

## Implementation path

1. `tpqx`: Add source-store snapshot/restore helpers, including evidence-span
   and dispute list conversion. Verify with deterministic store tests.
2. `jj5e`: Add in-memory session snapshot/restore helpers for
   `TempestSession`, excluding live chat handles. Verify a fake session
   round-trips transcript, mind map, personas, sources, claims, artifacts, and
   session id.
3. `zj62`: Add directory save/resume APIs with manifest-last writes and
   classed diagnostics for missing or corrupt files.
4. `xenk`: Persist and reload progress-event history without re-emitting it as
   live work.
5. `n64q`: Add bundled Shiny save/load and debounced autosave on top of the
   package API.

## Deterministic tests

Tests should use fake chats, local temporary directories, and in-memory stores.
No test should require API keys, network access, or live provider responses.

Coverage targets:

- `SourceStore` snapshot/restore preserves sources, claims, evidence spans,
  disputes, and artifacts.
- `TempestSession` snapshot/restore preserves topic, title, session id,
  personas, transcript, mind map, report artifacts, and store state.
- Restored sessions can run a new fake `step()` with fresh chats and the same
  durable `session_id`.
- Progress-event history loads into reducer state but is not emitted during
  resume unless explicitly replayed.
- Missing optional artifacts produce diagnostics; missing critical files abort.
- Shiny module tests restore a saved session and render Chat, Sources, Mind Map,
  and Report views without launching a browser.

## Non-goals

- No required database dependency.
- No serialization of ellmer chat objects, Shiny sessions, or provider-native
  thread handles.
- No persistence of API keys, raw credentials, or complete provider request
  bodies.
- No automatic restart of in-flight async work after resume.
- No replacement of existing STORM run artifact persistence in the first pass.

## Success criteria

1. A package user can save a Co-STORM session and resume it later with the same
   durable research state and a fresh runtime config.
2. Host apps can choose between a local bundle and an in-memory snapshot without
   depending on bundled Shiny internals.
3. The bundled Shiny app can restore a completed or partially completed session
   into the visible Chat, Sources, Mind Map, and Report surfaces.
4. Version and failure diagnostics are explicit enough for future migrations.
5. Follow-up implementation issues are small enough to verify with deterministic
   package and Shiny `testServer` tests.
