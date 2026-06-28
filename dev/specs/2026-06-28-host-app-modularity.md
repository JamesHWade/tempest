# Host-App Modularity for Tempest

Date: 2026-06-28
Status: Proposed design
Scope: Package contracts that let other apps embed Tempest capabilities without
copying the bundled Shiny app.

## Goal

Tempest should be usable as a research capability inside another application.
The host should be able to provide configuration, identity, permissions,
storage, and UI shell while Tempest owns the STORM / Co-STORM research loop,
experts, retrieval, evidence ledger, suggestions, and report generation.

This spec is the parent design for three implementation tracks:

- `tc8q`: extension points for experts, retrievers, and artifact stores.
- `wedz`: a host-agnostic run/event layer.
- `z0e0`: an embeddable Shiny adapter and example host.

## Target Embedding Scenario

Use a host Shiny app with an existing project workspace:

1. The host opens a "Research" panel for one project.
2. The host provides a `TempestConfig`, a session id, a project-scoped artifact
   store, and a permission policy for web/search tools.
3. Tempest starts or resumes a Co-STORM session for the project topic.
4. The host receives structured events for session start, warmup progress,
   expert calls, retrieved sources, extracted claims, report readiness, errors,
   and cancellation.
5. Tempest returns reusable artifacts: transcript, sources, claims, mind map,
   report Markdown, and citation audit.

The host app should not need to source files from `inst/shiny/R`, patch
`TempestSession` internals, or know how expert tools are registered.

## Current API Boundary

Stable enough to build on today:

- `tempest_config()` for provider/model/search/cache configuration.
- `tempest_run()` and `tempest_run_async()` for scripted STORM runs.
- `tempest_session()` / `TempestSession` for Co-STORM sessions.
- `SourceStore`, `tempest_sources()`, `tempest_claims()`,
  `tempest_verify_claims()`, `tempest_report_md()`, and
  `tempest_session_report_md()` for evidence and reports.
- `tempest_suggest_questions()` for follow-up question generation.
- `run_app()` for the bundled Shiny application.

Experimental or internal today:

- Shiny modules under `inst/shiny/R`.
- `ExpertSessionManager`, expert tool registration, and native-source
  harvesting.
- `TempestRetriever` and provider-specific search/fetch helpers.
- Run persistence internals under `R/run-persistence.R`.
- dsprrr module factories and optimization helpers as host extension surfaces.

The child issues should move only the smallest useful subset from experimental
to documented API.

## Capability Map

### Configuration

Hosts need one package-level config object plus optional per-run overrides.
`TempestConfig` remains the entry point. New host APIs should accept a config
and avoid global options except for low-level defaults such as warmup timeout.

### Experts

The current seam is `personas` in `tempest_session()`. That is already useful:
a host can supply a list of expert definitions with `id`, `name`, `title`,
`perspective`, and `initial_questions`. The next stable layer should validate
that shape and give clear errors before a session starts.

Do not expose `ExpertSessionManager` directly as the first host contract. It is
stateful R6 glue around ellmer chats and tool registration. Hosts should provide
expert definitions; Tempest should keep owning expert chat/session lifecycle.

### Retrieval

Tempest currently owns `TempestRetriever`, `SourceStore`, and web tools. Hosts
need two levels of control:

- policy: which search providers/tools are allowed for a run;
- implementation: optional custom retriever with `search()`, `fetch()`, and a
  shared `SourceStore`.

The first implementation should support a custom retriever object only if it can
run through the same `SourceStore` path, so citations, facts, reports, and
persistence keep working.

### Evidence And Artifacts

`SourceStore` is the stable mutable accumulator. It should remain R6 because
sessions mutate it in place. Host artifact storage should be a separate adapter,
not a replacement for `SourceStore`.

Minimal artifact-store contract:

```r
list(
  write = function(name, value, metadata = list()) NULL,
  read = function(name, default = NULL) default,
  list = function() character()
)
```

Names should be stable package artifact names: `transcript`, `sources`,
`claims`, `mindmap`, `report_md`, `citation_audit`, and provider-specific
extensions under a prefix.

### Run Events

Hosts should not parse chat text to infer progress. Introduce a simple event
record and callback path:

```r
tempest_event(
  type,
  run_id,
  session_id = NULL,
  at = tempest_now_utc(),
  payload = list(),
  visibility = c("compact", "debug")
)
```

Event types for the first pass:

- `session_started`, `session_finished`, `session_cancelled`, `session_error`
- `warmup_started`, `warmup_expert_started`, `warmup_question_started`,
  `warmup_question_finished`, `warmup_finished`
- `expert_call_started`, `expert_call_finished`, `expert_call_error`
- `source_added`, `claim_added`, `mindmap_updated`, `report_ready`

The existing Shiny app can keep compact chat messages, but it should eventually
derive them from events.

### Cancellation

The bundled Shiny app currently uses session/run guards to ignore stale async
callbacks. Keep that behavior, but move the identity to a package-level run
token that every async callback receives. A host can then cancel or replace a
run without importing Shiny reactive state.

## Decisions

1. Keep stateful execution in R6 and use S7/list records for value boundaries.
   This follows the existing evidence-ledger decision.
2. Stabilize package contracts before exporting Shiny modules. Host apps should
   embed a small adapter around package APIs, not depend on the bundled app's
   full sidebar/nav layout.
3. Treat `SourceStore` as the canonical in-memory ledger. Artifact stores mirror
   or persist state; they do not replace the live store.
4. Make event emission optional and no-op by default. CI and scripted users
   should not need a running event bus.
5. Keep observability, slash commands, and footer controls under the existing
   UI/runtime issues. They consume the host contracts; they are not prerequisites
   for defining them.

## First Implementation Path

### 1. Extension Points (`tc8q`)

- Add a validator/helper for host-supplied expert definitions.
- Allow a host-supplied retriever only when it shares a `SourceStore`.
- Add a no-op artifact-store adapter and tests showing report/source/claim
  writes can be captured without changing default behavior.

### 2. Run/Event Layer (`wedz`)

- Add event record construction and an event sink callback.
- Thread events through fakeable warmup/expert/report paths first.
- Preserve existing Shiny messages, but make them consume event payloads where
  practical.

### 3. Embeddable Adapter (`z0e0`)

- Build a minimal Shiny module that accepts `config`, `session_id`,
  `artifact_store`, and optional host controls.
- Include a tiny example host app that embeds the adapter with fake chats and no
  network/API dependency.
- Keep the bundled `run_app()` unchanged except for using shared package
  contracts when available.

## Verification Strategy

- Unit tests for expert validation, custom retriever behavior, artifact-store
  adapters, and event records.
- A non-Shiny test that runs a fake Tempest session and captures lifecycle
  events.
- A Shiny module/server test for the adapter with fake chats and no external
  APIs.
- Browser verification only when shinychat and provider credentials are
  available; document it as manual or optional.

## Non-Goals

- No generic plugin system in this cycle.
- No database-backed artifact store as a required dependency.
- No wholesale rewrite of `TempestSession`, `SourceStore`, or
  `ExpertSessionManager`.
- No public support promise for every file under `inst/shiny/R`.
- No replacement of dsprrr; it stays the structured module layer.

## Success Criteria

1. A host app can start a fake Tempest session without sourcing bundled app
   internals.
2. Host-provided experts, retrieval, and artifact persistence have documented
   contracts and focused tests.
3. Progress and artifact readiness are observable as structured events.
4. The bundled Shiny app remains compatible.
5. The child issues are small enough to verify with package tests and, where
   relevant, module construction checks.
