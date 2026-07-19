# Embed Tempest research in a host

Choose the smallest integration surface that provides the state and controls
the host actually needs.

## Embed the Tempest panels

Use `tempest_shiny_ui()` and `tempest_shiny_server()` when a Shiny host wants
Tempest's existing chat, sources, facts, mind map, transcript, report, or STORM
panels inside its own page shell.

Let the host supply:

- a per-session `tempest_config()`;
- optional selected experts;
- a process-local runtime and connection permissions;
- a stable session ID;
- storage and download policy;
- the panel selection and surrounding controls.

Use `tempest_shiny_store()` when multiple adapter instances or host controls
must share and inspect the same research session.

## Keep an existing shinychat interface

Use the headless Tempest APIs when the application already owns its shinychat
UI and should present research results in that conversation.

- Create a fresh ellmer chat client and Tempest session for every Shiny
  session. Never share mutable chat or Co-STORM state across users.
- Run batch STORM outside the reactive main path with
  `tempest_run_async()` or an equivalent host-owned background boundary.
- Keep Co-STORM as a persistent `tempest_session()` associated with the user
  session. Map user messages to intentional session turns rather than starting
  a new research session for every message.
- Surface progress, cancellation, failures, evidence, and report readiness.
  Do not show only a spinner followed by prose.
- Append the final report or a concise tool result to shinychat while keeping
  the source ledger and typed artifacts available in dedicated views or
  downloads.
- Use shinychat's tool-result display only for presentation. Authorization,
  evidence policy, persistence, and cancellation remain host or Tempest
  responsibilities.

## Expose a generic run

Use `tempest_storm_workflow_run()` or `tempest_costorm_workflow_run()` when the
host already understands `TempestRun`. Consume state through public accessors:

- `tempest_run_status()`;
- `tempest_run_events()`;
- `tempest_run_approvals()`;
- `tempest_run_artifacts()`;
- `tempest_run_capability_grants()`.

Feed events into a host-neutral reducer before rendering them. Keep Shiny
reactivity and widgets as adapters over package state rather than as the source
of workflow truth. Use the `evidence` reactive returned by
`tempest_shiny_server()` when the Shiny adapter owns the view.

## Offer the portable workflow instead

Use `conduct-storm-research` when the host should reproduce the STORM protocol
without calling Tempest. A skill loader can expose that package-shipped
`SKILL.md` to an ellmer client, but the host must separately provide retrieval,
evidence, state, and output tools.

Current versions of `btw` can discover skills bundled in attached R packages
and register a skill-loading tool through `btw_tools("skills")`. Check the
installed `btw` help before relying on this optional bridge. Alternatively,
copy the portable skill into a skill directory supported by the host.

## Verify the host boundary

Exercise:

- per-session isolation;
- background task lifetime and cleanup;
- cancellation before another search, model call, or export;
- ordered progress and stale-result rejection;
- client replacement and transcript continuity when used;
- approval UI against the exact pending request;
- rich tool-result rendering and accessible fallback text;
- report, evidence, and archive downloads;
- restore behavior after the process-local runtime is recreated.
