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
- process-local tools and authenticated clients when required;
- a stable session ID;
- storage and download policy;
- the panel selection and surrounding controls.

Use `tempest_shiny_store()` when multiple adapter instances or host controls
must share and inspect product state. Its exact 13 members are
`peek_costorm_session`, `costorm_session`, `costorm_workspace`,
`set_costorm_session`, `touch_costorm_session`, `save_costorm_session`,
`resume_costorm_session`, `costorm_persistence_status`, `report_md`,
`report_workspace`, `report_topic`, `publish_costorm_report`, and
`publish_storm_report`. Do not rely on former generic aliases.

The exact `tempest_shiny_server()` handle contains `store`, `costorm_session`,
`costorm_events`, `costorm_evidence`, `storm_events`, `report_md`,
`report_workspace`, `report_topic`, `report_navigation_event`, and
`touch_costorm_session`. The navigation value is a monotonic event counter, not
a Boolean readiness flag.

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
  sources, claims, and report available in dedicated views or downloads.
- Use shinychat's tool-result display only for presentation. Authorization,
  permissions, and external side effects remain host responsibilities;
  Tempest owns research evidence, persistence, and cancellation.
- Use explicit bounded Co-STORM archive download and upload. Do not claim
  browser-temporary autosave or invent a durable host filesystem policy.
- Do not add a parallel perspective-research control. The maintained STORM
  panel uses its asynchronous worker while each expert turn remains
  synchronously bound to one terminal Deputy execution.

## Observe product state

Use the direct STORM and Co-STORM APIs, and keep host UI state as a projection
over the research result or session:

- pass a progress callback to `tempest_run()` or `tempest_run_async()` for
  scripted STORM events;
- use `tempest_execution_events(session)` only for interactive Co-STORM event
  history;
- `tempest_progress_state()` for normalized event state.

Feed events into a host-neutral reducer before rendering them. Keep Shiny
reactivity and widgets as adapters over package state rather than as the source
of workflow truth. Use the `costorm_evidence` reactive returned by
`tempest_shiny_server()` when the Shiny adapter owns the view.

The installed `examples/shiny-host/app.R` uses
`tempest_shiny_ui(..., panels = "storm")` with `tempest_shiny_server()` and
therefore follows the same maintained asynchronous path as the bundled app.
Use polite atomic status regions for progress, persistence, and successful
publication. Use alerts for validation, cancellation, transport, and
publication failures, and never present a rejected product as successful.

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
- rich tool-result rendering and accessible fallback text;
- report, evidence, and archive downloads;
- restore behavior after process-local dependencies are recreated.
