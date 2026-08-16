# Interactive Co-STORM with Tempest

Use Co-STORM when the research process should remain interactive and a human
should steer a persistent panel rather than wait only for a batch report.

## Workflow model

Preserve these actors and state:

- multiple persistent experts with distinct perspectives and private
  conversation continuity;
- a moderator that delegates by stable expert ID and can surface useful
  follow-up questions;
- a human participant who can redirect the discussion;
- a shared evidence ledger of sources, claims, and evidence spans;
- a dynamic mind map that records and reorganizes the shared conceptual space;
- a transcript and final report derived from the session state.

Co-STORM is not complete if every turn is stateless, if the human cannot steer
the panel, or if the mind map and evidence are discarded between turns.

## Start a session

```r
config <- tempest_config(
  models = "openai/gpt-5.6-luna",
  search_provider = "wikipedia",
  citation_policy = "source_attributed",
  max_active_experts = 3
)

session <- tempest_session(
  "Grid-scale battery recycling",
  config = config,
  n_experts = 3
)
```

Use exact `tempest_expert()` profiles when the host owns the roster. Keep live
tools, clients, and credentials in the host process rather than serialized
profiles.

## Conduct the session

Warmup is optional but useful when the panel should establish baseline evidence
before the user joins:

```r
session$warmup(verbose = TRUE)
answer <- session$step(
  "Which barriers are most likely to constrain deployment this decade?"
)
cat(answer$answer)
```

Let the user observe, question, challenge, and redirect. Preserve the returned
turn, expert delegation, newly discovered evidence, transcript, and mind-map
updates. Do not fabricate autonomous turns when the configured mode expects
user initiative.

Enable the discourse manager only when autonomous turn selection is desired:

```r
config <- tempest_config(enable_discourse_manager = TRUE)
session <- tempest_session("Grid-scale battery recycling", config = config)
session$step(auto = TRUE)
```

Respect the active-expert budget. Adding or retiring experts changes the
available roster but must not redirect stable expert IDs or erase prior
evidence.

## Generate and inspect a report

```r
report <- session$report(
  style = "executive",
  include_references = TRUE,
  reorganize = TRUE
)
cat(report)
```

Inspect the session's research workspace, claims, transcript, mind map, and report
state. Report generation should preserve uncertainty and evidence
relationships collected during dialogue.

## Persist and restore

```r
tempest_session_save(
  session,
  "tempest-runs/battery-recycling-session",
  overwrite = TRUE
)

session <- tempest_session_resume(
  "tempest-runs/battery-recycling-session",
  config = config
)
```

Restore durable roster, transcript, mind map, evidence, and report state.
Recreate chats, credentials, callbacks, tools, and authenticated clients. Do
not resume work automatically before the host has inspected the restored
state.

## Verify

Test with fake chats and local evidence. Verify:

- exact expert delegation and continuity across turns;
- warmup progress and terminal events;
- dynamic roster limits and retired-expert behavior;
- transcript, mind-map, evidence, and report consistency;
- session save/restore and explicit live-dependency reattachment;
- cancellation and stale callback handling;
- live Shiny behavior when background work or reactive state is involved.
