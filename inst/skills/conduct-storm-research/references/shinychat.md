# Make the skill callable in shinychat

Use this pattern when a shinychat application should let its model load and
follow `conduct-storm-research` without calling Tempest's workflow APIs.

## Verify the bridge

Check the installed versions before coding. The required bridge must:

- discover package-bundled or installed Agent Skills;
- register a skill-loading tool on an ellmer client;
- expose the available skill names to the model;
- return the selected `SKILL.md` and bundled resource paths.

Current `btw` documentation provides this through `btw_tools("skills")` and
`btw_tool_skill()`. Older versions may not. Inspect the installed help and
upgrade or use the fallback below when those interfaces are absent.

## Register the skill and real tools

For a single-user prototype:

```r
library(tempest)
library(ellmer)
library(shinychat)

client <- ellmer::chat("openai/gpt-5.4")
client$register_tools(c(
  btw::btw_tools("skills"),
  host_search_tools(),
  host_evidence_tools(),
  host_artifact_tools()
))

shinychat::chat_app(client)
```

Attaching Tempest makes its `inst/skills` directory discoverable to compatible
package skill loaders. The outer model can then select
`conduct-storm-research`; the host-supplied tools perform retrieval, evidence
updates, and artifact publication.

Treat `host_search_tools()`, `host_evidence_tools()`, and
`host_artifact_tools()` above as placeholders for narrow, real
`ellmer::tool()` definitions. Do not provide a general R execution tool merely
to avoid designing those contracts.

For a multi-user application, create and configure the ellmer client inside
the Shiny server session and pass it to `chat_mod_server()`. Never share the
mutable client, tool state, or research ledgers between users.

## Instruct the outer model

Keep the outer system prompt short. State that:

- a portable STORM skill is available;
- the model should load it for deep, multi-perspective research requests;
- batch mode is the default;
- collaborative mode requires persistent host state;
- the registered research tools are the only authorized source and state
  interfaces.

Let the skill carry the detailed procedure. Do not duplicate its entire body
in the system prompt.

## Preserve state and lifecycle

- Run long work outside the reactive main path.
- Connect the shinychat stop control to the active research boundary.
- Emit bounded stage or turn progress without flooding the transcript.
- Preserve safe partial results and checkpoint identity after cancellation.
- Keep Co-STORM expert continuity, transcript, evidence, and mind-map state
  outside the model's visible turn window.
- Append a concise tool result to chat and expose the full report, sources,
  claims, and checkpoints through rich cards, panels, or downloads.
- Supply plain text or structured fallback content for non-shinychat hosts.

## Fallback without a skill loader

Install or copy `conduct-storm-research` into the application's resources,
read its `SKILL.md` and the selected mode references when creating a dedicated
research agent, and register that agent as one narrow tool on the outer ellmer
client.

Keep the dedicated agent's research state separate from the outer chat. Return
the same structured report, evidence, status, and checkpoint contract as the
dynamic skill-loader approach.

## Verify

Use fake local search and evidence tools to test:

- skill discovery and selection;
- missing-tool reporting;
- batch completion;
- collaborative-mode refusal without persistent state;
- per-session isolation;
- cancellation and partial checkpoint behavior;
- tool-result display and plain fallback;
- absence of credentials or live clients in saved state.
