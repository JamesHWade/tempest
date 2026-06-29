# Chat runtime controls

This pass maps the `btw` chat footer pattern onto the Tempest Shiny app.

## Source pattern

The installed `btw` app uses a compact footer adjacent to the chat surface. Its
footer includes:

- A system-prompt shortcut.
- A concise provider/model label.
- Inline token and cost counters updated as chat turns complete.
- Small, low-emphasis styling so the footer supports the chat rather than
  competing with it.

## Tempest mapping

Tempest now uses shinychat's native `footer` hook for a compact operator footer.
The footer keeps long-running Co-STORM controls close to the chat input:

- Runtime state: no session, ready, answering, or running the current progress
  stage.
- Session counts: experts, collected sources, collected facts, and report
  readiness.
- Icon actions: new session, experts, sources, facts, report, system prompt, and
  tools.

Token and cost counters are intentionally deferred. Tempest currently swaps
between initial, moderator, expert, and restored chat clients, so per-session
accounting needs a stable package-level contract before it should appear in the
UI.

## Slash commands

shinychat provides first-class slash command registration through the
`chat_server()` return object's `slash_command()` method. Tempest registers:

- `/new` and `/new-session` to clear the current session.
- `/experts` to summarize the expert panel.
- `/sources` to summarize collected sources.
- `/facts` and `/claims` to summarize evidence claims.
- `/report` to generate the current report.
- `/system` to show the moderator system prompt.
- `/tools` to show current provider/tool status and command help.
