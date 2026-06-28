# Agent persona icons

## Context

The bundled Shiny app now has two identity layers:

- The Tempest assistant identity, rendered through shinychat's
  `icon_assistant` hook with the app-local `logos/tempest.svg` asset.
- Runtime Co-STORM expert identities, rendered from progress metadata already
  emitted by expert and tool events.

This keeps the generic app assistant separate from the expert panel. It also
avoids writing raw expert answers into the main chat just to show which persona
is working.

## Event contract

Host apps should treat these fields in progress event payloads as the stable
identity contract for persona rendering:

- `expert_id`: stable persona identifier for a session.
- `expert_name`: display name for the persona.
- `session_id`: stable expert chat session id when available.

The Shiny app renders deterministic initials from `expert_name`, with
`expert_id` as the color-key fallback. Future deputy-backed orchestration should
preserve those same fields on deputy tool-call and agent-run events. If deputy
adds richer identity metadata later, it should be additive, for example:

- `agent_kind`: `moderator`, `expert`, `deputy`, or `tool`.
- `agent_role`: a compact role/category label supplied by the orchestrator.
- `icon_hint`: optional host-neutral hint such as `science`, `policy`, or
  `engineering`.

Host apps should continue to work when only `expert_id` and `expert_name` are
present because generated personas are runtime data, not packaged assets.

## Shiny rendering

The bundled Shiny app uses app-local helpers:

- `tempest_chat_icon()` for the Tempest assistant icon.
- `persona_icon()` for deterministic expert initials and color variants.
- `workflow_activity_item()` for active expert/tool/step progress rows.

This covers warmup fanout, ask-expert tool calls, and future deputy events that
carry the same identity fields. Narrow layouts keep the icon and label together
as a compact inline activity item, while screen readers receive an `aria-label`
such as `Expert Dr. Ada Flow`.
