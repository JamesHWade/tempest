---
name: use-tempest-research
description: Use the Tempest R package's built-in STORM and Co-STORM research workflows. Use when an agent needs to explain, select, configure, run, resume, inspect, evaluate, or embed scripted multi-perspective STORM reports or interactive Co-STORM sessions, including Shiny and shinychat host integration. Use the custom-workflow skills instead when the requested outcome requires a host-defined workflow graph.
---

# Use Tempest research

Choose the built-in research mode that matches the user's interaction model,
then operate it through Tempest's public APIs and inspectable evidence state.

## Choose the workflow

- Use scripted STORM when one topic should become an evidence-backed report
  through a bounded batch run.
- Use Co-STORM when a human should steer an ongoing expert discussion, inspect
  a shared mind map, ask follow-up questions, and request a report later.
- Use `tempest_storm_workflow_run()` or
  `tempest_costorm_workflow_run()` when a host needs the built-in behavior
  behind the generic `TempestRun` status, event, artifact, approval, and
  persistence interface.
- Use `design-tempest-workflow` when the requested steps, permissions,
  approvals, side effects, or output types are application-specific.
- Use `conduct-storm-research` when the STORM protocol must run without
  Tempest APIs.

Read the mode-specific reference before implementing or operating it:

- [references/tempest-storm.md](references/tempest-storm.md) for scripted
  STORM.
- [references/tempest-costorm.md](references/tempest-costorm.md) for
  interactive Co-STORM.
- [references/host-integration.md](references/host-integration.md) when
  embedding either mode in Shiny, shinychat, or another host.

## Operating workflow

1. Inspect the installed Tempest version, public help, and host code. Confirm
   the available APIs instead of relying on remembered signatures.
2. Establish the topic, desired output, interaction mode, evidence policy,
   approved source scope, model and retrieval budget, persistence needs, and
   completion criteria.
3. Create one `tempest_config()` with explicit model, search, evidence, and
   source-budget choices. Keep credentials outside R source and durable state.
4. Start with a narrow topic, small expert pool, and bounded question count.
   Expand only after the first run or session behaves correctly.
5. Preserve the `SourceStore`, typed artifact catalog, claims, evidence spans,
   citations, events, and run or session identifiers. Do not reduce the result
   to report text when the host needs auditability or resume support.
6. Inspect terminal or paused state explicitly. A report string alone does not
   prove that requested outputs, approvals, or evidence policies succeeded.
7. Save durable state at intentional boundaries and recreate live chats,
   credentials, callbacks, tools, and connection bindings before resuming.
8. Exercise the actual host path when correctness depends on asynchronous work,
   cancellation, Shiny session lifetime, reactive invalidation, tool display,
   approval UI, or downloads.

## Guardrails

- Do not describe STORM as a single long prompt. Preserve perspective
  discovery, iterative research, evidence capture, outline refinement,
  evidence-grounded writing, and polishing as distinct stages.
- Do not describe Co-STORM as batch STORM with a chat box. Preserve persistent
  experts, moderated turns, user steering, shared evidence, and mind-map state.
- Do not use the generic workflow kernel merely to rename built-in stages.
- Do not serialize functions, chat clients, credentials, callbacks, or
  authenticated connection bindings.
- Do not let tests require API keys, network access, or live provider
  responses. Use fake chats, local stores, fixtures, and focused Shiny tests.
- Do not claim a host integration is verified until the relevant live or
  deterministic host surface has been exercised.
