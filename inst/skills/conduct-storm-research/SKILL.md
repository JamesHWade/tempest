---
name: conduct-storm-research
description: Conduct or reproduce provider- and framework-neutral STORM-style deep research without relying on Tempest APIs. Use when a tool-capable agent must create an evidence-backed report through multi-perspective research, facilitate a persistent Co-STORM-style collaborative inquiry, implement the protocol in another framework, or expose it as a callable skill or agent tool in shinychat, ellmer, or another host.
---

# Conduct STORM research

Run the research protocol itself. Depend only on the tools and durable state
the host exposes; do not assume Tempest, Python STORM, or another workflow
engine is installed.

## Choose the mode

- Use batch STORM when one topic should become a bounded, cited report.
- Use collaborative Co-STORM when a human should steer a persistent panel,
  shared mind map, and evolving evidence base over multiple turns.
- Use batch STORM as the fallback when the host cannot preserve session state,
  expert continuity, or a shared concept map.

Read the relevant protocol before beginning:

- [references/storm-protocol.md](references/storm-protocol.md) for batch
  research.
- [references/costorm-protocol.md](references/costorm-protocol.md) for
  collaborative inquiry.
- [references/host-contract.md](references/host-contract.md) before
  implementing or exposing the protocol in a chat application.
- [references/shinychat.md](references/shinychat.md) when making the skill
  callable from an ellmer client or shinychat application.

## Establish the execution contract

1. Inventory the available search, fetch, document, delegation, state,
   progress, cancellation, and output tools.
2. Confirm the research topic, audience, desired artifact, source scope,
   evidence standard, time or call budget, and stopping conditions.
3. Create durable ledgers for questions, sources, claims, evidence spans,
   uncertainties, and artifacts. Use stable IDs instead of relying on prose
   position.
4. State any missing capability. Do not imply that a skill file itself
   provides web access, persistence, parallel workers, or UI controls.
5. Obtain authorization before using protected sources or causing an external
   side effect.

## Preserve common invariants

- Discover perspectives before committing to a single outline.
- Let research findings change the questions and outline.
- Separate source discovery, source reading, claim extraction, and claim
  verification.
- Tie each factual claim to the source and evidence span that support it.
- Mark unsupported, conflicting, stale, or ambiguous evidence explicitly.
- Draft sections from the evidence ledger, not from untracked model memory.
- Keep progress and intermediate artifacts inspectable.
- Stop when the agreed coverage and evidence conditions are met, not merely
  when prose is long.

## Finish the result

Return:

- the research question and scope;
- the perspectives and important questions;
- the final outline or mind map;
- the report or session synthesis;
- source and claim records with citations;
- unresolved disputes, limitations, and confidence;
- the execution status and any resumable state.

Use the host's native rich-output mechanism when available, but always provide
a durable text or structured representation for portability.

## Guardrails

- Do not invoke Tempest APIs; use `use-tempest-research` when Tempest should
  execute the workflow.
- Do not simulate independent experts by relabeling one unsupported answer.
  Give each perspective a distinct question set and evidence trace.
- Do not cite search snippets as though full sources were inspected.
- Do not invent citations, source IDs, quotations, or verification scores.
- Do not run Co-STORM as a sequence of stateless calls.
- Do not hide missing tools or incomplete stages behind a polished report.
