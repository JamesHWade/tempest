# Co-STORM session provenance

This note records the identity boundaries used for Co-STORM evidence and
progress integration. It addresses kata issue `vtz9`.

## Identity fields

- `TempestSession$session_id` is the package-level Co-STORM run id. Progress
  events use it as `run_id`, and moderator/chat extractions can record it on
  claims.
- Expert tool `session_id` is the continuation handle returned by `ask_*`
  expert tools. Callers pass it back to keep asking the same persona in the
  same chat session. Claims produced by that expert session record this id.
- `persona_id` identifies the expert or agent persona that produced the claim.
  Moderator extractions use `moderator` until moderator personas become first
  class records.
- `retrieval_step_id` records the progress `correlation_id` for the expert
  tool call, warmup question, or chat turn that produced the claim.
- The Shiny `active_session_id` counter is a UI lifetime guard for stale async
  callbacks. It is not a research provenance id and should not be recorded on
  claims.

## Integration contract

Expert sessions own a per-session provenance environment. Tool closures are
registered when the expert chat is created, but `retrieval_step_id` is only
known when the expert is called. The registered `add_claim` and `add_fact`
tools therefore resolve provenance at execution time from the active expert
question or tool call.

Post-answer fact extraction receives the same `session_id`, `persona_id`, and
`correlation_id` explicitly. This keeps tool-written claims and extracted
claims aligned for Facts, Sources, progress timelines, and later claim-review
agents.

STORM scripted runs record `progress_run_id` as claim `session_id` when it is
available. Sequential and parallel research both pass this run id into tool
registration and fact extraction. Section-level extraction records the section
title as `section_id`.

## Deferred work

Deputy or evaluator identities should become explicit persona records before
they are written into `persona_id`. Until then, `persona_id` should use stable
expert ids, `moderator`, or `NA_character_` rather than UI-only labels.
