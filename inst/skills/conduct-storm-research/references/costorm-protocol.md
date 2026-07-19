# Portable collaborative Co-STORM protocol

Use this protocol only when the host can preserve a multi-turn research
session. Fall back to batch STORM when persistent state is unavailable.

## Durable session state

Maintain:

- the topic, scope, evidence policy, and completion criteria;
- an active and retired expert roster with stable IDs;
- one perspective and question agenda per expert;
- private expert continuity or an equivalent per-expert state boundary;
- the public transcript;
- source, claim, evidence-span, dispute, and question ledgers;
- a shared hierarchical mind map;
- turn-policy state, progress, and cancellation state;
- report and checkpoint artifacts.

Never infer durable state only from what happens to remain in the visible chat
window.

## Build the panel

Choose complementary experts that create meaningful differences in questions
and evidence. For each expert, define:

- stable ID, display name, and role;
- perspective and focus;
- initial questions;
- allowed tools and source scope;
- active or retired status.

Do not create more experts than the host can manage safely. Retiring an expert
must not erase its transcript or evidence.

## Warm up intentionally

Optionally let each expert investigate its initial questions before public
dialogue. Use warmup to seed the shared evidence and mind map, not to generate
a hidden final answer.

Emit progress per expert and question. Preserve partial results and explicit
failures so the user can decide whether to continue.

## Run the collaborative discourse

For each turn:

1. Read the user's contribution and current shared state.
2. Decide whether to answer directly, delegate to one or more experts, ask a
   clarifying question, surface an unseen issue, or stop.
3. Give delegated experts only the permissions and sources allowed for the
   session.
4. Preserve their individual continuity and evidence traces.
5. Synthesize the public response without erasing disagreement.
6. Add new claims, sources, questions, and uncertainties to the ledgers.
7. Update the mind map with stable node and edge identities.
8. Offer the user a meaningful way to steer the next turn.

Use a moderator to manage participation and coverage. Do not let the moderator
silently invent expert findings or cite uninspected material.

## Surface unknown unknowns

Periodically inspect:

- credible sources not yet discussed;
- high-value questions left unanswered;
- overrepresented perspectives;
- isolated mind-map branches;
- assumptions repeated without evidence;
- conflicts that have not been explained.

Surface only useful gaps. Do not interrupt every turn with novelty for its own
sake.

## Manage the mind map

Treat the mind map as shared research state, not decorative output.

- Preserve stable node IDs.
- Attach notes, claims, sources, and transcript turns where possible.
- Split overloaded nodes and merge duplicates deliberately.
- Keep important disagreements visible.
- Record reorganization so saved or replayed sessions remain understandable.

## Stop and report

Stop the dialogue when the user requests a report, the agreed coverage is
reached, the budget is exhausted, or meaningful progress has stalled.

Generate the report from the shared evidence, transcript, and mind map. Include:

- the central findings;
- perspective-specific contributions;
- important disagreements and gaps;
- citations and evidence limitations;
- the final mind-map structure or a portable outline;
- recommended next questions when appropriate.

Persist a checkpoint before and after report generation.

## Verify the session

Test:

- expert identity and continuity across turns;
- user steering and moderator delegation;
- permission isolation;
- mind-map updates and replay;
- partial warmup or expert failure;
- cancellation and stale work rejection;
- save/restore without live clients or secrets;
- report consistency with transcript and evidence.
