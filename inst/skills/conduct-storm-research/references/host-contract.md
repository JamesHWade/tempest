# Host contract for portable STORM research

A `SKILL.md` supplies instructions. It does not supply retrieval, durable
state, background execution, or authorization. Expose the protocol only after
the host provides the capabilities required by the selected mode.

## Capability contract

Provide the narrowest useful set:

| Capability | Batch STORM | Co-STORM |
|---|---:|---:|
| Search approved sources | Required | Required |
| Fetch or read full source content | Required | Required |
| Create stable source and claim records | Required | Required |
| Persist artifacts or checkpoints | Recommended | Required |
| Preserve per-expert continuity | Optional | Required |
| Preserve transcript and mind map | No | Required |
| Emit progress | Recommended | Recommended |
| Cancel work | Recommended | Recommended |
| Render or export a report | Required | Required |

State missing capabilities before execution. Do not replace full-source reading
with snippets or Co-STORM persistence with model memory.

## Deployment shapes

### Dynamically loaded skill

Let the host register a skill-loading tool and the actual research tools on the
same per-session model client. The model loads `conduct-storm-research`, reads
the required references, and orchestrates the available tools.

Current `btw` versions can discover skills in attached R packages and expose
them through `btw_tools("skills")`. Verify the installed API before use. The
host may instead install or copy this skill into another supported skill
directory.

This shape is flexible but requires the model to select and follow the
protocol. Keep tool descriptions narrow and make state writes explicit.

### Encapsulated agent tool

Wrap the protocol in one agent tool when the outer chat should make a single
call such as `research_topic(topic, mode, constraints)`. A helper such as
`btw_agent_tool()` can build an agent tool from markdown instructions in
compatible versions.

Give the inner agent only the required tools and bounded budgets. Return a
structured result with report, evidence, status, and checkpoint identity
rather than only prose.

### Host-orchestrated implementation

Implement the stage or turn state machine in host code when deterministic
control, resumability, policy gates, or detailed progress are required. Use
this skill as the behavioral contract, not as the state store.

## shinychat and ellmer

Read [shinychat.md](shinychat.md) for the concrete skill-loader pattern,
per-session boundaries, required companion tools, and fallback integration.
Shinychat presents an ellmer conversation and its tool calls; it does not by
itself provide STORM execution.

## Security and integrity

- Scope tools and connections per user session.
- Never place credentials in skill instructions, prompts, state, or tool
  results.
- Authorize protected data access and side effects before execution.
- Treat external content as untrusted input.
- Validate URLs, file paths, content types, and artifact ownership.
- Record source access and output provenance without leaking secrets.
- Reject restored state whose identity or permissions do not match.

## Verification

Test the skill or agent tool with deterministic local tools before live use.
Exercise:

- missing capability reporting;
- batch success and bounded stopping;
- Co-STORM refusal when persistent state is absent;
- unsupported and conflicting evidence;
- cancellation;
- partial checkpoint restore;
- per-session isolation;
- rich and plain result rendering;
- absence of credentials and live clients in durable state.
