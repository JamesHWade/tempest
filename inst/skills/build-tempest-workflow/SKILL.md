---
name: build-tempest-workflow
description: "Tempest 0.1 only: build a custom workflow with the frozen generic kernel scheduled for removal in Tempest 0.2.0. Use when an agent is asked to maintain an existing host-defined Tempest workflow, add deterministic workflow tests, or integrate that legacy workflow into an R package, script, or Shiny host application."
---

# Build a Tempest workflow

> **Retirement notice:** This skill documents Tempest's frozen experimental
> generic kernel and will be removed in Tempest 0.2.0. Use the STORM and
> Co-STORM product APIs for new research workflows.

Implement the smallest complete vertical slice through Tempest's public API.
Prefer a runnable workflow with one validated artifact over a broad scaffold
whose execution boundary is unclear.

Read
[references/implementation-patterns.md](references/implementation-patterns.md)
before writing the runtime or tests.

## Implementation workflow

1. Inspect the current package help, source, and host code. Confirm that the
   requested behavior is not already covered by
   `tempest_storm_workflow_run()` or `tempest_costorm_workflow_run()`.
2. Start from a concrete design. If objective, deliverables, step graph,
   approvals, permissions, or acceptance tests are missing, establish them
   before coding.
3. Create durable specifications with public constructors. Keep them free of
   functions, credentials, live clients, and other non-serializable state.
4. Implement narrow runtime operations. Accept only named execution-context
   arguments the operation uses. Register every operation with a stable ID,
   version, and correct kind.
5. Use `tempest_generate_deliverable()` inside producing steps so validation,
   rendering, checksums, provenance, approval status, and catalog publication
   follow the shared lifecycle.
6. Build a reproducible runtime factory. It must recreate operation closures,
   capability factories, connection bindings, and runtime services after a
   saved run is restored.
7. Execute with the exact selected expert pool, deliverables, connection
   permissions, runtime context, policy adapter, and optional evidence store.
8. Inspect and control the run only through `tempest_run_*()` accessors. Do not
   reach into mutable R6 fields from host code.
9. Add deterministic tests with fake chats, local resources, in-memory stores,
   and temporary bundle directories. Do not require API keys, network access,
   or live providers.
10. Update roxygen, generated documentation, pkgdown, examples, and `NEWS.md`
    when the implementation changes a package's public API.

## Minimum complete slice

The first passing slice must demonstrate:

- one objective with acceptance criteria;
- one selected expert when the step is expert-assigned;
- one deliverable with a validator and renderer;
- one registered step operation;
- one workflow specification;
- a succeeded or intentionally `awaiting_approval` run;
- a typed artifact with run, step, and expert provenance;
- ordered observable events;
- a focused save/restore test when persistence is in scope.

Then add capabilities, connections, multiple steps, retries, exporters, custom
artifact codecs, or Shiny integration only as required.

## Implementation guardrails

- Treat `tempest_skill()` as an in-workflow serializable procedure, not as the
  `SKILL.md` file currently guiding the coding agent.
- Use public constructors and adapters; do not couple host code to R6 internals.
- Keep authenticated clients and credentials in runtime connection bindings.
- Do not let a step claim completion without publishing every declared output
  artifact.
- Do not silently replace invalid artifacts. Use the deliverable retry contract
  and preserve validation diagnostics.
- Do not automatically execute a restored run. Reattach runtime state, inspect
  it, then resume explicitly when the host is ready.
- Do not broaden restored connection permissions beyond the saved grants.

## Completion report

Report the implemented contracts, runtime boundary, approval behavior,
artifacts, tests run, and any remaining unverified host integrations. Include
the exact files changed and concrete verification results.
