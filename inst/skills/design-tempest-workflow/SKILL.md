---
name: design-tempest-workflow
description: Design a custom workflow for the Tempest R package before implementation. Use when an agent needs to translate an application request into Tempest objectives, resources, expert profiles, in-workflow skill and capability specifications, deliverables, workflow steps, approvals, runtime services, persistence boundaries, or a concrete implementation plan. Also use when deciding whether to use the built-in STORM or Co-STORM workflows instead of a host-defined workflow.
---

# Design a Tempest workflow

Turn the requested outcome into a small, versioned workflow contract that can
be implemented and tested without inventing application-specific state inside
Tempest.

Read [references/workflow-contracts.md](references/workflow-contracts.md)
before finalizing the design.

## Design workflow

1. Inspect the installed Tempest version, its public documentation, and any
   existing host application code. Do not assume an API from memory when the
   package source or help is available.
2. Decide whether the built-in workflow is sufficient:
   - Use `tempest_storm_workflow_run()` for scripted research and reports.
   - Use `tempest_costorm_workflow_run()` for collaborative research.
   - Design a custom workflow only when the requested outcome, steps,
     approvals, permissions, or artifact types are application-specific.
3. Define the objective in observable terms. Record constraints, approved
   input resource IDs, requested deliverables, and acceptance criteria.
4. Separate durable definitions from process-local runtime values. Keep IDs,
   versions, policies, schemas, and non-secret metadata durable. Keep R
   functions, chats, clients, credentials, callbacks, and authenticated
   connection bindings in the runtime.
5. Model the output first. For each deliverable, define canonical content,
   required fields, evidence policy, renderings, validation, export behavior,
   and whether output approval is required.
6. Draw the step graph. Give each step one stable operation ID, explicit
   dependencies, declared input and output artifacts, an assignment rule,
   retry and failure policy, required capabilities, and any pre-execution
   approval checkpoint.
7. Design least-privilege access. Use opaque connection references and narrow
   capability specifications. Grant only the connection IDs needed by the
   selected expert or model role for this run.
8. Define the host boundary: expert selection, policy adapter, runtime
   services, artifact storage, evidence ledger, progress callback, approval
   UI, and persistence location.
9. Write a verification plan before implementation. Include graph validation,
   operation preflight, success, denial, approval, retry, failure,
   cancellation, artifact provenance, and save/restore cases.

## Required design output

Produce a compact design packet with:

- the objective and acceptance criteria;
- a table of durable specifications and process-local runtime values;
- selected experts and their in-workflow skills/capabilities;
- deliverable contracts and artifact representations;
- a dependency-ordered step graph;
- an approval and side-effect policy table;
- stable operation IDs and versions;
- persistence and runtime-reattachment requirements;
- deterministic acceptance tests;
- unresolved decisions and assumptions.

Use Mermaid for a graph with three or more dependent steps. Keep IDs concrete
enough to carry directly into R code.

## Guardrails

- Do not confuse this Agent Skill with `tempest_skill()`. The former guides the
  coding agent; the latter creates a serializable procedure assigned to a
  Tempest expert.
- Do not store executable functions, live services, secrets, or credentials in
  objectives, experts, capabilities, workflow specifications, metadata, or
  snapshots.
- Do not add package-level domain classes merely to model host-owned customer,
  project, CRM, or tenant state. Use objective context, resources, namespaced
  metadata, and runtime services.
- Do not use output approval as a substitute for authorization before a
  side-effecting operation.
- Do not leave artifact IDs, operation versions, evidence policy, or completion
  conditions implicit.
