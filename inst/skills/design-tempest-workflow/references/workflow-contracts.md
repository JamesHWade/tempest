# Tempest workflow contract map

Use this map to turn application requirements into Tempest's public workflow
contracts.

## Durable specifications

| Requirement | Public constructor | Design rule |
|---|---|---|
| Requested outcome | `tempest_objective()` | State constraints and observable acceptance criteria. |
| Approved evidence/input | `tempest_resource()` | Use an opaque locator; keep authenticated access in runtime bindings. |
| Expert identity | `tempest_expert()` | Store identity, procedure, and permission requirements, not a live chat. |
| Expert procedure | `tempest_skill()` | Refer to capabilities and operation IDs; do not embed functions. |
| Callable permission | `tempest_capability_spec()` | Declare operation, connections, roles, schemas, and side-effect status. |
| Authenticated system | `tempest_connection_ref()` | Store a non-secret opaque reference and scope labels only. |
| Requested output | `tempest_deliverable_spec()` | Define generation, validation, render/export operations, evidence, and approval. |
| Executable unit | `tempest_workflow_step()` | Declare dependencies, artifacts, assignment, capabilities, retry, and policy gates. |
| Directed workflow | `tempest_workflow_spec()` | Use stable versioned IDs and an acyclic graph. |

All durable fields and metadata must be canonical JSON-compatible. Use stable
IDs and explicit versions wherever a specification refers to a runtime
operation.

## Process-local runtime

| Runtime concern | Public surface | Design rule |
|---|---|---|
| Step/generator/validator/renderer/exporter functions | `tempest_operation_registry()` | Register by stable ID, version, and kind. |
| Complete runtime | `tempest_runtime()` | Bind operations, skill/capability specs, implementations, and connections. |
| Capability implementation | `capability_implementations` | Use factories that run only after authorization. |
| Authenticated client | `connection_bindings` | Never serialize it or expose credentials in events. |
| Host service | `runtime_context` | Pass retrievers, managers, or application services explicitly. |
| Authorization | `policy_adapter` and `connection_permissions` | Apply least privilege per run, expert, and role. |
| Observation | `progress` and run accessors | Consume ordered public event records. |

Generic run restoration is not part of the Tempest 0.2 product API. This table
is retained only as section-10 deletion inventory; do not reattach or resume
these process-local values through product bundles.

## Graph invariants

- Step IDs are unique and dependencies are acyclic.
- Every required input artifact has exactly one producer.
- A consuming step depends directly or transitively on that producer.
- Every declared produced artifact is published with the current run ID and
  step ID before the step can count as complete.
- Exact assignment rules name only experts in the selected run pool.
- Required and optional capability IDs are disjoint.
- Operation ID, version, and kind resolve during preflight before execution.

## Approval and side-effect choices

Use the narrowest boundary that matches the risk:

- `approval_checkpoint = TRUE`: pause before the step operation runs.
- `requires_approval = TRUE` on a deliverable: validate and render output, then
  pause before approval-dependent export or workflow completion.
- `side_effecting = TRUE`: declare external mutation and require the host
  policy to authorize it. This is not replaced by later output review.

Persist approval requests and decisions as run state. On restore, reject
decisions that target another request, step, run, or artifact set.

## Evidence policy

Choose deliberately:

- `none`: evidence is not required.
- `source_attributed`: preserve source attribution.
- `claim_verified`: require claim-oriented validation.
- `strict`: use the strongest evidence enforcement available to the workflow.

Carry resource, claim, and evidence-span IDs into artifact provenance when the
deliverable depends on evidence.

## Built-in versus custom

Prefer `tempest_storm_workflow_run()` or `tempest_costorm_workflow_run()` when
the desired outcome is Tempest's research/report workflow. Use a custom
workflow when the host needs different deliverables, domain operations,
permissions, expert assignments, approval gates, or side effects. A custom
workflow may still reuse built-in operations and capabilities.
