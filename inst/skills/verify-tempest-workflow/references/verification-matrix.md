# Tempest workflow verification matrix

Select the applicable cases for the workflow under review. Add domain-specific
cases where a host operation can mutate external state or expose protected
data.

## Definitions and graph

| Case | Expected result |
|---|---|
| Duplicate step ID | Construction fails. |
| Unknown dependency | Construction fails. |
| Dependency cycle | Construction fails. |
| Two producers for one artifact ID | Construction fails. |
| Required input without producer | Construction fails. |
| Consumer not dependent on producer | Construction fails. |
| Required and optional capability overlap | Construction fails. |
| Non-serializable metadata or credential-like value | Construction fails or value is excluded by the documented boundary. |

## Runtime preflight

| Case | Expected result |
|---|---|
| Missing step operation | Fails before a run side effect. |
| Operation version mismatch | Fails during preflight. |
| Operation kind mismatch | Fails during preflight. |
| Exact assignment names unselected expert | Fails explicitly. |
| Required capability unavailable or retired | Step is denied or fails explicitly before implementation runs. |
| Runtime context missing | Classed failure with inspectable run when construction already succeeded. |

## Capabilities and connections

| Case | Expected result |
|---|---|
| Allowed expert and allowed connection | Capability resolves and grant is recorded. |
| Allowed capability but ungranted connection | Denied before factory/client use. |
| Wrong model role | Denied. |
| Side-effecting capability without policy approval | Denied or paused according to policy. |
| Factory throws | Failure is classed; secrets are absent from event payloads. |
| Retry | Per-attempt grant history is retained. |
| Restore with narrower permissions | Narrowing succeeds. |
| Restore with broader permissions | Broadening is rejected. |

## Approvals

| Case | Expected result |
|---|---|
| Step checkpoint | Operation has not run before approval. |
| Rejected step checkpoint | Step does not run; terminal state follows policy. |
| Approval-required artifact | Artifact is validated and awaiting approval; approval-dependent export has not run. |
| Approved artifact | Owning artifact transitions correctly and eligible execution resumes. |
| Rejected artifact | Workflow does not report success for the rejected requested output. |
| Approval ID from another run | Rejected. |
| Approval targets changed artifact set or step | Rejected. |
| Duplicate or stale decision | Idempotent only when semantically identical; conflicting decisions are rejected. |
| Partially persisted approval state | Restore rejects it or requires explicit partial recovery. |

## Artifacts and deliverables

| Case | Expected result |
|---|---|
| Validator passes | Artifact is valid or awaiting approval. |
| Validator fails | Invalid artifact and diagnostics remain inspectable. |
| Retry replaces invalid output | Only the owned draft/invalid artifact is replaced; diagnostics are retained. |
| Declared artifact not published | Step cannot complete successfully. |
| Artifact owned by another run/step | Completion check rejects it. |
| Exporter mutates immutable fields | Rejected. |
| External storage read has mismatched identity | Rejected. |
| Custom codec unavailable after restore | Deterministic failure or documented fallback without type confusion. |
| Evidence-required output lacks lineage | Validation or completion fails according to policy. |

## Execution and observation

| Case | Expected result |
|---|---|
| Success | Requested outputs exist and run status is `succeeded`. |
| Retryable operation failure | Attempts are bounded by policy and ordered events describe them. |
| `failure_policy = "stop"` | Downstream work does not run. |
| `failure_policy = "continue"` | Only eligible independent work continues; requested-output checks still apply. |
| Cancellation request | No later step or external side effect begins after the cancellation boundary. |
| Progress callback failure | Workflow behavior follows the documented observer contract without corrupting run state. |
| Event replay cursor | Sequences are strictly increasing and filtering resumes after the cursor. |

## Persistence and host integration

| Case | Expected result |
|---|---|
| Complete bundle save/restore | Snapshot and manifest checksums validate. |
| Missing or changed bundle file | Restore fails explicitly. |
| In-flight state without partial recovery | Restore refuses ambiguous continuation. |
| Runtime values in snapshot | No functions, clients, callbacks, policy adapters, or credentials are present. |
| Restored run | Runtime is reattached explicitly and execution does not start automatically. |
| Shiny approval flow | UI state, run state, and artifact state remain aligned across reactive invalidation. |
| Background operation | Session lifetime, cancellation, stale callbacks, and ordered commits are exercised. |
| External store/exporter | Real adapter contract is tested with local fakes and, when required, a live host smoke test. |
