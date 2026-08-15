# Inspect and control a generic Tempest run

**\[experimental\]**

## Usage

``` r
tempest_run_status(run)

tempest_run_events(run, after_sequence = 0L)

tempest_run_approvals(run, status = NULL)

tempest_run_artifacts(run, ...)

tempest_run_capability_grants(run)

tempest_run_artifact(run, artifact_id)

tempest_run_record_approval(
  run,
  approval_id,
  decision = c("approved", "rejected"),
  note = NULL,
  metadata = list(),
  resume = TRUE
)

tempest_run_request_cancel(run, reason = "Cancellation requested.")
```

## Arguments

- run:

  A `TempestRun` created by
  [`tempest_run_workflow()`](https://jameshwade.github.io/tempest/reference/tempest_run_workflow.md)
  or restored by
  [`tempest_run_restore()`](https://jameshwade.github.io/tempest/reference/tempest_run_restore.md).

- after_sequence:

  Return only events whose sequence is greater than this non-negative
  run-local cursor.

- status:

  Optional approval status filter: `"pending"`, `"approved"`,
  `"rejected"`, or `"cancelled"`.

- ...:

  Filters forwarded to the run's typed artifact catalog.

- artifact_id:

  Stable artifact identifier.

- approval_id:

  Stable approval-request identifier.

- decision:

  Either `"approved"` or `"rejected"`.

- note:

  Optional human-readable decision note.

- metadata:

  Canonical JSON-compatible decision metadata.

- resume:

  Whether to resume execution immediately after recording the decision.

- reason:

  Human-readable cancellation reason.

## Value

`tempest_run_status()` returns one run-status string.

`tempest_run_events()` returns an ordered list of generic event records.

`tempest_run_approvals()` returns a named list of approval records.

`tempest_run_artifacts()` returns typed artifact metadata records.

`tempest_run_capability_grants()` returns the latest grant records
grouped by workflow step and expert, plus per-attempt grant history for
retried steps.

`tempest_run_artifact()` returns one typed artifact, including its
inline content or external storage reference.

Control functions return `run` invisibly.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

These functions provide host applications with stable access to mutable
`TempestRun` state without reaching into R6 fields or methods directly.
Event filtering preserves run-local sequence order. Approval controls
remain nonblocking: automatic resume stops again if another approval is
required.

**Host record contracts.**

Run status is one of `pending`, `running`, `awaiting_approval`,
`succeeded`, `failed`, `cancel_requested`, `cancelled`, or
`partially_recovered`.

Each event record contains `event_id`, a positive run-local `sequence`,
`run_id`, `workflow_id`, `event_type`, `status`, `timestamp`, and a
serializable `payload`. Context fields `step_id`, `attempt`,
`expert_id`, `artifact_id`, `approval_id`, and `message` are `NULL` when
they do not apply.

Approval records are named by `approval_id` and contain `approval_kind`
(`step` or `artifact`), `step_id`, `artifact_ids`, `status`, `reason`,
`policy_decision_id`, `requested_at`, `decided_at`, `note`, and
serializable `metadata`.

Capability grants are named by step id. Each step record contains the
latest `attempt`, per-expert `experts` grants, step-level `step` grants,
`recorded_at`, and a named `attempts` history. Individual grants contain
capability and operation ids and versions, required and status flags,
connection reference ids, denial reason fields, and serializable
metadata.
