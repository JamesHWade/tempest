# Inspect the Tempest 0.1 generic-kernel deletion inventory

> **Lifecycle notice:** The experimental generic workflow kernel
> documented in this article remains only as section-10 deletion
> inventory in the Tempest 0.2 migration train. It is not a
> compatibility path, and no migration shim is planned. Use Tempest’s
> STORM and Co-STORM product APIs.

Tempest 0.1 included a workflow kernel that was not limited to research
reports. The baseline below shows how that machinery turned a customer
request, product objective, or internal task into a typed outcome.

This article preserves a small action-register workflow so the deletion
scope remains reviewable. It runs entirely in R, without an API key or
network connection.

## Understand the Tempest 0.1 workflow model

A frozen Tempest 0.1 workflow has four parts:

| Part | Responsibility |
|----|----|
| Specifications | Serializable objectives, experts, deliverables, and workflow steps |
| Runtime | Process-local R functions, capabilities, connections, and services |
| Run | Mutable execution state, assignments, approvals, events, and cancellation |
| Artifacts | Validated, typed outputs with provenance and persistence |

The separation between specifications and runtime code is deliberate.
Specifications can be stored, compared, and restored. Functions,
authenticated clients, credentials, and other live services stay in the
host process and must be reattached when a run is restored.

In the frozen 0.1 implementation, Tempest owns the generic execution
contract. That ownership is superseded by the Tempest 0.2
package-boundary decision. The host application still owns its domain
model, expert selection, operation implementations, policy decisions,
credentials, and user interface.

## Describe the requested outcome

An objective records what the caller wants, the constraints that apply,
and the observable conditions for completion. Host-specific context
belongs in `context` or namespaced `metadata`; Tempest does not need a
customer or project class in its own package API.

``` r

library(tempest)

objective <- tempest_objective(
  "Turn the approved customer request into a reviewable action register.",
  title = "Prepare the implementation review",
  context = list(
    customer_segment = "enterprise",
    requested_window = "next planning cycle"
  ),
  constraints = c(
    "Do not invent commitments.",
    "Keep unresolved decisions visible."
  ),
  acceptance_criteria = c(
    "Every action has an owner.",
    "Every action has an observable completion signal."
  ),
  deliverable_ids = "action-register"
)
```

The host also chooses the exact expert pool for the run. Profiles are
durable definitions of identity and procedure, not live model sessions
or tool closures.

``` r

expert_pool <- list(
  delivery = tempest_expert(
    expert_id = "expert.delivery",
    name = "Delivery Analyst",
    title = "Implementation specialist",
    description = "Turns requests into executable plans.",
    instructions = paste(
      "Surface owners, completion signals, dependencies,",
      "and unresolved decisions."
    ),
    focus_areas = c("delivery planning", "decision readiness")
  ),
  risk = tempest_expert(
    expert_id = "expert.risk",
    name = "Risk Analyst",
    title = "Delivery risk specialist",
    description = "Identifies assumptions and execution risks.",
    instructions = "Make uncertainty and missing decisions explicit.",
    focus_areas = c("dependencies", "risk controls")
  )
)

selected_experts <- expert_pool["delivery"]
```

Passing `selected_experts` to the run is the selection decision.
Assignment rules in the workflow can only assign experts from that
supplied pool.

## Specify the output template

A deliverable specification is the reusable output template. It names
the runtime operations that generate, validate, render, and optionally
export the outcome. It also records the expected fields, media types,
evidence policy, and approval requirement.

``` r

deliverable <- tempest_deliverable_spec(
  "action-register",
  version = "1",
  title = "Action register",
  purpose = "Turn the objective into concrete next actions.",
  instructions = paste(
    "Return a summary and concrete actions with owners",
    "and completion signals."
  ),
  content_schema = list(
    type = "object",
    required = c("summary", "actions", "risks")
  ),
  required_fields = c("summary", "actions", "risks"),
  evidence_policy = "none",
  generator_id = "tempest.generator.provided_content",
  validator_ids = "tempest.validator.required_fields",
  renderer_ids = "example.renderer.action_register",
  content_type = "action-register",
  media_types = "application/json",
  operation_versions = c(
    "tempest.generator.provided_content" = "1",
    "tempest.validator.required_fields" = "1",
    "example.renderer.action_register" = "1"
  ),
  requires_approval = TRUE
)
```

`content_schema` records the contract for hosts and downstream tooling.
Validation is performed only by the operations in `validator_ids`; here
the built-in required-fields validator enforces the three declared
fields.

One deliverable may render several artifact representations. For
example, the same canonical content could become both JSON for another
system and Markdown for a reviewer.

## Implement the runtime operations

Executable functions live in an operation registry. Stable operation IDs
and versions connect these functions to the serializable specifications.

The built-in registry provides a generator for host-supplied content and
the required-fields validator. This example adds a JSON renderer and the
workflow step that prepares the action register.

``` r

operations <- tempest_builtin_operation_registry()

operations$register(
  "example.renderer.action_register",
  kind = "renderer",
  version = "1",
  implementation = function(content) {
    tempest_artifact_representation(
      content = content,
      artifact_kind = "action-register",
      media_type = "application/json"
    )
  }
)

operations$register(
  "example.step.plan",
  kind = "step",
  version = "1",
  implementation = function(
    objective,
    expert_id,
    artifact_catalog,
    run_id,
    step
  ) {
    content <- list(
      summary = objective@description,
      actions = list(
        list(
          action = "Confirm the implementation scope",
          owner = expert_id,
          completion_signal = "Scope is approved"
        ),
        list(
          action = "Review the proposed delivery decision",
          owner = "host.project-owner",
          completion_signal = "Decision is approved or revised"
        )
      ),
      risks = list(
        list(
          risk = "The requested window may not match available capacity",
          response = "Confirm capacity before making a commitment"
        )
      )
    )

    tempest_generate_deliverable(
      deliverable,
      context = list(content = content),
      registry = operations,
      catalog = artifact_catalog,
      provenance = list(
        artifact_id = "action-register-json",
        run_id = run_id,
        step_id = step@step_id,
        expert_id = expert_id
      )
    )
  }
)
```

Operations receive the named execution-context arguments they declare.
They do not need to accept a package-wide context object or know about
R6 internals. The step delegates output validation, rendering,
checksums, provenance, and catalog publication to
[`tempest_generate_deliverable()`](https://jameshwade.github.io/tempest/reference/tempest_generate_deliverable.md).

## Assemble and run the workflow

A workflow specification declares the step graph and assignment rules. A
step’s `produced_artifact_ids` are completion promises: the step must
publish those artifacts with provenance for the current run and step.

``` r

workflow <- tempest_workflow_spec(
  "example.action-register",
  version = "1",
  title = "Action-register workflow",
  purpose = "Create a reviewed action register.",
  supported_deliverable_types = "action-register",
  steps = list(tempest_workflow_step(
    "plan",
    title = "Plan",
    purpose = "Prepare the action register.",
    operation_id = "example.step.plan",
    produced_artifact_ids = "action-register-json",
    assignment_rule = "expert.delivery"
  ))
)

run <- tempest_run_workflow(
  objective,
  workflow,
  runtime = operations,
  experts = selected_experts,
  deliverables = list(deliverable)
)

tempest_run_status(run)
#> [1] "awaiting_approval"
```

The step has completed, but `requires_approval = TRUE` leaves its
validated artifact in `awaiting_approval`. The producing operation is
not rerun when the host records a decision.

``` r

pending <- tempest_run_approvals(run, status = "pending")
pending[[1]][c("approval_kind", "status")]
#> $approval_kind
#> [1] "artifact"
#> 
#> $status
#> [1] "pending"

tempest_run_record_approval(
  run,
  names(pending)[[1]],
  decision = "approved",
  note = "Approved by the host application."
)

tempest_run_status(run)
#> [1] "succeeded"
```

Use `approval_checkpoint = TRUE` on a workflow step when approval is
needed *before* the operation executes. Use `requires_approval = TRUE`
on a deliverable when a human or host policy must review the generated
output. Side-effecting capabilities and steps also pass through the
policy boundary.

## Inspect the artifact and audit trail

Host applications should use the public run accessors instead of
reaching into mutable R6 fields.

``` r

artifact <- tempest_run_artifact(run, "action-register-json")

c(
  status = artifact@status,
  media_type = artifact@media_type
)
#>             status         media_type 
#>         "approved" "application/json"

artifact@content$actions[[1]]
#> $action
#> [1] "Confirm the implementation scope"
#> 
#> $owner
#> [1] "expert.delivery"
#> 
#> $completion_signal
#> [1] "Scope is approved"

events <- tempest_run_events(run)
event_summary <- data.frame(
  sequence = vapply(events, `[[`, integer(1), "sequence"),
  event_type = vapply(events, `[[`, character(1), "event_type"),
  status = vapply(events, `[[`, character(1), "status")
)

utils::tail(event_summary, 6)
#>    sequence         event_type            status
#> 7         7 approval.requested awaiting_approval
#> 8         8  artifact.approved          approved
#> 9         9  approval.resolved          approved
#> 10       10     step.succeeded         succeeded
#> 11       11   workflow.running           running
#> 12       12 workflow.completed         succeeded
```

Artifacts retain their deliverable version and fingerprint, validation
results, checksum, run and step ownership, expert attribution, evidence
lineage, and approval status. Events use a strictly increasing sequence
within the run, so a UI or external observer can resume from its last
seen cursor.

## Treat generic persistence as deletion inventory

Generic run-bundle restoration is no longer part of the Tempest 0.2
product API. Its implementation remains frozen only so the section-10
deletion PR can remove the generic kernel as a coherent unit. New
integrations must not call the internal generic restore helpers or
depend on their artifact-catalog and workflow-run records.

Persist supported research products through
[`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
for Co-STORM or the `output_dir` bundle written by
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
for STORM. Those formats retain the correlated research manifest,
explicit research workspace, narrow report product, and optional
immutable Graft snapshot without serializing executable operations, live
services, chats, tools, credentials, or generic workflow state.

## Maintain skills, capabilities, and connections

The simple example uses a pure R operation. An existing Tempest 0.1 host
may already give each expert durable skill and capability requirements
while keeping authenticated clients and credentials outside the profile:

``` r

planning_skill <- tempest_skill(
  "planning.synthesis",
  title = "Planning synthesis",
  purpose = "Turn approved context into an implementation plan.",
  instructions = "Preserve constraints, dependencies, and uncertainty.",
  required_capability_ids = "customer-context.read"
)

customer_context <- tempest_connection_ref(
  "approved-customer-context",
  provider_id = "example.host",
  connection_type = "customer-context",
  title = "Approved customer context",
  description = "Read-only context selected for this run.",
  scopes = "read"
)

read_customer_context <- tempest_capability_spec(
  "customer-context.read",
  title = "Read approved customer context",
  purpose = "Use only the context approved for this workflow run.",
  instructions = "Do not access ungranted customer records.",
  operation_id = "example.capability.customer_context_read",
  connection_ref_ids = customer_context@connection_id,
  model_roles = "expert"
)
```

The frozen 0.1 host reconstructs a
[`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
with these specifications, process-local capability implementations, and
connection bindings. Experts refer to skills and capabilities by ID.
`connection_permissions` on
[`tempest_run_workflow()`](https://jameshwade.github.io/tempest/reference/tempest_run_workflow.md)
grants only the opaque connection IDs allowed for that expert or model
role.

Capability factories run only after authorization. The resulting
decisions are available through
[`tempest_run_capability_grants()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md),
while tools, clients, and credentials never enter profiles, events,
snapshots, or bundles.

The bundled frozen 0.1 Shiny host example connects these pieces end to
end, including process-local connection bindings, capability
authorization, step and artifact approvals, typed JSON output, and the
generic Shiny run adapter:

``` r

shiny::runApp(
  system.file("examples/shiny-host", package = "tempest")
)
```

## Review the frozen Tempest 0.1 integration boundary

The deletion-owned baseline keeps these host-owned parts explicit:

1.  Map the incoming request to a
    [`tempest_objective()`](https://jameshwade.github.io/tempest/reference/tempest_objective.md).
2.  Select profiles from the host’s expert pool.
3.  Define versioned deliverable specifications for the required output
    shapes.
4.  Register generators, validators, renderers, exporters, and step
    operations.
5.  Declare the workflow graph, assignments, retries, and approval
    checkpoints.
6.  Attach policy, connections, runtime services, and the host UI.

These frozen run, artifact, validation, event, approval, cancellation,
and persistence contracts are not part of the Tempest 0.2 product API.
Do not extend or adopt them.

[`tempest_storm_workflow_run()`](https://jameshwade.github.io/tempest/reference/tempest_storm_workflow_run.md)
and
[`tempest_costorm_workflow_run()`](https://jameshwade.github.io/tempest/reference/tempest_costorm_workflow_run.md)
remain only for the section-10 deletion PR. Research code should call
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
or
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
directly.
