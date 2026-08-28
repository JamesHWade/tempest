# Build a governed daily briefing

A useful daily briefing is an attention system: it says what changed,
why the change matters, what deserves review today, and what important
topics did not change. It should not turn every observation into
accepted organizational knowledge.

This workflow needs no briefing-specific Tempest interface. The host
owns the schedule, monitored topic, presentation, and approval policy.
The packages keep their existing responsibilities:

| Module | Responsibility in the briefing |
|----|----|
| Graft | Pin accepted knowledge before research and hold reviewed revisions afterward. |
| Tempest | Produce a source-grounded research product and a review-only evidence proposal. |
| dsprrr | Run Tempest’s validated structured research stages. |
| Deputy | Execute Tempest’s permission-bounded agent work. |
| scans | Inspect the closed Tempest trajectory without reopening live product state. |

The host directly calls Tempest, Graft, and scans. Tempest uses dsprrr
and Deputy behind its product interface, so the host does not need to
coordinate their internal objects.

## Pin what the organization already knows

Open one Graft store with Tempest’s schema. Capture an immutable view
before starting the run, then select the accepted evidence that is
relevant to this briefing. An empty selection is valid on the first day.

``` r

library(tempest)
library(graft)
library(scans)

store <- graft_open(
  tempest_graft_schema(),
  "briefing/knowledge.duckdb",
  okf = "managed"
)

snapshot <- graft_snapshot(store)
view <- graft_at(store, snapshot)

topic <- paste(
  "Daily decision briefing on grid-scale battery recycling as of",
  Sys.Date(),
  "Cover what changed, why it matters, what needs review today,",
  "and material no-change signals. Preserve uncertainty and cite evidence."
)

matches <- graft_find(view, "grid-scale battery recycling", limit = 25)
accepted_classes <- c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
record_ids <- matches$id[matches$class %in% accepted_classes]
knowledge <- tempest_knowledge(view, record_ids = record_ids)
```

The snapshot fixes the accepted boundary for the whole run. A commit
made by another process halfway through the briefing cannot silently
alter its input.

## Research today’s change

Use a small expert panel that reflects the decision, not a collection of
generic personas. The topic asks for the briefing’s decision structure
while Tempest keeps the exact report, sources, claims, and claim-support
pairs behind its ordinary product interface.

``` r

technical <- tempest_expert(
  name = "Technical readiness reviewer",
  title = "Battery recycling process specialist",
  description = "Reviews demonstrated process performance and scale-up risk.",
  instructions = paste(
    "Separate measured performance from projections.",
    "Call out consequential unknowns and material no-change signals."
  ),
  focus_areas = c("process yield", "scale-up", "safety")
)

market <- tempest_expert(
  name = "Market and policy reviewer",
  title = "Battery supply-chain analyst",
  description = "Reviews policy, capacity, partnerships, and market movement.",
  instructions = paste(
    "Distinguish announcements from operating evidence.",
    "State which developments could change a decision today."
  ),
  focus_areas = c("policy", "capacity", "partnerships", "economics")
)

result <- tempest_run(
  topic,
  config = tempest_config(
    citation_policy = "claim_verified",
    on_unsupported_claim = "flag"
  ),
  knowledge = knowledge,
  experts = list(technical, market),
  max_questions_per_perspective = 2,
  output_dir = "briefing/runs",
  run_id = paste0("daily-", Sys.Date())
)

report <- tempest_report(result)
sources <- tempest_sources(result)
claims <- tempest_claims(result)
supports <- tempest_claim_supports(result)
```

Treat the report as the briefing and the tables as its evidence rail. A
host UI can lead with “What changed,” “Why it matters,” and “Review
today,” while still making unsupported or disputed claims visible.
Whether somebody was shown the briefing or read it is operational state;
it is not accepted knowledge.

## Review the evidence and the run separately

Prepare a Graft plan, but do not commit it. In parallel, project the
completed run into scans. The first review asks whether the proposed
knowledge is fit to accept; the second asks whether the agent trajectory
itself behaved well.

``` r

proposal <- tempest_promotion_bundle(result)
plan <- tempest_graft_plan(store, proposal)

review <- tempest_trajectory_review(
  result,
  promotion_bundle = proposal
)
trajectory <- scans::as_trajectory_tempest(review)

run_summary <- scans::summarize_trajectories(trajectory)
run_findings <- scans::scan_trajectories(trajectory)

plan@valid
plan@changes[, c("class", "record_id", "action", "changed_fields")]
run_summary
run_findings
```

Do not collapse these into one confidence score. Claim support, proposed
Graft changes, and trajectory findings answer different review
questions.

## Accept deliberately, including no change

If every planned action is `"match"`, today’s research proposes no
accepted state change. Publish the briefing and record that no-change
outcome in the host’s run log; do not manufacture a knowledge revision
merely to prove the schedule ran.

When the reviewer accepts a real change, commit the exact plan that was
shown and bind a Tempest receipt to the resulting immutable snapshot:

``` r

has_change <- any(plan@changes$action != "match")

if (has_change) {
  commit_result <- graft_commit(store, plan)
  receipt <- tempest_promotion_receipt(
    store,
    proposal,
    plan,
    commit_result
  )

  accepted_review <- tempest_trajectory_review(
    result,
    promotion_bundle = proposal,
    promotion_receipt = receipt
  )
}
```

The next briefing captures a new Graft snapshot and explicitly selects
the accepted records it should carry forward. Rejection leaves accepted
knowledge unchanged. This two-speed loop keeps routine monitoring
passive and makes only material changes enter the slower
decision-and-review path.

``` r

graft_close(store)
```
