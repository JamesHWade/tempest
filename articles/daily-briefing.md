# A daily briefing with persistent evidence

A daily briefing should distinguish new evidence, an unchanged finding,
and a correction. Its earlier reports and evidence must remain
inspectable after the next run. Tempest produces and validates the
research; the host reviews it; Graft retains exact artifacts and
decisions.

## Ownership

| Component | Responsibility |
|----|----|
| Host | Schedule, topic, credentials, review, access, and retention policy |
| Tempest | Evidence, verification, report, scientific provenance, saved research |
| Graft | Exact artifact selections and explicit host decision history |
| scans | Inspection through Tempest’s validated public trajectory projection |

## Start with a retained basis

These are host integration snippets, not a scheduled service. Supply
real current eligibility checks and model/search configuration in the
application.

``` r

library(tempest)
library(graft)
source(system.file("examples", "briefing-basis.R", package = "tempest"))
store <- graft_artifact_store("briefing/artifacts", create = TRUE)
basis_path <- "briefing/basis.rds"
knowledge <- NULL
if (file.exists(basis_path)) {
  basis <- readRDS(basis_path) # Trusted host-owned checkpoint.
  changes <- briefing_changes(store, basis)
  historical <- read_briefing_basis(store, basis)
  # Inspect changed decisions before choosing the basis for another run.
  knowledge <- reuse_briefing_basis(
    store, basis, eligible = function(event) host_allows(event)
  )
}
```

The checkpoint names one exact decision, selection, stream, and purpose.
Reading history verifies the retained evidence and report without
granting current admission. Reuse also checks that the decision is the
current accepted head, that the purpose matches, and that the host
permits consultation now. A stale, withdrawn, or disallowed basis fails;
it is never silently replaced by “latest.”

## Run and review

``` r

result <- tempest_run(
  "Battery recycling: evidence that changes today's operating decisions",
  knowledge = knowledge,
  config = tempest_config(),
  output_dir = "briefing/runs"
)
report <- tempest_report(result)
review <- tempest_trajectory_review(result)
selection <- tempest_publish_artifact_research(result, store)
```

Publishing stores a candidate and its exact report, sources, spans,
claims, supports, and program provenance. It does not accept the
research. The host reviews the complete evidence before making an
explicit decision.

``` r

previous <- if (file.exists(basis_path)) basis$decision else NULL
accepted <- graft_artifact_decide(
  store, "battery-briefing", "review-2026-09-18", expected = previous,
  selection = selection, action = "accept", actor = "reviewer",
  reason = "Evidence and proposed changes reviewed", purpose = "briefing"
)
basis <- capture_briefing_basis(
  store, "battery-briefing", accepted$id, "briefing"
)
saveRDS(basis, basis_path)
```

The predecessor is explicit. If another review has advanced the stream,
this request fails and the host reviews the changed state. Retrying the
same committed request returns its original event; it cannot restore
eligibility after withdrawal.

## Unchanged days and corrections

Tempest compares verified statements with active Claim records in the
admitted artifact selection. A verified restatement supports a no-change
finding; it cannot masquerade as a new observation. The report still
needs exact evidence.

An unchanged day can retain its new run and report without replacing the
accepted basis. An explicit new review of the same selection creates a
distinct decision. A correction accepts a new complete selection, making
the old event ineligible for new consultation while keeping the old
report and bytes readable. Nothing automatically unions evidence across
decisions or accepts model output.

``` r

withdrawn <- graft_artifact_decide(
  store, basis$stream, "withdraw-2026-09-19", expected = basis$decision,
  selection = basis$selection, action = "withdraw", actor = "reviewer",
  reason = "Evidence needs reconsideration", purpose = basis$purpose
)
historical <- read_briefing_basis(store, basis)
# reuse_briefing_basis() now fails for this event.
```

## Save, inspect, and resume

Saved STORM and Co-STORM products contain their exact artifact selection
and materialized evidence, not a native graph object or snapshot
sidecar. Offline inspection requires neither a Graft store nor
permission to start a new run. Resuming a product with accepted inputs
requires a fresh matching knowledge value and current admission. The
host controls interruptions and checks during active work; a prior
admission is not a lasting permit.

The offline [artifact research
example](https://jameshwade.github.io/tempest/articles/artifact-knowledge.md)
exercises acceptance, an unchanged review, correction, and withdrawal.
The daily-briefing tests also reopen checkpoints in a fresh process.
These synthetic fixtures are contract evidence, not a claim that a live
model researched the example topic.
