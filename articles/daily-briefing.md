# Build a recurring decision briefing

A useful daily or weekly briefing is an attention system. It answers
four questions without manufacturing novelty:

1.  **What changed?** Verified claims that do not restate what the team
    already accepted.
2.  **Why does it matter?** Labeled, confidence-calibrated assessments
    bound to their supporting claims.
3.  **What needs review today?** Concrete proposed actions, clearly
    distinct from established facts.
4.  **What held steady?** Explicit no-change signals—emitted only when
    today’s verified evidence actually reconfirms an accepted claim
    (never when evidence is missing or inconclusive).

You do not need a separate briefing framework. Your host script owns the
schedule and approval policy, **Tempest** runs the research and computes
the change signal structurally against pinned accepted claims, **Graft**
stores exact selections and human review decisions, and **scans** audits
completed trajectories.

## 1. Load your accepted briefing basis

Start by opening your Graft store and checking whether a previous
accepted checkpoint exists. On day one, `knowledge` is `NULL` and every
verified claim is new. On later runs, reuse the exact previously
accepted decision after verifying current eligibility:

``` r

library(tempest)
library(graft)
source(system.file("examples", "briefing-basis.R", package = "tempest"))

store <- graft_store("briefing/memory", create = TRUE)
basis_path <- "briefing/basis.rds"
knowledge <- NULL

if (file.exists(basis_path)) {
  basis <- readRDS(basis_path)
  # Inspect any decisions recorded since the last run
  changes <- briefing_changes(store, basis)
  knowledge <- reuse_briefing_basis(
    store,
    basis,
    eligible = function(event) host_allows(event)
  )
}
```

If another reviewer corrected or withdrew the stream in the meantime,
`reuse_briefing_basis()` stops rather than silently substituting a
different basis—giving your host a chance to inspect `changes` first.

## 2. Run today’s briefing

Pass `knowledge` into
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md).
Use a focused expert panel matched to the decision at hand:

``` r

experts <- list(
  tempest_expert(
    name = "Technical reviewer",
    title = "Process and scale-up specialist",
    description = "Evaluates demonstrated operating performance and technical risk.",
    instructions = "Separate measured plant results from projections.",
    focus_areas = c("yield", "scale-up", "safety")
  ),
  tempest_expert(
    name = "Market and policy reviewer",
    title = "Supply-chain and regulatory analyst",
    description = "Tracks policy changes, capacity expansions, and economics.",
    instructions = "Distinguish binding rules and operating plants from announcements.",
    focus_areas = c("policy", "capacity", "economics")
  )
)

result <- tempest_run(
  paste(
    "Decision briefing on grid-scale battery recycling as of", Sys.Date(),
    "highlighting what changed, why it matters, what needs review,",
    "and any material no-change signal backed by verified evidence."
  ),
  config = tempest_config(
    models = "openai/gpt-5.6-luna",
    citation_policy = "claim_verified",
    on_unsupported_claim = "flag"
  ),
  knowledge = knowledge,
  experts = experts,
  max_questions_per_perspective = 2,
  output_dir = "briefing/runs",
  run_id = paste0("briefing-", Sys.Date())
)

report <- tempest_report(result)
claims <- tempest_claims(result)
supports <- tempest_claim_supports(result)
```

Tempest decides structurally—not just via prompts—whether each verified
claim belongs under **What changed** or **No material change** by
comparing it against active `Claim` records in `knowledge`.

## 3. Publish, review, and accept deliberately

Publish today’s run as a candidate selection in Graft and inspect its
trajectory before deciding whether to advance the accepted briefing
stream:

``` r

selection <- tempest_publish_artifact_research(result, store)
review <- tempest_trajectory_review(result)

# On an unchanged day, keep the briefing report in your run log without
# manufacturing a new accepted knowledge revision.
# When a reviewer confirms a real update or correction, advance the stream:
previous <- if (file.exists(basis_path)) basis$decision else NULL

accepted <- graft_accept(
  store,
  graft_read_selection(store, selection),
  stream = "battery-briefing",
  expected = previous,
  key = paste0("review-", Sys.Date()),
  actor = "briefing-reviewer",
  reason = "Reviewed today's new claims and source spans",
  purpose = "briefing"
)

basis <- capture_briefing_basis(store, "battery-briefing", accepted@id, "briefing")
saveRDS(basis, basis_path)
```

Because `expected = previous` is explicit, two reviewers cannot silently
overwrite each other’s decisions. If evidence later turns out to be
flawed, record a withdrawal—future runs stop consulting that stream
while historical briefings remain readable:

``` r

graft_withdraw(
  store,
  stream = "battery-briefing",
  expected = basis$decision,
  key = paste0("withdraw-", Sys.Date()),
  actor = "briefing-reviewer",
  reason = "Primary source retracted; re-evaluate next cycle"
)

# Historical inspection remains available even after withdrawal
historical <- read_briefing_basis(store, basis)
```

## 4. Pair with interactive review and `scans`

You can open any briefing run inside `tempest_mcp_app(result)` for
read-only claim inspection, serve your accepted `"battery-briefing"`
stream to chat agents with
[`graft::graft_tools()`](https://jameshwade.github.io/graft/reference/graft_tools.html),
or project
[`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md)
into `scans` to monitor run reliability over time. See [Research that
remembers with
Graft](https://jameshwade.github.io/tempest/articles/research-memory.md)
for the full composition guide.
