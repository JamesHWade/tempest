# Reuse and correct accepted research

A synthetic pilot first reports 82% material recovery. A later
correction says 62%. The host accepts each completed research product,
reviews which claim is superseded, and retains the exact evidence used
for the first answer.

This example runs without credentials or network access. Its two saved
promotion bundles were produced from completed Tempest STORM fixtures
using
[`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
and
[`tempest_save_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_save_promotion_bundle.md).
Their reports come from
[`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md).
The fixture builder is `tools/rebuild-accepted-research.R` in the source
repository. These are synthetic inputs for exercising acceptance and
reuse, not measured pilot results or a claim that a live model
researched this topic.

``` r

library(tempest)
example <- new.env()
sys.source(system.file("examples", "accepted-research.R", package = "tempest"), example)
result <- example$accepted_research_example()
result$selected_records
#>   initial corrected 
#>         4         4
result$reviewed_changes[, c("class", "action")]
#>   class action
#> 1 Claim update
result$original_preserved
#> [1] TRUE
```

The example creates and closes a temporary durable Graft store. It
reopens the store and checkpoint before an unchanged day. It then
accepts the correction and separately commits the host’s decision to
supersede the older claim. This is an explicit review decision;
differing model outputs alone do not invalidate accepted knowledge.

Each selected bundle retains its claim, support, evidence span and
source. Multiple selections form a union; an unchanged day preserves the
full union. The recipe stops if that complete evidence exceeds 1,000
records, rather than keeping claims while discarding their support.
Selected-record changes, including deletion tombstones, require review
before another automatic run.

The earlier checkpoint still reconstructs the earlier accepted
revisions:

``` r

cat(result$initial_report, "\n\n", result$corrected_report)
```

## Promotion evidence

The pilot recovered 82% of the material. [^1]

### References

### Execution review

- `extract_claims` attempt `attempt-persistence-03-extract_claims`:
  status `succeeded`; path `grounded`; support `unknown`; publication
  blocked.
- `refined_outline` attempt `attempt-persistence-06-refined_outline`:
  status `succeeded`; path `grounded`; support `unknown`; publication
  blocked.

\# Promotion evidence

The corrected pilot result is 62%, not 82%. [^2]

### References

### Execution review

- `extract_claims` attempt `attempt-persistence-03-extract_claims`:
  status `succeeded`; path `grounded`; support `unknown`; publication
  blocked.
- `refined_outline` attempt `attempt-persistence-06-refined_outline`:
  status `succeeded`; path `grounded`; support `unknown`; publication
  blocked.

The reports retain their execution-review annotations, including unknown
stage support and publication blocks. Host acceptance of the selected
evidence does not clear those diagnostics or establish publishable
research quality.

Readable synthesis remains host-owned checkpoint content. Graft stores
the accepted research evidence through Tempest’s existing schema; this
example adds no report class or executable procedure. The checkpoint
links a report to its exact selection and accepted boundary, but does
not prove every sentence. Source identity, source-content hash and
captured excerpts remain distinct from that prose. File retention,
sharing, atomic checkpoint publication and erasure are responsibilities
of the application.

The [daily
briefing](https://jameshwade.github.io/tempest/articles/daily-briefing.md)
applies the same selection recipe to a scheduled host. Its research step
is optional and may use a live model; the acceptance/restart/correction
example above is entirely offline.

[^1]: Promotion evidence source. <https://example.com/pilot-initial>
    (retrieved 2026-01-01T00:00:00Z).

[^2]: Promotion evidence source. <https://example.com/pilot-correction>
    (retrieved 2026-01-01T00:00:00Z).
