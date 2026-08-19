# Render non-authoritative Markdown with footnotes

`tempest_report_md()` deterministically renders caller-supplied Markdown
and evidence from an explicit
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md).
It does not commit a report, finalize a research Manifest, publish a
product, or grant promotion authority. Read `result$report_md` from a
completed
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
product, or use
[`tempest_session_report_md()`](https://jameshwade.github.io/tempest/reference/tempest_session_report_md.md)
after Co-STORM publication, when the exact authoritative product report
is required.

## Usage

``` r
tempest_report_md(
  title,
  body,
  workspace,
  citation_policy = "source_attributed",
  on_unsupported_claim = "flag",
  min_support_score = 0.7
)
```

## Arguments

- title:

  Document title.

- body:

  Markdown body text that may include inline citations like
  `[Sxxxxxxxxxxxx]`.

- workspace:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  or
  [TempestRetriever](https://jameshwade.github.io/tempest/reference/TempestRetriever.md)
  containing retrieved sources.

- citation_policy:

  One of "none", "source_attributed" (default), "claim_verified",
  "strict". "none" leaves inline citation ids unchanged and omits
  references. Under verified policies, footnotes show a verification
  badge; under "strict", unsupported/contradicted inline citations are
  handled per `on_unsupported_claim`.

- on_unsupported_claim:

  One of "flag" (default), "drop", "keep_with_warning", or "revise".
  Under strict policy, `drop` removes the unsupported assertion and
  `revise` replaces it with a revision notice.

- min_support_score:

  Minimum support score in `[0, 1]` for a claim to be considered
  supported.

## Value

Rendered Markdown with footnotes. This value alone is not an
authoritative product report.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
md <- tempest_report_md(
  title = "History of Jazz",
  body = result$draft_md,
  workspace = result$workspace
)
} # }
```
