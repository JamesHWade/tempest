# Assemble a Markdown report with footnotes

Assemble a Markdown report with footnotes

## Usage

``` r
tempest_report_md(
  title,
  body,
  store,
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

- store:

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

Markdown with footnotes.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("History of jazz", config = tempest_config())
md <- tempest_report_md(
  title = "History of Jazz",
  body = result$draft_md,
  store = result$store
)
} # }
```
