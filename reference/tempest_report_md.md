# Assemble a Markdown report with footnotes

Assemble a Markdown report with footnotes

## Usage

``` r
tempest_report_md(title, body, store)
```

## Arguments

- title:

  Document title.

- body:

  Markdown body text that may include inline citations like
  `[Sxxxxxxxxxxxx]`.

- store:

  A `SourceStore` containing sources.

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
