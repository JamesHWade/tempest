# Review a completed research product in an MCP App

**\[experimental\]**

## Usage

``` r
tempest_mcp_app(research, name = "tempest-research")
```

## Arguments

- research:

  A completed
  [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  product or succeeded, quiescent `TempestSession`.

- name:

  A non-empty MCP App name prefix. The exact review identity is appended
  so a new product gets a distinct resource. Use distinct prefixes when
  hosting multiple cards for the same product together.

## Value

A
[`shinymcp::McpApp`](https://jameshwade.github.io/shinymcp/reference/McpApp.html).
Preview with
[`shinymcp::preview_app()`](https://jameshwade.github.io/shinymcp/reference/preview_app.html),
serve with
[`shinymcp::serve()`](https://jameshwade.github.io/shinymcp/reference/serve.html),
or embed with
[`shinymcp::mcp_embed()`](https://jameshwade.github.io/shinymcp/reference/mcp_host_ui.html).

## Details

Builds a research card with a report, claim-by-span evidence, and a
bounded execution review. The three read-only tools also work in MCP
clients without interactive pages. The card captures one exact completed
product at creation; reopening or inspecting it never runs research or
changes accepted knowledge.

The model receives a compact summary and can request a particular claim
or the report. The page receives the complete report and presentation
tables. Raw HTML and unsafe links in the report are suppressed when it
is rendered. Graft acceptance and withdrawal remain explicit host
operations; this card cannot record review decisions or authorize reuse.

## See also

[`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md),
[`tempest_publish_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_publish_artifact_research.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- tempest_run("Battery recycling")
card <- tempest_mcp_app(result)
shinymcp::preview_app(card)

# A deterministic ecosystem demonstration, without credentials or network.
tempest_app(demo = TRUE)
} # }
```
