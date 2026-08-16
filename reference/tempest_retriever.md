# Create a TempestRetriever

Create a TempestRetriever

## Usage

``` r
tempest_retriever(
  config = tempest_config(),
  workspace = tempest_research_workspace()
)
```

## Arguments

- config:

  A `TempestConfig`.

- workspace:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md).

## Value

A `TempestRetriever`.

## Examples

``` r
retriever <- tempest_retriever(config = tempest_config())
if (FALSE) { # \dontrun{
results <- retriever$search("history of jazz", provider = "wikipedia")
} # }
```
