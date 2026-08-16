# Create a TempestRetriever

Create a TempestRetriever

## Usage

``` r
tempest_retriever(
  config = tempest_config(),
  store = tempest_research_workspace()
)
```

## Arguments

- config:

  A `TempestConfig`.

- store:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md).
  The argument name is retained for compatibility.

## Value

A `TempestRetriever`.

## Examples

``` r
retriever <- tempest_retriever(config = tempest_config())
if (FALSE) { # \dontrun{
results <- retriever$search("history of jazz", provider = "wikipedia")
} # }
```
