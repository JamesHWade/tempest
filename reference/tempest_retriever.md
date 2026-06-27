# Create a TempestRetriever

Create a TempestRetriever

## Usage

``` r
tempest_retriever(config = tempest_config(), store = SourceStore$new())
```

## Arguments

- config:

  A `TempestConfig`.

- store:

  A `SourceStore`.

## Value

A `TempestRetriever`.

## Examples

``` r
retriever <- tempest_retriever(config = tempest_config())
if (FALSE) { # \dontrun{
results <- retriever$search("history of jazz", provider = "wikipedia")
} # }
```
