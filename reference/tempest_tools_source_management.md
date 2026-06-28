# Get source/claim management tools (without web_search/fetch_url)

Returns tools for managing sources and claims, but not the web search
tools. Used when provider-native web search is enabled.

## Usage

``` r
tempest_tools_source_management(
  retriever,
  allow_claim_writes = TRUE,
  claim_provenance = list()
)
```

## Arguments

- retriever:

  A TempestRetriever object

- allow_claim_writes:

  Whether to include `add_claim` and transitional fact-writing aliases.

- claim_provenance:

  Optional provenance recorded on claims written via `add_claim`.

## Value

List of ellmer tools for source/claim management
