# Semantic fact retrieval

When ragnar is configured, retrieves semantically similar chunks and
maps them back to facts. Falls back to keyword filtering otherwise.

## Usage

``` r
tempest_semantic_filter_facts(retriever, query, store, max_items = 30)
```

## Arguments

- retriever:

  A `TempestRetriever` object.

- query:

  Search query.

- store:

  A `SourceStore` object.

- max_items:

  Maximum facts to return.

## Value

A list of fact objects.
