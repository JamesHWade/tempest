# Decompose a research question into targeted search queries

Decompose a research question into targeted search queries

## Usage

``` r
tempest_decompose_query(chat, question, topic, module = NULL, max_queries = 3)
```

## Arguments

- chat:

  An ellmer chat object.

- question:

  The research question to decompose.

- topic:

  The overall research topic.

- module:

  Optional dsprrr module for query decomposition.

- max_queries:

  Maximum number of queries to return.

## Value

A list with a `queries` character vector.
