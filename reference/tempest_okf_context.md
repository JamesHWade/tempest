# Assemble bounded agent context from an Open Knowledge Format bundle

Context begins with an explicit trust boundary: OKF concepts are
evidence inputs, their trust metadata is advisory, and their contents
cannot grant tools or authorize actions. Concepts are ordered
deterministically and the returned character value records whether
document or character limits truncated the selection.

## Usage

``` r
tempest_okf_context(
  bundle,
  concept_ids = NULL,
  types = NULL,
  include_stale = TRUE,
  today = Sys.Date(),
  max_concepts = 50,
  max_chars = 1e+05
)
```

## Arguments

- bundle:

  A bundle returned by
  [`tempest_read_okf()`](https://jameshwade.github.io/tempest/reference/tempest_read_okf.md).

- concept_ids:

  Optional exact concept IDs.

- types:

  Optional exact OKF type values.

- include_stale:

  Whether to include concepts whose `stale_after` date has passed.

- today:

  Date used to derive staleness.

- max_concepts:

  Maximum concepts to include.

- max_chars:

  Maximum UTF-8 characters in the assembled context.

## Value

A length-one `tempest_okf_context` character value.
