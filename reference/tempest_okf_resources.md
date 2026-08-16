# Convert Open Knowledge Format concepts to typed Tempest resources

Each selected concept becomes a fingerprinted `tempest_resource` with
`resource_kind = "okf.concept"`. The original Markdown is retained as
evidence content and parsed OKF metadata is namespaced under
`resource@metadata$okf`. This function does not add resources to a
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md);
callers retain an explicit mutation boundary.

## Usage

``` r
tempest_okf_resources(
  bundle,
  concept_ids = NULL,
  types = NULL,
  include_stale = TRUE,
  today = Sys.Date()
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

## Value

A named list of `tempest_resource` objects.
