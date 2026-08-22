# Warm up a Co-STORM session asynchronously

**\[experimental\]**

## Usage

``` r
tempest_session_warmup_async(
  session,
  timeout_s = getOption("tempest.costorm.warmup_timeout_s", 120),
  max_parallel_experts = getOption("tempest.costorm.warmup_max_parallel_experts", 3L),
  is_current = function() TRUE
)
```

## Arguments

- session:

  A
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
  created by
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

- timeout_s:

  Maximum seconds allowed for each expert orientation. Use `NULL`, a
  non-positive value, or a non-finite value to disable timeouts.

- max_parallel_experts:

  Maximum number of expert requests started at the same time.

- is_current:

  A zero-argument function that returns `TRUE` while the originating
  host session is current. Stale operations resolve without committing
  late results.

## Value

A promise that resolves to an internal `tempest_warmup_result` S7 object
with aggregate counts and per-expert audit records.

## Details

Runs one bounded, evidence-seeking orientation for each active expert
without blocking the caller. Expert requests run in bounded parallel
batches, while transcript and evidence commits occur deterministically
in expert order. A single mind-map update follows the completed evidence
commits.
