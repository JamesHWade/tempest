# Create an in-memory Tempest progress event collector

**\[experimental\]**

## Usage

``` r
tempest_progress_collector(include_payload = FALSE)
```

## Arguments

- include_payload:

  If `TRUE`, retain event payloads. The default `FALSE` stores event
  metadata only.

## Value

A `tempest_progress_collector` list with `record()`, `events()`,
[`data()`](https://rdrr.io/r/utils/data.html), `replay()`, and `clear()`
methods.

## Details

`tempest_progress_collector()` returns a small host-neutral sink for
Tempest progress events. Use `collector$record` as a `progress`
callback, then read events back with filtering or replay them to another
callback.

By default, the collector strips event payloads before storing them. Set
`include_payload = TRUE` when tests or trusted host adapters need to
assert or replay structured payload metadata.

## Examples

``` r
collector <- tempest_progress_collector()
collector$record(tempest_progress_event(
  run_id = "run-1",
  workflow = "storm",
  event_type = "stage",
  status = "started",
  stage = "research"
))
collector$data(stage = "research")
#> [[1]]
#> [[1]]$event_id
#> [1] "P_156ccf6bc314813e"
#> 
#> [[1]]$run_id
#> [1] "run-1"
#> 
#> [[1]]$workflow
#> [1] "storm"
#> 
#> [[1]]$event_type
#> [1] "stage"
#> 
#> [[1]]$stage
#> [1] "research"
#> 
#> [[1]]$step
#> [1] NA
#> 
#> [[1]]$status
#> [1] "started"
#> 
#> [[1]]$timestamp
#> [1] "2026-06-29 11:21:55 UTC"
#> 
#> [[1]]$message
#> [1] NA
#> 
#> [[1]]$payload
#> list()
#> 
#> [[1]]$parent_event_id
#> [1] NA
#> 
#> [[1]]$correlation_id
#> [1] NA
#> 
#> 
```
