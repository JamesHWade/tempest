# Convert a Tempest progress event to a list

Convert a Tempest progress event to a list

## Usage

``` r
tempest_progress_event_data(event)
```

## Arguments

- event:

  A `tempest_progress_event` object.

## Value

A named list with the event contract fields.

## Examples

``` r
event <- tempest_progress_event(
  run_id = "session-1",
  workflow = "costorm",
  event_type = "expert",
  status = "started",
  stage = "warmup"
)
tempest_progress_event_data(event)
#> $event_id
#> [1] "P_706372b3d544c8cf"
#> 
#> $run_id
#> [1] "session-1"
#> 
#> $workflow
#> [1] "costorm"
#> 
#> $event_type
#> [1] "expert"
#> 
#> $stage
#> [1] "warmup"
#> 
#> $step
#> [1] NA
#> 
#> $status
#> [1] "started"
#> 
#> $timestamp
#> [1] "2026-06-28 20:46:00 UTC"
#> 
#> $message
#> [1] NA
#> 
#> $payload
#> list()
#> 
#> $parent_event_id
#> [1] NA
#> 
#> $correlation_id
#> [1] NA
#> 
```
