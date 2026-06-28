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
#> [1] "P_2acf8b01b7f45ef3"
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
#> [1] "2026-06-28 12:56:42 UTC"
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
