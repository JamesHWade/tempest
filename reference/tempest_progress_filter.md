# Filter Tempest progress events

Filter Tempest progress events

## Usage

``` r
tempest_progress_filter(
  events,
  run_id = NULL,
  workflow = NULL,
  event_type = NULL,
  stage = NULL,
  status = NULL,
  correlation_id = NULL
)
```

## Arguments

- events:

  A list of `tempest_progress_event` objects, or a
  `tempest_progress_collector`.

- run_id, workflow, event_type, stage, status, correlation_id:

  Optional scalar filters. `NULL` leaves that field unfiltered.

## Value

A list of matching `tempest_progress_event` objects.

## Examples

``` r
events <- list(tempest_progress_event(
  run_id = "run-1",
  workflow = "storm",
  event_type = "stage",
  status = "started",
  stage = "research"
))
tempest_progress_filter(events, stage = "research")
#> [[1]]
#> <tempest::tempest_progress_event>
#>  @ event_id       : chr "P_3b9de10c589a1553"
#>  @ run_id         : chr "run-1"
#>  @ workflow       : chr "storm"
#>  @ event_type     : chr "stage"
#>  @ stage          : chr "research"
#>  @ step           : chr NA
#>  @ status         : chr "started"
#>  @ timestamp      : chr "2026-06-28 18:28:06 UTC"
#>  @ message        : chr NA
#>  @ payload        : list()
#>  @ parent_event_id: chr NA
#>  @ correlation_id : chr NA
#> 
```
