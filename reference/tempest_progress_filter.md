# Filter Tempest progress events

**\[experimental\]**

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
