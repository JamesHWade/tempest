# Replay Tempest progress events to a callback

**\[experimental\]**

## Usage

``` r
tempest_progress_replay(events, progress)
```

## Arguments

- events:

  A list of `tempest_progress_event` objects, or a
  `tempest_progress_collector`.

- progress:

  Function called once for each event.

## Value

The input events, invisibly.

## Examples

``` r
events <- list(tempest_progress_event(
  run_id = "run-1",
  workflow = "storm",
  event_type = "workflow",
  status = "started"
))
tempest_progress_replay(events, function(event) event@status)
```
