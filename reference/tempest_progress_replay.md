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

  Function called once for each event with the canonical plain progress
  record.

## Value

The input events, invisibly.
