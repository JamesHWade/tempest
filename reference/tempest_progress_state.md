# Reduce Tempest progress events to workflow state

**\[experimental\]**

## Usage

``` r
tempest_progress_state(events)
```

## Arguments

- events:

  A list of `tempest_progress_event` objects, a
  `tempest_progress_collector`, or a list of event data lists from
  [`tempest_progress_event_data()`](https://jameshwade.github.io/tempest/reference/tempest_progress_event_data.md).

## Value

A named list with run/workflow ids, current active work, completed
stages, failures, cancellation details, artifact references, and
terminal status.

## Details

`tempest_progress_state()` consumes recorded Tempest progress events and
returns a compact, serializable state object that host apps can poll or
render. The reducer is host-neutral: it does not import Shiny and it
works with raw event lists, collector objects, or stored event data
lists.
