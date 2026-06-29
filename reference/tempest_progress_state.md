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

## Examples

``` r
events <- list(
  tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "workflow",
    status = "started"
  ),
  tempest_progress_event(
    run_id = "run-1",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research"
  )
)
tempest_progress_state(events)
#> $run_id
#> [1] "run-1"
#> 
#> $workflow
#> [1] "storm"
#> 
#> $status
#> [1] "running"
#> 
#> $terminal
#> [1] FALSE
#> 
#> $current_stage
#> [1] "research"
#> 
#> $current_step
#> [1] NA
#> 
#> $completed_stages
#> character(0)
#> 
#> $skipped_stages
#> character(0)
#> 
#> $active
#> $active$stages
#> $active$stages$P_cedff75333de8573
#> $active$stages$P_cedff75333de8573$event_id
#> [1] "P_cedff75333de8573"
#> 
#> $active$stages$P_cedff75333de8573$event_type
#> [1] "stage"
#> 
#> $active$stages$P_cedff75333de8573$stage
#> [1] "research"
#> 
#> $active$stages$P_cedff75333de8573$step
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$status
#> [1] "started"
#> 
#> $active$stages$P_cedff75333de8573$message
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$started_at
#> [1] "2026-06-29 10:44:22 UTC"
#> 
#> $active$stages$P_cedff75333de8573$updated_at
#> [1] "2026-06-29 10:44:22 UTC"
#> 
#> $active$stages$P_cedff75333de8573$parent_event_id
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$correlation_id
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$expert_id
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$expert_name
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$session_id
#> [1] NA
#> 
#> $active$stages$P_cedff75333de8573$question_index
#> [1] NA
#> 
#> 
#> 
#> $active$steps
#> list()
#> 
#> $active$experts
#> list()
#> 
#> $active$tools
#> list()
#> 
#> 
#> $failures
#> list()
#> 
#> $cancellation
#> NULL
#> 
#> $artifacts
#> list()
#> 
#> $event_count
#> [1] 2
#> 
#> $latest_event_id
#> [1] "P_cedff75333de8573"
#> 
#> $updated_at
#> [1] "2026-06-29 10:44:22 UTC"
#> 
```
