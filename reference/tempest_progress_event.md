# Create a Tempest progress event

`tempest_progress_event()` creates a small host-neutral event record for
STORM and Co-STORM workflow progress. Events are designed for package
code, host apps, tests, and future telemetry adapters that need
lifecycle, stage, step, expert/tool, cancellation, and artifact status
without importing Shiny.

## Usage

``` r
tempest_progress_event(
  run_id,
  workflow,
  event_type,
  status,
  stage = NA_character_,
  step = NA_character_,
  message = NA_character_,
  payload = list(),
  parent_event_id = NA_character_,
  correlation_id = NA_character_,
  event_id = NULL,
  timestamp = NULL
)
```

## Arguments

- run_id:

  Stable run or session identifier.

- workflow:

  Workflow kind, one of `"storm"` or `"costorm"`.

- event_type:

  Event family, one of `"workflow"`, `"stage"`, `"step"`, `"expert"`,
  `"tool"`, `"artifact"`, or `"cancellation"`.

- status:

  Event status, one of `"pending"`, `"started"`, `"running"`,
  `"succeeded"`, `"failed"`, `"cancelled"`, `"skipped"`, or
  `"available"`.

- stage:

  Optional workflow stage name.

- step:

  Optional stage-local step name.

- message:

  Optional short human-readable progress message.

- payload:

  Optional list of structured progress metadata.

- parent_event_id:

  Optional parent event identifier.

- correlation_id:

  Optional identifier shared by related events.

- event_id:

  Optional stable event identifier. Defaults to a generated id.

- timestamp:

  Optional event timestamp. Defaults to the current UTC time.

## Value

A `tempest_progress_event` S7 object.

## Details

Core fields are validated at construction. Payloads should stay small
and privacy-aware: use them for progress metadata and references, not
full raw expert answers or source bodies by default.

## Examples

``` r
event <- tempest_progress_event(
  run_id = "run-1",
  workflow = "storm",
  event_type = "stage",
  status = "started",
  stage = "research"
)
event@status
#> [1] "started"
```
