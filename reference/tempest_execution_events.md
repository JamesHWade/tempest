# Query events from a Tempest execution

**\[experimental\]**

## Usage

``` r
tempest_execution_events(x, after_sequence = 0L)
```

## Arguments

- x:

  A `TempestRun` or
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).

- after_sequence:

  Return only events whose sequence is greater than this non-negative
  execution-local cursor.

## Value

An ordered list of normalized event records.

## Details

`tempest_execution_events()` gives host adapters one cursor-based event
query for generic `TempestRun` workflows and interactive
[TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
sessions. It returns immutable list records rather than requiring
callers to reach into mutable R6 fields.

Every record contains `event_id`, a positive execution-local `sequence`,
`run_id`, `event_type`, `status`, `timestamp`, and a serializable
`payload`. Generic-run records also contain `workflow_id` and step,
attempt, expert, artifact, and approval context. Co-STORM records
contain `workflow`, `stage`, `step`, parent, and correlation context.
