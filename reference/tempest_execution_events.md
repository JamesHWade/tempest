# Query events from a Tempest product execution

**\[experimental\]**

## Usage

``` r
tempest_execution_events(x, after_sequence = 0L)
```

## Arguments

- x:

  A
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md).

- after_sequence:

  Return only events whose sequence is greater than this non-negative
  execution-local cursor.

## Value

An ordered list of normalized event records.

## Details

`tempest_execution_events()` gives host adapters one cursor-based query
for immutable Co-STORM progress records. Generic `TempestRun` histories
are outside the supported product boundary and reject immediately.
