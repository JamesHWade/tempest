# Snapshot generic Tempest run state

**\[experimental\]**

## Usage

``` r
tempest_run_snapshot(run)
```

## Arguments

- run:

  A `TempestRun`.

## Value

An in-memory serializable run record.

## Details

Runtime functions, clients, capabilities, and credentials are
deliberately excluded. Restore requires an explicit runtime.
