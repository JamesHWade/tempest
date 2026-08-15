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

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

Runtime functions, clients, capabilities, and credentials are
deliberately excluded. Restore requires an explicit runtime.
