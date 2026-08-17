# TempestProgramSet (S7)

A validated live collection of the exact dsprrr programs used by
Tempest. Durable projections contain only entry metadata; registry-bound
modules and the local bundle root remain live process state.

## Usage

``` r
TempestProgramSet(
  schema_version = 2L,
  bundle_root = character(0),
  entries = list(),
  programs = list()
)
```
