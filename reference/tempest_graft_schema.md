# Load Tempest's compiled scientific Graft schema

The packaged contract is compiled for Graft consumer contract `0.2.0`,
which runtime loading checks through
[`graft::graft_contract_version()`](https://jameshwade.github.io/graft/reference/graft_contract_version.html).
Loading never compiles LinkML and rejects any manifest whose immutable
build digest differs.

## Usage

``` r
tempest_graft_schema()
```

## Value

A validated `graft::GraftSchema`.
