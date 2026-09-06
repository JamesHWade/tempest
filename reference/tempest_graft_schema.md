# Load Tempest's compiled scientific Graft schema

The packaged schema was compiled for Graft consumer contract `0.2.0`.
Runtime loading accepts contracts `>= 0.2.0` and `< 0.7.0`, with store
format `3.1.0`, through
[`graft::graft_contract_version()`](https://jameshwade.github.io/graft/reference/graft_contract_version.html).
Loading never compiles LinkML and rejects any manifest whose immutable
build digest differs.

## Usage

``` r
tempest_graft_schema()
```

## Value

A validated `graft::GraftSchema`.
