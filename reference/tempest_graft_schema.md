# Load Tempest's compiled scientific Graft schema

The packaged schema was compiled for Graft consumer contract `0.2.0`.
Runtime loading accepts contracts `>= 0.2.0` and `< 2.0.0`, with store
format `3.1.0`, through
[`graft::graft_contract_version()`](https://jameshwade.github.io/graft/reference/graft_contract_version.html).
The range follows Graft's additive-minor contract policy within the
tested 0.x and 1.x major lines. The 1.0 data-dict manifest cutoff does
not affect Tempest's pinned LinkML schema. Future minors are not
individually certified by this range. Required exports and the compiled
schema are checked independently. Loading never compiles LinkML and
rejects any manifest whose immutable build digest differs.

## Usage

``` r
tempest_graft_schema()
```

## Value

A validated `graft::GraftSchema`.
