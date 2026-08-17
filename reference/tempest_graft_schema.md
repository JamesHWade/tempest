# Load Tempest's compiled scientific Graft schema

The packaged contract is compiled against Graft accessor commit
`81bd3f83a3c8ee2bee22b61ff09b475f58b4f0e5`. Runtime loading never
compiles LinkML and rejects any manifest whose immutable build digest
differs.

## Usage

``` r
tempest_graft_schema()
```

## Value

A validated `graft::GraftSchema`.
