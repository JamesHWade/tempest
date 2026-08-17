# Reference an accepted governed procedure

Resolves one accepted `GovernedProcedure` and its exact dsprrr
`ProgramArtifact` through an immutable Graft view. The returned value
binds the current accepted revision and every snapshot authority
dimension.

## Usage

``` r
tempest_governed_procedure_ref(knowledge_view, record_id)
```

## Arguments

- knowledge_view:

  A pinned `GraftView` returned by
  [`graft::graft_at()`](https://jameshwade.github.io/graft/reference/graft_at.html).

- record_id:

  Graft record identifier for the governed procedure.

## Value

A `TempestGovernedProcedureRef` S7 object.

## Details

A reference can only be created from records proven at the supplied
pinned boundary. Tempest repeats the same verification immediately
before provider execution so a serialized reference never becomes
authority by itself.
