# Tempest governed-procedure reference

An immutable reference to one governed procedure revision and the pinned
Graft view in which it was accepted. The reference also binds the exact
dsprrr program artifact and Tempest evaluator contract.

## Usage

``` r
TempestGovernedProcedureRef(
  stage = character(0),
  tempest_governed_procedure_id = character(0),
  record_id = character(0),
  revision_id = character(0),
  program_artifact_id = character(0),
  contract_version = integer(0),
  evaluator_id = character(0),
  evaluator_version = character(0),
  store_id = character(0),
  snapshot_id = character(0),
  schema_build_digest = character(0),
  commit_order = numeric(0)
)
```
