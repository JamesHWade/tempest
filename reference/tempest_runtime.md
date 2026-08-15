# Create a Tempest runtime

**\[experimental\]**

## Usage

``` r
tempest_runtime(
  operations = tempest_operation_registry(),
  skill_specs = list(),
  capability_specs = list(),
  capability_implementations = list(),
  connection_refs = list(),
  connection_bindings = list(),
  include_builtins = TRUE
)
```

## Arguments

- operations:

  A
  [`tempest_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_operation_registry.md).

- skill_specs:

  List of
  [`tempest_skill()`](https://jameshwade.github.io/tempest/reference/tempest_skill.md)
  specifications.

- capability_specs:

  List of
  [`tempest_capability_spec()`](https://jameshwade.github.io/tempest/reference/tempest_capability_spec.md)
  specifications.

- capability_implementations:

  Named runtime capability factories.

- connection_refs:

  List of
  [`tempest_connection_ref()`](https://jameshwade.github.io/tempest/reference/tempest_connection_ref.md)
  specifications.

- connection_bindings:

  Named runtime connection factories or clients.

- include_builtins:

  Whether to include Tempest's narrow web, evidence, semantic-retrieval,
  and expert-delegation capabilities.

## Value

A mutable `TempestRuntime` with role and expert resolution methods.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

A runtime binds durable workflow definitions to process-local
operations, skills, capabilities, and authenticated connections. Runtime
factories and clients are deliberately excluded from snapshots.

## Examples

``` r
runtime <- tempest_runtime()
runtime$capabilities$list()
#> $tempest.evidence.read
#> $tempest.evidence.read$specification
#> $tempest.evidence.read$specification$capability_id
#> [1] "tempest.evidence.read"
#> 
#> $tempest.evidence.read$specification$version
#> [1] "1"
#> 
#> $tempest.evidence.read$specification$title
#> [1] "tempest.evidence.read"
#> 
#> $tempest.evidence.read$specification$purpose
#> [1] "Read sources, claims, and evidence in the run ledger."
#> 
#> $tempest.evidence.read$specification$instructions
#> [1] "Use only evidence already available to this run."
#> 
#> $tempest.evidence.read$specification$operation_id
#> [1] "tempest.capability.evidence.read"
#> 
#> $tempest.evidence.read$specification$operation_version
#> [1] "1"
#> 
#> $tempest.evidence.read$specification$connection_ref_ids
#> character(0)
#> 
#> $tempest.evidence.read$specification$model_roles
#> [1] "expert"      "coordinator" "writer"      "mindmap"     "judge"      
#> 
#> $tempest.evidence.read$specification$input_schema
#> list()
#> 
#> $tempest.evidence.read$specification$output_schema
#> list()
#> 
#> $tempest.evidence.read$specification$side_effecting
#> [1] FALSE
#> 
#> $tempest.evidence.read$specification$state
#> [1] "active"
#> 
#> $tempest.evidence.read$specification$metadata
#> list()
#> 
#> $tempest.evidence.read$specification$schema_version
#> [1] 1
#> 
#> $tempest.evidence.read$specification$fingerprint
#> [1] "302d619c4ce607e9dc8062c93c010b49162fbaecdbb7a7851827a12840623c5f"
#> 
#> 
#> $tempest.evidence.read$implementation_registered
#> [1] TRUE
#> 
#> 
#> $tempest.evidence.write
#> $tempest.evidence.write$specification
#> $tempest.evidence.write$specification$capability_id
#> [1] "tempest.evidence.write"
#> 
#> $tempest.evidence.write$specification$version
#> [1] "1"
#> 
#> $tempest.evidence.write$specification$title
#> [1] "tempest.evidence.write"
#> 
#> $tempest.evidence.write$specification$purpose
#> [1] "Record source-backed claims in the run ledger."
#> 
#> $tempest.evidence.write$specification$instructions
#> [1] "Record only atomic claims backed by inspected sources."
#> 
#> $tempest.evidence.write$specification$operation_id
#> [1] "tempest.capability.evidence.write"
#> 
#> $tempest.evidence.write$specification$operation_version
#> [1] "1"
#> 
#> $tempest.evidence.write$specification$connection_ref_ids
#> character(0)
#> 
#> $tempest.evidence.write$specification$model_roles
#> [1] "expert"
#> 
#> $tempest.evidence.write$specification$input_schema
#> list()
#> 
#> $tempest.evidence.write$specification$output_schema
#> list()
#> 
#> $tempest.evidence.write$specification$side_effecting
#> [1] FALSE
#> 
#> $tempest.evidence.write$specification$state
#> [1] "active"
#> 
#> $tempest.evidence.write$specification$metadata
#> list()
#> 
#> $tempest.evidence.write$specification$schema_version
#> [1] 1
#> 
#> $tempest.evidence.write$specification$fingerprint
#> [1] "b5f7fccca6051d720b3a708b4b227658cba360307f80185640bb6e188f4769cf"
#> 
#> 
#> $tempest.evidence.write$implementation_registered
#> [1] TRUE
#> 
#> 
#> $tempest.expert.delegate
#> $tempest.expert.delegate$specification
#> $tempest.expert.delegate$specification$capability_id
#> [1] "tempest.expert.delegate"
#> 
#> $tempest.expert.delegate$specification$version
#> [1] "1"
#> 
#> $tempest.expert.delegate$specification$title
#> [1] "tempest.expert.delegate"
#> 
#> $tempest.expert.delegate$specification$purpose
#> [1] "Delegate work to one active expert by stable expert id."
#> 
#> $tempest.expert.delegate$specification$instructions
#> [1] "Delegate only to active experts selected for this run."
#> 
#> $tempest.expert.delegate$specification$operation_id
#> [1] "tempest.capability.expert.delegate"
#> 
#> $tempest.expert.delegate$specification$operation_version
#> [1] "1"
#> 
#> $tempest.expert.delegate$specification$connection_ref_ids
#> character(0)
#> 
#> $tempest.expert.delegate$specification$model_roles
#> [1] "coordinator"
#> 
#> $tempest.expert.delegate$specification$input_schema
#> list()
#> 
#> $tempest.expert.delegate$specification$output_schema
#> list()
#> 
#> $tempest.expert.delegate$specification$side_effecting
#> [1] FALSE
#> 
#> $tempest.expert.delegate$specification$state
#> [1] "active"
#> 
#> $tempest.expert.delegate$specification$metadata
#> list()
#> 
#> $tempest.expert.delegate$specification$schema_version
#> [1] 1
#> 
#> $tempest.expert.delegate$specification$fingerprint
#> [1] "e718e1bd8f9af47e38211722b5c6d7fb590c54efb8ee1af2d007030b2f88fb15"
#> 
#> 
#> $tempest.expert.delegate$implementation_registered
#> [1] TRUE
#> 
#> 
#> $tempest.research.web
#> $tempest.research.web$specification
#> $tempest.research.web$specification$capability_id
#> [1] "tempest.research.web"
#> 
#> $tempest.research.web$specification$version
#> [1] "1"
#> 
#> $tempest.research.web$specification$title
#> [1] "tempest.research.web"
#> 
#> $tempest.research.web$specification$purpose
#> [1] "Discover and inspect public web evidence."
#> 
#> $tempest.research.web$specification$instructions
#> [1] "Search only when the execution context grants web research. Inspect sources before relying on them."
#> 
#> $tempest.research.web$specification$operation_id
#> [1] "tempest.capability.research.web"
#> 
#> $tempest.research.web$specification$operation_version
#> [1] "1"
#> 
#> $tempest.research.web$specification$connection_ref_ids
#> character(0)
#> 
#> $tempest.research.web$specification$model_roles
#> [1] "expert"      "coordinator"
#> 
#> $tempest.research.web$specification$input_schema
#> list()
#> 
#> $tempest.research.web$specification$output_schema
#> list()
#> 
#> $tempest.research.web$specification$side_effecting
#> [1] FALSE
#> 
#> $tempest.research.web$specification$state
#> [1] "active"
#> 
#> $tempest.research.web$specification$metadata
#> list()
#> 
#> $tempest.research.web$specification$schema_version
#> [1] 1
#> 
#> $tempest.research.web$specification$fingerprint
#> [1] "33f4c45e80eea7583f49fcc855923cd6a8deb163b73cdc080b8ade4f154dbf5e"
#> 
#> 
#> $tempest.research.web$implementation_registered
#> [1] TRUE
#> 
#> 
#> $tempest.retrieval.semantic
#> $tempest.retrieval.semantic$specification
#> $tempest.retrieval.semantic$specification$capability_id
#> [1] "tempest.retrieval.semantic"
#> 
#> $tempest.retrieval.semantic$specification$version
#> [1] "1"
#> 
#> $tempest.retrieval.semantic$specification$title
#> [1] "tempest.retrieval.semantic"
#> 
#> $tempest.retrieval.semantic$specification$purpose
#> [1] "Retrieve approved evidence from the semantic store."
#> 
#> $tempest.retrieval.semantic$specification$instructions
#> [1] "Retrieve only from the run-scoped semantic store."
#> 
#> $tempest.retrieval.semantic$specification$operation_id
#> [1] "tempest.capability.retrieval.semantic"
#> 
#> $tempest.retrieval.semantic$specification$operation_version
#> [1] "1"
#> 
#> $tempest.retrieval.semantic$specification$connection_ref_ids
#> character(0)
#> 
#> $tempest.retrieval.semantic$specification$model_roles
#> [1] "expert"  "writer"  "mindmap"
#> 
#> $tempest.retrieval.semantic$specification$input_schema
#> list()
#> 
#> $tempest.retrieval.semantic$specification$output_schema
#> list()
#> 
#> $tempest.retrieval.semantic$specification$side_effecting
#> [1] FALSE
#> 
#> $tempest.retrieval.semantic$specification$state
#> [1] "active"
#> 
#> $tempest.retrieval.semantic$specification$metadata
#> list()
#> 
#> $tempest.retrieval.semantic$specification$schema_version
#> [1] 1
#> 
#> $tempest.retrieval.semantic$specification$fingerprint
#> [1] "7fb0894e24e2eaf84a3bfaf49af907036508ad19130bbb7e974e6e4a24ac6e0b"
#> 
#> 
#> $tempest.retrieval.semantic$implementation_registered
#> [1] TRUE
#> 
#> 
```
