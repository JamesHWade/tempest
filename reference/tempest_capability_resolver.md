# Create a Tempest capability resolver

**\[experimental\]**

## Usage

``` r
tempest_capability_resolver(
  specifications = list(),
  implementations = list(),
  connection_provider = NULL
)
```

## Arguments

- specifications:

  Optional list of
  [`tempest_capability_spec()`](https://jameshwade.github.io/tempest/reference/tempest_capability_spec.md)
  objects.

- implementations:

  Named runtime factories or descriptor lists with `factory` and
  optional `authorize` functions. A factory receives `capability_spec`,
  named `connections`, and runtime `context`, and returns a list
  containing `tools`, `registrars`, and optional serializable
  `metadata`.

- connection_provider:

  Optional
  [`tempest_connection_provider()`](https://jameshwade.github.io/tempest/reference/tempest_connection_provider.md)
  used to resolve explicitly allowed opaque connection references.

## Value

A mutable resolver with registration, inspection, and `resolve()`
methods. Resolution returns runtime tools and registrars plus
serializable grant records.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

The resolver keeps serializable capability specifications separate from
runtime factories. It preflights every requested capability before
invoking factories, treats required failures as errors, and records
optional failures as serializable denied grants.

## Examples

``` r
specification <- tempest_capability_spec(
  "documents.search",
  purpose = "Search approved documents",
  instructions = "Search only the granted index.",
  operation_id = "capability.documents.search"
)
resolver <- tempest_capability_resolver(
  list(specification),
  implementations = list(
    "documents.search" = function(capability_spec, connections, context) {
      list(tools = list(), registrars = list())
    }
  )
)
resolver$resolve(required_capability_ids = "documents.search")
#> $tools
#> list()
#> 
#> $registrars
#> list()
#> 
#> $grants
#> $grants$documents.search
#> $grants$documents.search$capability_id
#> [1] "documents.search"
#> 
#> $grants$documents.search$capability_version
#> [1] "1"
#> 
#> $grants$documents.search$operation_id
#> [1] "capability.documents.search"
#> 
#> $grants$documents.search$operation_version
#> [1] "1"
#> 
#> $grants$documents.search$required
#> [1] TRUE
#> 
#> $grants$documents.search$status
#> [1] "granted"
#> 
#> $grants$documents.search$connection_ref_ids
#> character(0)
#> 
#> $grants$documents.search$reason_code
#> NULL
#> 
#> $grants$documents.search$reason
#> NULL
#> 
#> $grants$documents.search$metadata
#> list()
#> 
#> 
#> 
#> attr(,"class")
#> [1] "tempest_capability_resolution" "list"                         
```
