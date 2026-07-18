# Create a Tempest runtime connection provider

**\[experimental\]**

## Usage

``` r
tempest_connection_provider(connections = list(), bindings = list())
```

## Arguments

- connections:

  Optional list of serializable connection references.

- bindings:

  Named runtime factories or pre-built clients. A factory is called with
  `connection_ref` and `context`. Binding names must match the stable
  connection identifiers in `connections`.

## Value

A mutable provider with `register()`, `has()`,
[`list()`](https://rdrr.io/r/base/list.html), `preflight()`, and
`resolve()` methods. Listings never expose bindings.

## Details

A connection provider keeps authenticated clients and factories outside
durable
[`tempest_connection_ref()`](https://jameshwade.github.io/tempest/reference/tempest_connection_ref.md)
records. Resolution requires an explicit allow-list, so a workflow
cannot acquire connections that were not granted for its current
execution context.

## Examples

``` r
reference <- tempest_connection_ref(
  "documents",
  provider_id = "host",
  connection_type = "search",
  title = "Documents",
  description = "Approved document index"
)
provider <- tempest_connection_provider(
  list(reference),
  bindings = list(documents = list(endpoint = "local"))
)
provider$resolve("documents", allowed_ref_ids = "documents")
#> $documents
#> $documents$endpoint
#> [1] "local"
#> 
#> 
```
