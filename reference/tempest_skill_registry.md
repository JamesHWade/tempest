# Create a Tempest skill registry

**\[experimental\]**

## Usage

``` r
tempest_skill_registry(
  skills = list(),
  operations = tempest_operation_registry()
)
```

## Arguments

- skills:

  Optional list of
  [`tempest_skill()`](https://jameshwade.github.io/tempest/reference/tempest_skill.md)
  specifications.

- operations:

  A
  [`tempest_operation_registry()`](https://jameshwade.github.io/tempest/reference/tempest_operation_registry.md)
  containing runtime skill operations.

## Value

A mutable registry with `register()`,
[`get()`](https://rdrr.io/r/base/get.html), `has()`,
[`list()`](https://rdrr.io/r/base/list.html), `resolve()`, and
`resolve_for_expert()` methods. Resolution combines skill instructions
with the required and optional capability sets supplied by the caller or
declared by the expert. Caller-supplied sets must be disjoint. When a
selected skill requires an otherwise optional capability, the
requirement is promoted and the returned sets remain disjoint.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

The registry binds serializable skill specifications to versioned
runtime operations. Every operation declared by a skill must be
available from `operations` with kind `"skill"`.

## Examples

``` r
operations <- tempest_operation_registry()
operations$register(
  "skill.compare",
  function(left, right) identical(left, right),
  kind = "skill"
)
skills <- tempest_skill_registry(
  list(tempest_skill(
    "compare",
    purpose = "Compare two values",
    instructions = "Compare the supplied values.",
    operation_ids = "skill.compare"
  )),
  operations = operations
)
skills$resolve("compare")$prompt
#> [1] "Compare the supplied values."
```
