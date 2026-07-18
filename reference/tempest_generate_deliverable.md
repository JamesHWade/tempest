# Generate and finalize a Tempest deliverable

**\[experimental\]**

## Usage

``` r
tempest_generate_deliverable(
  deliverable,
  context = list(),
  registry = NULL,
  catalog = NULL,
  runtime = list(),
  provenance = list()
)
```

## Arguments

- deliverable:

  A `tempest_deliverable_spec`.

- context:

  Serializable generation and rendering context.

- registry:

  Runtime operation registry. Defaults to the built-in registry.

- catalog:

  Typed artifact catalog. A new in-memory catalog is created by default.

- runtime:

  Runtime-only clients and callbacks.

- provenance:

  Run, step, expert, evidence, and artifact identifiers. A retry may set
  `replace_invalid_artifacts = TRUE` to replace only a draft or invalid
  artifact from the same run, step, and specification while retaining
  its validation diagnostics in artifact metadata.

## Value

A `tempest_deliverable_result` containing canonical content, validation
results, typed artifacts, the catalog, and resolved operation metadata.

## Details

This is the application-neutral output lifecycle used by built-in and
host-defined workflows. It resolves all operations before generation,
runs validators, renders typed artifacts, invokes exporters only for
`valid` or `approved` output, and publishes the artifacts to a catalog.
Failed validation produces inspectable invalid artifacts rather than
dropping output. Approval-required output is exported by its owning
`TempestRun` after the host approves it.

Generator, validator, renderer, and exporter operations receive named
arguments and may declare only those they use. See
[`tempest_artifact_representation()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_representation.md)
for the renderer return contract. Exporters may return `NULL` or the
same artifact with only `storage_ref`, `updated_at`, and `metadata`
changed. All other finalized artifact fields, including content,
checksum, validation, provenance, and status, are immutable.

## Examples

``` r
registry <- tempest_operation_registry(list(
  generate = list(
    kind = "generator",
    implementation = function(context) context$text
  ),
  render = list(
    kind = "renderer",
    implementation = function(content) content
  )
))
spec <- tempest_deliverable_spec(
  "answer",
  title = "Answer",
  purpose = "Answer the request",
  instructions = "Be concise.",
  generator_id = "generate",
  renderer_ids = "render"
)
result <- tempest_generate_deliverable(
  spec,
  context = list(text = "Done"),
  registry = registry
)
result$artifacts[[1]]@content
#> [1] "Done"
```
