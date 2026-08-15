# Create a Co-STORM session

Create a Co-STORM session

## Usage

``` r
tempest_session(
  topic,
  config = tempest_config(),
  runtime = tempest_runtime(),
  n_experts = 3,
  experts = NULL,
  connection_permissions = list(),
  retriever = NULL,
  progress = NULL,
  session_id = NULL
)
```

## Arguments

- topic:

  Topic string.

- config:

  A `TempestConfig`.

- runtime:

  Frozen Tempest 0.1
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
  adapter. Existing integrations only.

- n_experts:

  Number of expert agents.

- experts:

  Optional list of validated expert profiles. If `NULL`, experts are
  generated automatically.

- connection_permissions:

  Frozen Tempest 0.1 per-role or per-expert connection allow-lists.

- retriever:

  Optional `TempestRetriever` or compatible retriever object with a
  `SourceStore` at `$store`.

- progress:

  Optional function called with `tempest_progress_event` objects as the
  session makes progress.

- session_id:

  Optional stable session identifier. If `NULL`, a new identifier is
  generated.

## Frozen Tempest 0.1 seams

`runtime` and `connection_permissions` remain only for existing Tempest
0.1 integrations and are scheduled for replacement in Tempest 0.2.0. New
host code should keep role-specific tools and authenticated clients
process-local.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("What styles emerged in the 1950s?")
} # }
```
