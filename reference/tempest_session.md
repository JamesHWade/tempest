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

  A
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
  containing process-local adapters.

- n_experts:

  Number of expert agents.

- experts:

  Optional list of validated expert profiles. If `NULL`, experts are
  generated automatically.

- connection_permissions:

  Named per-role or per-expert connection allow-lists.

- retriever:

  Optional `TempestRetriever` or compatible retriever object with a
  `SourceStore` at `$store`.

- progress:

  Optional function called with `tempest_progress_event` objects as the
  session makes progress.

- session_id:

  Optional stable session identifier. If `NULL`, a new identifier is
  generated.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("What styles emerged in the 1950s?")
} # }
```
