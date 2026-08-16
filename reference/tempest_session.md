# Create a Co-STORM session

Create a Co-STORM session

## Usage

``` r
tempest_session(
  topic,
  config = tempest_config(),
  n_experts = 3,
  experts = NULL,
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

- n_experts:

  Number of expert agents.

- experts:

  Optional list of validated expert profiles. If `NULL`, experts are
  generated automatically.

- retriever:

  Optional `TempestRetriever` or compatible retriever object with a
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  at `$workspace`.

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
