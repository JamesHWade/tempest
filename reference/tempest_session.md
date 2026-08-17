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
  session_id = NULL,
  program_set = NULL,
  knowledge_view = NULL
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

- program_set:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  containing the exact dsprrr programs used by Co-STORM. If `NULL`,
  [`tempest_program_set()`](https://jameshwade.github.io/tempest/reference/tempest_program_set.md)
  creates the builtin set.

- knowledge_view:

  Optional immutable Graft view. It is required for a fresh session when
  `program_set` contains governed procedures.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("What styles emerged in the 1950s?")
} # }
```
