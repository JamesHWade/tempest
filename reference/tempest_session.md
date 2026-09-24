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
  knowledge = NULL
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
  at `$workspace` and `search(query, k)` and `fetch(url)` methods.
  [`search()`](https://rdrr.io/r/base/search.html) returns a data frame
  with `title` and HTTP/HTTPS `url` columns; `snippet` is optional and
  Tempest derives or verifies `source_id`. Create the workspace with
  [`tempest_research_workspace()`](https://jameshwade.github.io/tempest/reference/tempest_research_workspace.md)
  and source IDs with
  [`tempest_source_id()`](https://jameshwade.github.io/tempest/reference/tempest_source_id.md).

- progress:

  Optional function called with `tempest_progress_event` objects as the
  session makes progress.

- session_id:

  Optional stable session identifier. If `NULL`, a new identifier is
  generated.

- knowledge:

  Optional accepted organizational knowledge from
  [`tempest_artifact_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_knowledge.md).
  It supplies accepted evidence records without selecting or authorizing
  executable programs.

## Value

A `TempestSession` R6 object for the active Co-STORM research session.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("What styles emerged in the 1950s?")
} # }
```
