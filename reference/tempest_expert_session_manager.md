# Create a scoped Tempest expert-session manager

**\[experimental\]**

## Usage

``` r
tempest_expert_session_manager(
  experts,
  runtime,
  config,
  retriever,
  allowed_connection_ref_ids = list(),
  extractor = NULL,
  store = NULL,
  progress = NULL,
  run_id = NULL
)
```

## Arguments

- experts:

  List of
  [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  profiles.

- runtime:

  A
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md).

- config:

  A
  [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md).

- retriever:

  A `TempestRetriever`.

- allowed_connection_ref_ids:

  Named list of allowed connection ids by stable expert id.

- extractor:

  Optional fact-extraction chat.

- store:

  Optional `SourceStore`; defaults to the retriever store.

- progress:

  Optional progress callback.

- run_id:

  Optional shared workflow run id.

## Value

An `ExpertSessionManager`.

## Details

The manager owns a validated live expert roster and creates one
capability-scoped chat per expert. Runtime tools and authenticated
connections are resolved before chat creation and are never inferred
from display names.
