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
  extract_claims_program = NULL,
  workspace = NULL,
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

- extract_claims_program:

  ProgramSet-bound claim-extraction execution. Required when `extractor`
  is supplied.

- workspace:

  Optional
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md);
  defaults to the retriever workspace.

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
