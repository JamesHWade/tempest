# Attach and start the generic workflow for a Co-STORM session

**\[experimental\]**

## Usage

``` r
tempest_costorm_workflow_run(
  session,
  style = c("technical", "executive"),
  include_references = TRUE,
  reorganize = TRUE,
  verbose = TRUE,
  run_id = session$session_id,
  progress = NULL
)
```

## Arguments

- session:

  A `TempestSession`.

- style:

  Report style.

- include_references:

  Whether the report includes references.

- reorganize:

  Whether to reorganize the mind map before reporting.

- verbose:

  Whether warmup prints progress.

- run_id:

  Optional run id. Defaults to the session id.

- progress:

  Optional generic run event callback.

## Value

The session-owned `TempestRun` in `awaiting_approval`.

## Details

The run executes warmup and then waits at the dialogue approval
checkpoint. Conduct session turns normally, then approve the pending
checkpoint with
[`tempest_run_record_approval()`](https://jameshwade.github.io/tempest/reference/tempest_run_accessors.md)
to snapshot the dialogue and generate the report.
