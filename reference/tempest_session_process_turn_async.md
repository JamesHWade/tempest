# Process a completed Co-STORM turn asynchronously

**\[experimental\]**

## Usage

``` r
tempest_session_process_turn_async(
  session,
  completion_id,
  suggest = TRUE,
  n_suggestions = 4L,
  is_current = function() TRUE
)
```

## Arguments

- session:

  A
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
  object.

- completion_id:

  Opaque, process-local completion identifier returned by
  `session$request_completion_async()`.

- suggest:

  Whether to generate follow-up questions.

- n_suggestions:

  Maximum number of follow-up questions.

- is_current:

  Process-local predicate returning `TRUE` while this work is allowed to
  commit. It is never retained in the result.

## Value

A promise resolving to a typed, serializable
`tempest_session_turn_result` object.

## Details

Records a completed user and moderator exchange, then asynchronously
commits cited evidence, updates the session mind map, and optionally
generates follow-up questions. Enrichment failures are returned as typed
notices so host applications can choose their own presentation. Stale
work is cancelled before it can commit later pipeline stages.
