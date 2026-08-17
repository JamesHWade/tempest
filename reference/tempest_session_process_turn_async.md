# Process a completed Co-STORM turn asynchronously

**\[experimental\]**

## Usage

``` r
tempest_session_process_turn_async(
  session,
  user_text,
  assistant_text,
  deputy_execution,
  provider_turn = NULL,
  suggest = TRUE,
  n_suggestions = 4L,
  turn_id = NULL,
  is_current = function() TRUE
)
```

## Arguments

- session:

  A
  [TempestSession](https://jameshwade.github.io/tempest/reference/TempestSession.md)
  object.

- user_text:

  Completed user input as a single string.

- assistant_text:

  Completed moderator response as a single string.

- deputy_execution:

  Exact completed moderator Deputy trace captured with the response
  before any asynchronous queueing.

- provider_turn:

  Optional process-local provider turn used to harvest native sources.
  It is never retained in the result.

- suggest:

  Whether to generate follow-up questions.

- n_suggestions:

  Maximum number of follow-up questions.

- turn_id:

  Optional stable correlation identifier. When supplied, it must equal
  the completed moderator Deputy trace correlation. When `NULL`, that
  trace correlation is authoritative.

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
