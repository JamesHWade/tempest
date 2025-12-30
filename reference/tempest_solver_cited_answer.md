# A vitals solver that answers questions using retrieval + citations

This solver is designed to evaluate the end-to-end behavior of the
tempest retrieval + citation discipline. It is intentionally lighter
than a full [`tempest_run()`](tempest_run.md) call.

## Usage

``` r
tempest_solver_cited_answer(
  input,
  config = tempest_config(),
  solver_chat = NULL
)
```

## Arguments

- input:

  Character vector of questions.

- config:

  A `TempestConfig`.

- solver_chat:

  Optional (ignored). Present to match vitals conventions.

## Value

A list with `result` and `solver_chat` (and `solver_metadata`).
