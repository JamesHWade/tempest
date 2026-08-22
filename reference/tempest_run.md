# Run the STORM pipeline

This is a scripted workflow that:

1.  discovers perspectives and research questions,

2.  runs a multi-perspective research loop (search/fetch + expert
    synthesis),

3.  creates an outline,

4.  writes a cited report in Markdown.

## Usage

``` r
tempest_run(
  topic,
  config = tempest_config(),
  retriever = NULL,
  knowledge = NULL,
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_writing = FALSE,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  progress = NULL,
  verbose = TRUE
)
```

## Arguments

- topic:

  Research topic or question.

- config:

  A `TempestConfig`.

- retriever:

  Optional `TempestRetriever`. If `NULL`, created from `config`.

- knowledge:

  Optional accepted organizational knowledge from
  [`tempest_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_knowledge.md).
  It pins an immutable Graft view, supplies accepted evidence records,
  and carries any accepted governed-procedure stage bindings. It is
  never persisted.

- n_experts:

  Number of expert profiles to generate when `experts` is `NULL`
  (default 3).

- experts:

  Optional list of active profiles created by
  [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md).
  When supplied, STORM uses this selected team and does not generate
  experts.

- research_strategy:

  Either "key_questions" (default, faster) or "conversation" (more
  thorough but slower). Key questions uses predefined questions;
  conversation dynamically generates follow-up questions.

- max_rounds:

  Maximum rounds per perspective for the "conversation" strategy
  (default 3).

- max_questions_per_perspective:

  Maximum questions per perspective for the "key_questions" strategy
  (default 3).

- parallel_writing:

  If `TRUE`, write report sections in parallel using the mirai package.
  Failed parallel sections are retried sequentially.

- steps:

  Character vector controlling which steps to run. Defaults to all.

- output_dir:

  Optional directory for persisted STORM run artifacts. When supplied, a
  current schema-7 product bundle with schema-4 STORM state is written
  under a topic-specific subdirectory.

- resume:

  If `TRUE` and `output_dir` contains a previous run, load saved
  current-format artifacts and skip stages recorded as complete. Older,
  future, missing, extra, or mismatched product shapes are rejected
  rather than migrated.

- run_id:

  Optional run directory name. Defaults to a slug of `topic`.

- progress:

  Optional function called with a `tempest_progress_event` object as
  STORM workflow stages start, finish, fail, persist artifacts, or make
  final artifacts available.

- verbose:

  If `TRUE`, prints progress messages.

## Value

A validated completed Tempest research product. Read it with
[`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md),
[`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md),
[`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md),
[`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md),
and
[`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- tempest_config()
result <- tempest_run("History of jazz", config = cfg)
cat(tempest_report(result))
} # }
```
