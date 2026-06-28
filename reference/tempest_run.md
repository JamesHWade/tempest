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
  n_experts = 3,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  parallel_research = FALSE,
  parallel_writing = FALSE,
  dsprrr_modules = NULL,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  output_dir = NULL,
  resume = FALSE,
  run_id = NULL,
  remove_duplicate = FALSE,
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

- n_experts:

  Number of expert personas to use (default 3).

- research_strategy:

  Either "key_questions" (default, faster) or "conversation" (more
  thorough but slower). Key questions uses predefined questions;
  conversation dynamically generates follow-up questions.

- max_rounds:

  Maximum rounds per perspective for "conversation" strategy (default
  3).

- max_questions_per_perspective:

  Maximum questions per perspective for "key_questions" strategy
  (default 3).

- parallel_research:

  If `TRUE`, run research perspectives in parallel using the mirai
  package. Requires mirai to be installed. Default `FALSE`.

- parallel_writing:

  If `TRUE`, write report sections in parallel using the mirai package.
  Failed parallel sections are retried sequentially.

- dsprrr_modules:

  Optional named list of dsprrr modules, typically from
  [`tempest_optimize_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_optimize_dsprrr_modules.md).
  If `NULL`, fresh modules are created.

- steps:

  Character vector controlling which steps to run. Defaults to all.

- output_dir:

  Optional directory for persisted STORM run artifacts. When supplied,
  artifacts are written under a topic-specific subdirectory.

- resume:

  If `TRUE` and `output_dir` contains a previous run, load saved
  artifacts and skip stages recorded as complete.

- run_id:

  Optional run directory name. Defaults to a slug of `topic`.

- remove_duplicate:

  If `TRUE`, ask the polish step to remove duplicate or highly
  repetitive content while preserving unique cited claims.

- verbose:

  If `TRUE`, prints progress messages.

## Value

A list with `title`, `outline`, `draft_md`, `report_md`, `store`, and
`output_dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- tempest_config()
result <- tempest_run("History of jazz", config = cfg)
cat(result$report_md)
} # }
```
