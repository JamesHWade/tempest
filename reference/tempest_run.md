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
  experts = NULL,
  runtime = tempest_runtime(),
  runtime_factory = function() tempest_runtime(),
  connection_permissions = list(),
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
  progress = NULL,
  verbose = TRUE,
  artifact_catalog = NULL,
  workflow_run = NULL
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

  Number of expert profiles to generate when `experts` is `NULL`
  (default 3).

- experts:

  Optional list of active profiles created by
  [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md).
  When supplied, STORM uses this selected team and does not generate
  experts.

- runtime:

  Frozen Tempest 0.1
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
  adapter. Existing integrations only.

- runtime_factory:

  Function that recreates the frozen 0.1 `runtime` inside parallel
  workers. Existing integrations using a custom runtime with
  `parallel_research = TRUE` must provide a matching factory.

- connection_permissions:

  Frozen Tempest 0.1 mapping from expert or model-role ids to opaque
  connection ids allowed for this run.

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

- progress:

  Optional function called with a `tempest_progress_event` object as
  STORM workflow stages start, finish, fail, persist artifacts, or make
  final artifacts available.

- verbose:

  If `TRUE`, prints progress messages.

- artifact_catalog:

  Frozen Tempest 0.1 shared `TempestArtifactCatalog`, used by the
  generic STORM workflow adapter.

- workflow_run:

  Frozen Tempest 0.1 owning `TempestRun`. When supplied, the result
  exposes it as `workflow_run`.

## Value

A list with product fields `title`, `perspectives`, `experts`,
`outline`, `draft_md`, `report_md`, `store`, and `output_dir`. Frozen
0.1 compatibility fields `artifact_catalog` and `workflow_run` are also
returned temporarily.

## Frozen Tempest 0.1 seams

`runtime`, `runtime_factory`, `connection_permissions`,
`artifact_catalog`, and `workflow_run` expose the frozen experimental
generic kernel. They remain only for existing Tempest 0.1 integrations
and are scheduled for replacement or removal in Tempest 0.2.0. New code
should consume `report_md` and the scientific evidence in `store`.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- tempest_config()
result <- tempest_run("History of jazz", config = cfg)
cat(result$report_md)
} # }
```
