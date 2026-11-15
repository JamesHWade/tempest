# Enrich completed research with computations and illustrations

Creates a portable HTML report alongside its original evidence, analysis
code, outputs, images and checksummed manifest. This derived report does
not change the research product or publish accepted knowledge. Model
summaries and generated illustrations are labeled separately from
research evidence. A supplied plan avoids a model call; otherwise dsprrr
plans from the report.

## Usage

``` r
tempest_enrich_report(
  x,
  output_dir,
  plan = NULL,
  planner = NULL,
  executor = NULL,
  image_generator = NULL,
  allow_code = FALSE,
  max_analyses = 3L,
  max_illustrations = 2L
)
```

## Arguments

- x:

  A completed STORM product or finalized Co-STORM session.

- output_dir:

  A new directory to create. Existing paths are refused.

- plan:

  Optional list with `title`, `summary`, `analyses`, and
  `illustrations`. See the enriched research vignette for task fields.

- planner:

  Optional function of `report`, `sources`, `max_analyses`, and
  `max_illustrations`, returning a plan. Used only when `plan` is
  `NULL`.

- executor:

  Optional function of `task` and `directory` returning `output`,
  `files`, and `metadata`. Use
  [`tempest_r_executor()`](https://jameshwade.github.io/tempest/reference/tempest_r_executor.md)
  for trusted R.

- image_generator:

  Optional function of `task` and `directory` returning `file` and
  `metadata`. See
  [`tempest_image_generator()`](https://jameshwade.github.io/tempest/reference/tempest_image_generator.md).

- allow_code:

  Explicit authorization to execute the plan's code. The default is
  `FALSE`; code is saved for review but not executed. The built-in R
  executor uses the local account's access and is not an OS sandbox.

- max_analyses, max_illustrations:

  Task budgets, from zero to ten.

## Value

A validated `TempestEnrichedReport` with `path`, `status`, and
`manifest` properties. `status` is `partial` if any task failed or
skipped.
