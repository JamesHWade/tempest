# Optimize STORM dsprrr modules

**\[experimental\]**

## Usage

``` r
tempest_optimize_dsprrr_modules(
  trainsets,
  modules = NULL,
  config = tempest_config(),
  teleprompter = NULL,
  valsets = NULL,
  .llm = NULL,
  save_dir = NULL,
  k = 4L,
  seed = 123L,
  strict = TRUE,
  verbose = TRUE
)
```

## Arguments

- trainsets:

  Named list of training data frames. Names must match module names from
  [`tempest_make_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_make_dsprrr_modules.md),
  such as `"query_decomposition"`, `"extract_claims"`,
  `"section_writing"`, or `"lead_section"`.

- modules:

  Optional named list of modules to optimize. Defaults to fresh modules
  from `tempest_make_dsprrr_modules(config)`.

- config:

  A `TempestConfig` object used when `modules` is `NULL`.

- teleprompter:

  Optional dsprrr teleprompter, named list of teleprompters, or factory
  function. Defaults to
  [`dsprrr::LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.html).

- valsets:

  Optional named list of validation data frames.

- .llm:

  Optional ellmer chat object passed to dsprrr compilation.

- save_dir:

  Optional directory or `.rds` path for the optimized module list.

- k:

  Number of examples for the default `LabeledFewShot` teleprompter.

- seed:

  Random seed for the default teleprompter.

- strict:

  If `TRUE`, abort on the first compile failure. If `FALSE`, warn and
  keep the unoptimized module for that step.

- verbose:

  If `TRUE`, print optimization progress.

## Value

A named list of dsprrr modules.

## Details

Compiles selected STORM dsprrr modules against user-provided training
data. This is an explicit optimization step: run it with labeled
examples, then pass the returned module list to
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
via `dsprrr_modules`.

## Examples

``` r
if (FALSE) { # \dontrun{
trainset <- data.frame(
  question = "What are battery recycling bottlenecks?",
  topic = "lithium batteries"
)
trainset$queries <- list(c("battery recycling", "EV battery capacity"))
modules <- tempest_optimize_dsprrr_modules(
  trainsets = list(query_decomposition = trainset),
  config = tempest_config()
)
} # }
```
