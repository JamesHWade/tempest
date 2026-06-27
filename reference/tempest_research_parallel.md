# Run research in parallel using mirai

Falls back to sequential research when mirai daemons cannot be started
or the workers fail (for example when the installed 'tempest' package is
not available to the workers). Any perspective that fails in a worker is
retried sequentially so its evidence is never silently dropped.

## Usage

``` r
tempest_research_parallel(
  perspectives,
  personas,
  config,
  retriever,
  store,
  topic,
  research_strategy,
  max_rounds,
  max_questions_per_perspective,
  dsprrr_modules = NULL,
  verbose
)
```
