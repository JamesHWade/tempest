# Save compiled dsprrr modules

Save compiled dsprrr modules

## Usage

``` r
tempest_save_dsprrr_modules(modules, path)
```

## Arguments

- modules:

  A named list of dsprrr modules.

- path:

  File path ending in `.rds`, or a directory where `dsprrr-modules.rds`
  will be written.

## Value

Invisibly returns the RDS path.

## Examples

``` r
if (FALSE) { # \dontrun{
modules <- tempest_optimize_dsprrr_modules(
  trainsets = list(query_decomposition = trainset)
)
path <- tempest_save_dsprrr_modules(modules, tempfile(fileext = ".rds"))
} # }
```
