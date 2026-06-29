# Load compiled dsprrr modules

**\[experimental\]**

## Usage

``` r
tempest_load_dsprrr_modules(path)
```

## Arguments

- path:

  File path ending in `.rds`, or a directory containing
  `dsprrr-modules.rds`.

## Value

A named list of dsprrr modules.

## Examples

``` r
if (FALSE) { # \dontrun{
modules <- tempest_load_dsprrr_modules("path/to/dsprrr-modules.rds")
result <- tempest_run("History of jazz", dsprrr_modules = modules)
} # }
```
