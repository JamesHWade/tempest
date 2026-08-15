# Load compiled dsprrr modules

**\[experimental\]**

## Usage

``` r
tempest_load_dsprrr_modules(path, registry = list(), trusted = FALSE)
```

## Arguments

- path:

  Program bundle file created by
  [`tempest_save_dsprrr_modules()`](https://jameshwade.github.io/tempest/reference/tempest_save_dsprrr_modules.md).

- registry:

  Named runtime registry passed to
  [`dsprrr::restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.html).

- trusted:

  Whether dsprrr may restore embedded trusted runtime values.

## Value

A named list of dsprrr modules.

## Details

Validates the bundle checksum before asking dsprrr to validate and
restore each versioned program artifact.

## Examples

``` r
if (FALSE) { # \dontrun{
modules <- tempest_load_dsprrr_modules("path/to/storm-programs.rds")
result <- tempest_run("History of jazz", dsprrr_modules = modules)
} # }
```
