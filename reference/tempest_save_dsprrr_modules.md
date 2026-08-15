# Save compiled dsprrr modules

**\[experimental\]**

## Usage

``` r
tempest_save_dsprrr_modules(
  modules,
  path,
  registry = list(),
  trusted = FALSE,
  overwrite = FALSE
)
```

## Arguments

- modules:

  A non-empty named list of dsprrr modules.

- path:

  Program bundle file ending in `.rds`.

- registry:

  Named runtime registry passed to
  [`dsprrr::program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.html).

- trusted:

  Whether dsprrr may embed trusted runtime values. The safer default is
  `FALSE`; prefer stable registry IDs.

- overwrite:

  Whether to replace an existing Tempest dsprrr bundle.

## Value

Invisibly returns the normalized bundle path.

## Details

Saves every module as a dsprrr versioned program artifact inside one
checksummed Tempest bundle. Runtime chats, credentials, caches, and
execution history are not persisted. The completed bundle is installed
atomically.

## Examples

``` r
if (FALSE) { # \dontrun{
modules <- tempest_optimize_dsprrr_modules(
  trainsets = list(query_decomposition = trainset)
)
path <- tempest_save_dsprrr_modules(modules, "storm-programs.rds")
} # }
```
