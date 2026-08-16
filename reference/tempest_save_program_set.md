# Save a Tempest program set

Materializes every live program through
[`dsprrr::save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.html),
reloads it through
[`dsprrr::load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.html),
verifies its artifact identity, and writes the manifest last. The
destination must not already exist.

## Usage

``` r
tempest_save_program_set(program_set, path, registry = list())
```

## Arguments

- program_set:

  A `TempestProgramSet`.

- path:

  New directory for the ProgramSet bundle.

- registry:

  Named runtime-binding registry passed to dsprrr. It is not persisted.

## Value

A file-backed `TempestProgramSet` resolved from the new bundle.
