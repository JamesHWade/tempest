# Load and verify a Tempest program set

Loads a closed-inventory ProgramSet bundle through
[`dsprrr::load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.html)
and recomputes every dsprrr artifact ID before the set can be used.

## Usage

``` r
tempest_load_program_set(path, registry = list())
```

## Arguments

- path:

  Directory containing a ProgramSet bundle.

- registry:

  Named runtime-binding registry required by custom dsprrr artifacts.
  The resolved modules retain bindings, while the registry is not
  included in durable ProgramSet metadata.

## Value

A verified, file-backed `TempestProgramSet`.
