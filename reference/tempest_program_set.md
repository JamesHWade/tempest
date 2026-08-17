# Create a validated Tempest program set

`tempest_program_set()` resolves the exact ten dsprrr programs used by
the STORM and Co-STORM product stages. With `programs = NULL` and
`path = NULL`, it creates the package's addressable builtin programs
without writing files. Custom programs require `path` and are persisted
immediately as verified dsprrr program artifacts.

## Usage

``` r
tempest_program_set(
  programs = NULL,
  path = NULL,
  contract_versions = 1L,
  evaluators = NULL,
  governed_procedure_refs = list(),
  registry = list()
)
```

## Arguments

- programs:

  `NULL` for the exact package builtin programs, or an exact named list
  of ten dsprrr Module objects.

- path:

  Optional directory in which to create a file-backed ProgramSet. It
  must not already exist. Custom `programs` require this argument.

- contract_versions:

  Contract version `1`, or an exact named integer vector containing
  version `1` for every stage.

- evaluators:

  `NULL` for Tempest's named stage output-contract evaluators, or an
  exact named list whose records contain `evaluator_id` and
  `evaluator_version`. These identify how stage output is judged and are
  distinct from an optimization teleprompter or metric.

- governed_procedure_refs:

  Optional named list of typed
  [`tempest_governed_procedure_ref()`](https://jameshwade.github.io/tempest/reference/tempest_governed_procedure_ref.md)
  values by stage.

- registry:

  Named runtime-binding registry passed to dsprrr artifact operations.
  It is never stored in ProgramSet metadata.

## Value

A validated `TempestProgramSet` S7 object.

## Details

The returned live value retains executable modules. Its manifest
projection contains only portable identifiers, evaluator metadata,
governed-procedure references, and builtin or bundle-relative artifact
references.

Resume compares program identity independently of physical location, so
a relocated verified bundle is accepted. A custom-program run must
resume with a ProgramSet carrying the same identities. Omitting
`program_set` at a run boundary resolves the package's current builtins
and can resume only when those builtin artifact IDs match the persisted
run.
