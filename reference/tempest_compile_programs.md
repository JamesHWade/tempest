# Compile programs in a Tempest program set

Compiles the selected stages through
[`dsprrr::compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.html)
and only publishes a new complete ProgramSet after every requested
compilation and artifact verification succeeds. Compilation errors are
never replaced with the original uncompiled program.

## Usage

``` r
tempest_compile_programs(
  program_set,
  trainsets,
  teleprompters,
  path,
  valsets = NULL,
  registry = list(),
  .llm = NULL,
  compile_args = NULL
)
```

## Arguments

- program_set:

  A `TempestProgramSet`.

- trainsets:

  Named list of training data by stage. Its names select the stages to
  compile.

- teleprompters:

  One dsprrr Teleprompter used for every selected stage, or an exact
  named list of Teleprompters.

- path:

  New directory for the compiled ProgramSet bundle.

- valsets:

  Optional exact named list of validation data for the selected stages.
  Omitted stages receive `NULL` only when the whole argument is omitted.

- registry:

  Named runtime-binding registry passed to dsprrr artifact operations.
  It is not persisted.

- .llm:

  Optional language-model runtime passed to dsprrr compilation.

- compile_args:

  Optional exact named list of argument lists passed to
  [`dsprrr::compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.html)
  for each selected stage.

## Value

A new verified, file-backed `TempestProgramSet`.
