# Abort with a formatted error message

Wrapper around
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) for
consistent error formatting.

## Usage

``` r
tempest_abort(message, ..., .envir = rlang::caller_env())
```

## Arguments

- message:

  Error message with cli formatting.

- ...:

  Additional arguments passed to
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

- .envir:

  Environment for glue interpolation.

## Value

Never returns; always throws an error.
