# Warn user with a formatted message

Wrapper around
[`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) for
consistent warning formatting.

## Usage

``` r
tempest_warn(message, ..., .envir = rlang::caller_env())
```

## Arguments

- message:

  Warning message with cli formatting.

- ...:

  Additional arguments passed to
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html).

- .envir:

  Environment for glue interpolation.
