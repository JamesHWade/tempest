# Inform user with a formatted message

Wrapper around
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
for consistent messaging.

## Usage

``` r
tempest_inform(message, ..., .envir = rlang::caller_env())
```

## Arguments

- message:

  Message with cli formatting.

- ...:

  Additional arguments passed to
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).

- .envir:

  Environment for glue interpolation.
