# Run the Tempest research application

Launches an interactive app that provides:

- Co-STORM chat, sources, facts, mind map, transcript, and committed
  reports;

- asynchronous scripted STORM research and report publication; and

- bounded Co-STORM session archive download and upload without autosave.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

A Shiny app object (invisibly, from
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)).

## Details

Live progress, persistence, and successful publication are announced
through polite status regions. Validation, cancellation, and publication
failures are announced as alerts.

## Examples

``` r
if (FALSE) { # \dontrun{
run_app()
} # }
```
