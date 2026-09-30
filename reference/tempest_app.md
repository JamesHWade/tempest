# Run the Tempest research application

Launches an interactive app that provides:

- Co-STORM chat, sources, facts, mind map, transcript, and committed
  reports;

- asynchronous scripted STORM research and report publication;

- a bounded, read-only Run review with separately labeled live progress;
  and

- bounded Co-STORM session archive download and upload without autosave.

## Usage

``` r
tempest_app(..., demo = FALSE)
```

## Arguments

- demo:

  If `TRUE`, open the deterministic ecosystem demonstration. Scripted
  model responses and synthetic local evidence exercise real deputy and
  dsprrr execution, a shinymcp research card, and explicit Graft review,
  correction, withdrawal, and fresh-process reuse. No credentials or
  network are needed. The example uses a local demo directory; set
  `options(tempest.demo_dir = "path")` to keep it across R sessions.

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
tempest_app()
} # }
```
