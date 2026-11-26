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
tempest_app(
  ...,
  demo = FALSE,
  graft_store = NULL,
  memory_purpose = "research",
  memory_actor = NULL,
  memory_eligible = NULL
)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- demo:

  If `TRUE`, open the deterministic ecosystem demonstration. Scripted
  model responses and synthetic local evidence exercise real deputy and
  dsprrr execution, a shinymcp research card, and explicit Graft review,
  correction, withdrawal, and fresh-process reuse. No credentials or
  network are needed. The example uses a local demo directory; set
  `options(tempest.demo_dir = "path")` to keep it across R sessions.

- graft_store:

  Optional local directory path or
  [`graft::ArtifactStore`](https://jameshwade.github.io/graft/reference/ArtifactStore.html)
  (including
  [`graft::graft_store_postgres()`](https://jameshwade.github.io/graft/reference/graft_store_postgres.html))
  for reviewed project memory. Defaults to `NULL` (disabled). When
  supplied, Workspace settings can pin an accepted stream for new
  sessions or attach read-only `graft_tools()` recall (with `shinychat`
  tool cards), and the Report tab adds explicit review, acceptance,
  correction, and withdrawal controls.

- memory_purpose:

  Consultation purpose recorded on Graft decisions and memory recall
  tools. Defaults to `"research"`.

- memory_actor:

  Default reviewer label shown in the Graft review panel. Defaults to
  `"local-reviewer"`.

- memory_eligible:

  Optional one-argument host eligibility function `function(stream)`
  returning `TRUE` or `FALSE`. Defaults to allowing all streams in the
  store. Multi-user hosts must supply an authenticated policy function.

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
tempest_app(graft_store = "research-memory")
} # }
```
