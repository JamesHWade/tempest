# Embed Tempest panels in a Shiny UI

**\[experimental\]**

## Usage

``` r
tempest_shiny_ui(
  id,
  panels = c("chat", "sources", "facts", "mindmap", "transcript", "report"),
  show_config = FALSE
)
```

## Arguments

- id:

  Shiny module id.

- panels:

  Character vector of panels to include. Use `"all"` for every panel.
  The default embeds the Co-STORM chat and durable research views.

- show_config:

  If `TRUE`, include the bundled configuration controls in the Chat
  sidebar. Hosts that provide their own config should leave this as
  `FALSE`.

## Value

A Shiny tag object.

## Details

`tempest_shiny_ui()` is a compact Shiny module UI for host applications
that want to embed Tempest without sourcing files from `inst/shiny/R`.
It reuses the bundled app's panels while letting the host provide the
page shell, configuration, storage policy, and surrounding controls.

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- bslib::page_fillable(tempest_shiny_ui("research"))
} # }
```
