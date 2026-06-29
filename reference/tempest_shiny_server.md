# Run embedded Tempest Shiny panels

**\[experimental\]**

## Usage

``` r
tempest_shiny_server(
  id,
  config = tempest_config(),
  store = NULL,
  panels = c("chat", "sources", "facts", "mindmap", "transcript", "report"),
  personas = NULL,
  session_id = NULL
)
```

## Arguments

- id:

  Shiny module id.

- config:

  A `TempestConfig`, a reactive returning one, a function returning one,
  or `NULL` to use the bundled config module defaults.

- store:

  Optional store from
  [`tempest_shiny_store()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_store.md).
  If `NULL`, a new store is created.

- panels:

  Character vector matching the UI panels.

- personas:

  Optional expert definitions passed to
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  for new Co-STORM sessions. May be a value, function, or reactive.

- session_id:

  Optional stable session id passed to
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  for new Co-STORM sessions. May be a value, function, or reactive.

## Value

A list with `store`, `session`, `report`, and `report_ready`.

## Details

`tempest_shiny_server()` pairs with
[`tempest_shiny_ui()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_ui.md)
and lets a host app provide a runtime
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md),
optional expert personas, a stable session id, and an optional shared
store. The returned handle exposes the shared store and reactive
accessors for the current session, report, and report readiness signal.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  tempest_shiny_server("research", config = tempest_config())
}
} # }
```
