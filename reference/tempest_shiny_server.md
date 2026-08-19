# Run embedded Tempest Shiny panels

**\[experimental\]**

## Usage

``` r
tempest_shiny_server(
  id,
  config = tempest_config(),
  store = NULL,
  panels = c("chat", "sources", "facts", "mindmap", "transcript", "report"),
  experts = NULL,
  session_id = NULL,
  program_set = NULL,
  knowledge_view = NULL
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

- experts:

  Optional expert profiles passed to
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  for new Co-STORM sessions. May be a value, function, or reactive.

- session_id:

  Optional stable session id passed to
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  for new Co-STORM sessions. May be a value, function, or reactive.

- program_set:

  Optional
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  used for new and restored Co-STORM sessions. May be a value, function,
  or reactive. `NULL` uses the builtin set.

- knowledge_view:

  Optional immutable Graft view used with `program_set`. May be a value,
  function, or reactive. A governed ProgramSet requires its exact pinned
  view before any provider call; the view remains process-local and is
  never serialized.

## Value

A list with the shared `store`; reactive `costorm_session`,
`costorm_events`, `costorm_evidence`, `storm_events`, `report_md`,
`report_workspace`, and `report_topic` accessors; a monotonic
`report_navigation_event` counter; and a `touch_costorm_session()`
control.

## Details

`tempest_shiny_server()` pairs with
[`tempest_shiny_ui()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_ui.md)
and lets a host app provide a
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md),
optional expert profiles, a stable session id, and an optional shared
store. The returned handle exposes only product session, event,
evidence, and report state.

Progress, persistence, and successful publication use polite atomic
status regions in the bundled UI. Validation, cancellation, and
publication failures use alerts and never convert a rejected product
into report-ready state.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  tempest_shiny_server("research", config = tempest_config())
}
} # }
```
