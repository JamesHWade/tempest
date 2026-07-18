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
  runtime = tempest_runtime(),
  connection_permissions = list(),
  session_id = NULL,
  run = NULL
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

- runtime:

  A
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md),
  function, or reactive used to resolve process-local operations,
  capabilities, and connections.

- connection_permissions:

  Named per-role or per-expert connection allow-lists passed to
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

- session_id:

  Optional stable session id passed to
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  for new Co-STORM sessions. May be a value, function, or reactive.

- run:

  Optional `TempestRun`, function, or reactive. This lets a host expose
  a custom headless workflow through the same generic adapter reactives
  as built-in workflows.

## Value

A list with the shared `store`; reactive `run`, `status`, `events`,
`approvals`, `assignments`, `artifacts`, `evidence`, `grants`,
`session`, `report`, and `report_ready` accessors; and `approve()`,
`cancel()`, and `touch()` controls.

## Details

`tempest_shiny_server()` pairs with
[`tempest_shiny_ui()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_ui.md)
and lets a host app provide a
[TempestConfig](https://jameshwade.github.io/tempest/reference/TempestConfig.md),
process-local
[`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md),
optional expert profiles, a stable session id, and an optional shared
store. The returned handle exposes the shared store and reactive
accessors for generic run, event, approval, capability-grant, artifact,
session, and report state.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  tempest_shiny_server("research", config = tempest_config())
}
} # }
```
