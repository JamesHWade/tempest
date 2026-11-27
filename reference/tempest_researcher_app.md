# Launch the enriched research application

Opens a dedicated Shiny application for research briefs, optional R
analyses and illustrations, and portable reports. Each research or MCP
discovery job runs in a separate R process. Cancelling a job or closing
its browser session stops its process tree. The existing
[`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md)
remains available for interactive Co-STORM sessions.

## Usage

``` r
tempest_researcher_app(
  ...,
  config = tempest_config(),
  tools = list(),
  mcp_config = NULL,
  output_dir = file.path(tempdir(), "tempest-researcher"),
  model = "openai/gpt-6.1-sol",
  report_library = FALSE,
  graft_store = NULL,
  memory_purpose = "research",
  memory_actor = NULL,
  memory_eligible = NULL
)
```

## Arguments

- ...:

  Arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- config:

  Research configuration from
  [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md).
  The model selected in the app applies to all research roles. Custom
  chat factories and other configuration settings are retained.

- tools:

  Named list of trusted, zero-argument R functions. Each selected
  function creates a list of ellmer tool definitions inside the research
  worker. Factories must work in a fresh R process; use explicit package
  namespaces and do not capture open connections. Tools are attached
  only to research experts, under Tempest's existing Deputy research
  permissions.

- mcp_config:

  Optional path to an MCP JSON configuration with a `mcpServers` object.
  The app also accepts a session-local JSON upload. Only explicitly
  selected servers are contacted during discovery, and only selected
  tools are attached during research. MCP transport and authentication
  are supplied by Deputy and its supported mcptools versions.

- output_dir:

  Parent directory for newly generated report directories. Existing
  reports are never overwritten. The default lasts for the current R
  session; choose a persistent directory to retain reports after R
  exits.

- model:

  Initial provider/model selection, editable in the application. When
  omitted with a configured Chat object, starts with that Chat's model.
  A supplied Chat can change models within its provider; use a custom
  `config` chat factory to support switching providers.

- report_library:

  Enable browsing previously published app reports in `output_dir`.
  Defaults to `FALSE`. When enabled, every user of this app can open and
  download the reports in that directory. Use a dedicated directory
  managed by the host and enable this only for an audience that can read
  the entire collection.

- graft_store:

  Optional local directory path,
  [`graft::ArtifactStore`](https://jameshwade.github.io/graft/reference/ArtifactStore.html)
  (including
  [`graft::graft_store()`](https://jameshwade.github.io/graft/reference/graft_store.html)
  and
  [`graft::graft_store_postgres()`](https://jameshwade.github.io/graft/reference/graft_store_postgres.html)),
  or zero-argument store factory `function()` returning a
  [`graft::ArtifactStore`](https://jameshwade.github.io/graft/reference/ArtifactStore.html)
  for reviewed project memory. Defaults to `NULL` (disabled). When
  supplied, completed runs are published as unaccepted candidates that
  you can review, accept into a named stream, correct, or withdraw; new
  runs can consult accepted streams through read-only
  [`graft::graft_tools()`](https://jameshwade.github.io/graft/reference/graft_tools.html)
  or pin an accepted stream as their comparison basis. For PostgreSQL
  stores, pass a zero-argument factory function or configure standard
  `libpq` credentials (`PGPASSWORD`, `.pgpass`, or `PGSERVICE`) so
  isolated background workers can reconnect without serializing
  credentials to disk.

- memory_purpose:

  Consultation purpose recorded on Graft decisions and memory recall
  tools. Defaults to `"research"`.

- memory_actor:

  Default reviewer label shown in the Graft review panel. Defaults to
  `"local-reviewer"`.

- memory_eligible:

  Optional one-argument host eligibility function `function(stream)`
  returning `TRUE` or `FALSE`. Defaults to allowing all streams in the
  local store. Multi-user hosts must supply an authenticated policy
  function.

## Value

Invisibly, the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

Run the application for trusted users. MCP configurations can launch
local programs, and host-supplied tool factories execute R code.
Discovery does not enable tools. Tool selections do not override
Deputy's research permissions: tools requiring filesystem writes, shell
execution, or other denied effects remain unavailable. External tool
output is research context, not automatically a verified source or
claim.

Generated R analysis and image generation are disabled initially and
enabled separately for each run. R execution uses the local account's
access; it is not an OS sandbox. Image generation uses
[`tempest_image_generator()`](https://jameshwade.github.io/tempest/reference/tempest_image_generator.md)
and reads `OPENAI_API_KEY` in the worker. Credentials and uploaded MCP
configuration contents are not copied into report downloads. Host tools
remain responsible for the contents of their own returned data.

A configured Ragnar store is copied into private staging for each
research run and opened inside its worker. Existing corpus contents
remain available; newly ingested chunks belong to that run and are not
written back to the host store. The temporary copy is removed when the
job ends. Embedding functions must work in a fresh R process without
captured open connections.

## Examples

``` r
if (FALSE) { # \dontrun{
tempest_researcher_app(
  output_dir = "research-reports",
  report_library = TRUE,
  graft_store = "research-memory"
)

tempest_researcher_app(
  tools = list(plant_data = function() {
    list(ellmer::tool(
      function() as.data.frame(datasets::CO2),
      name = "read_co2_data",
      description = "Read the built-in CO2 plant experiment data",
      arguments = list(),
      annotations = ellmer::tool_annotations(read_only_hint = TRUE)
    ))
  }),
  mcp_config = "mcp.json"
)
} # }
```
