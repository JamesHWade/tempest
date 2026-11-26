# tempest

STORM & Co-STORM research agents for R

## Research that checks its sources and remembers what your team reviewed.

Tempest runs multi-perspective research workflows in R, verifies every
claim against exact source quotes, pairs reports with reproducible R
analyses, and connects to **graft** so reviewed findings carry cleanly
into your next session, app, or MCP client.

[Get started](https://jameshwade.github.io/tempest/articles/tempest.md)
[Research that remembers with
Graft](https://jameshwade.github.io/tempest/articles/research-memory.md)
[Read a live
report](https://jameshwade.github.io/tempest/live-research/index.md)

## How Tempest works

Step 1

### Investigate from multiple angles

Run a scripted **STORM** pipeline or hold an interactive **Co-STORM**
panel discussion. Specialists search the web, your `ragnar` store, host
R tools, or MCP servers.

Step 2

### Verify claims & run analyses

Every claim is checked against an exact source span. Optionally add
scientific claim review, targeted evidence repair, reproducible R
computations, and conceptual illustrations.

Step 3

### Review & remember with Graft

Nothing becomes organizational knowledge automatically. Review reports
in the app, explicitly accept or withdraw streams in **graft**, and let
future agents consult only what is currently approved.

## Quick start in R

``` r

library(tempest)

# 1. Run a scripted STORM report
cfg <- tempest_config(
  models = "openai/gpt-5.6-luna",
  search_provider = "wikipedia"
)
result <- tempest_run(
  "Technical and policy barriers to grid-scale battery recycling",
  config = cfg,
  n_experts = 2,
  max_questions_per_perspective = 2
)

cat(tempest_report(result))
tempest_claims(result)
tempest_claim_supports(result)
```

Or launch the Researcher app with a persistent report library and Graft
project memory:

``` r

tempest_researcher_app(
  output_dir = "research-reports",
  report_library = TRUE,
  graft_store = "research-memory"
)
```

## Choose your interactive app

### Researcher app

Draft or pick a starter brief, attach host R tools or MCP servers, run
optional R analyses, browse saved reports, and accept findings into
Graft.

`tempest_researcher_app(graft_store = “memory”)`

### Co-STORM workspace

Explore an open-ended question interactively with a panel of experts,
live mind map, evidence tables, shinychat memory cards, and report
publishing.

`tempest_app(graft_store = “memory”)`

### Offline ecosystem demo

Try real deputy, dsprrr, shinymcp, and graft execution locally with zero
API keys or network calls—accept, correct, withdraw, and reopen exact
evidence.

`tempest_app(demo = TRUE)`

## Tempest in the ellmer ecosystem

Tempest focuses on scientific research, claim-centered evidence, and
reports, delegating horizontal concerns to companion packages:

| Package | Role | How it pairs with Tempest |
|----|----|----|
| **ellmer** & **shinychat** | LLM calls, tools, and streaming chat UI | Drives every model role and renders interactive Co-STORM conversations and reviewed-memory tool cards |
| **deputy** | Permission-bounded agent runtime | Runs moderator and expert sessions with explicit tool allowlists and terminal execution traces |
| **dsprrr** | Typed structured language programs | Executes structured stages (perspectives, claim extraction, verification, outline, writing) |
| **graft** | Reviewed project memory & persistent artifacts | Stores immutable research candidates, explicit human accept/withdraw decisions, and multi-stream recall tools (`graft_tools()`) |
| **ragnar** | Chunking & semantic retrieval (RAG) | Finds candidate passages across large corpora; pair with Graft so **ragnar finds and graft verifies** |
| **shinymcp** & **mcptools** | MCP Apps & MCP tool servers | Inspect completed research in read-only cards ([`tempest_mcp_app()`](https://jameshwade.github.io/tempest/reference/tempest_mcp_app.md)) and serve reviewed Graft streams to IDE agents |
| **scans** & **vitals** | Trajectory inspection & evaluation | Audit completed trajectories and `@extra$graft` consultations with `scans`; evaluate models offline with `vitals` |

## Guides

### Get started with Tempest

Pick a workflow, configure models and search, produce your first cited
STORM report, and explore Co-STORM.

[Read the getting-started
guide](https://jameshwade.github.io/tempest/articles/tempest.md)

### Enriched reports & the Researcher app

Combine cited research with reproducible R analyses, custom tools, MCP
servers, and a saved-report library.

[Build enriched research
reports](https://jameshwade.github.io/tempest/articles/enriched-research.md)

### Research that remembers with Graft

Review, accept, correct, and withdraw research streams, enable
multi-stream agent recall, and serve memory over MCP.

[Connect Graft project
memory](https://jameshwade.github.io/tempest/articles/research-memory.md)

### Decision briefings & scientific review

Run recurring briefings that flag what changed against accepted claims,
or enable adversarial claim review and evidence repair.

[Daily
briefings](https://jameshwade.github.io/tempest/articles/daily-briefing.md)
· [Scientific
review](https://jameshwade.github.io/tempest/articles/scientific-review.md)
