# tempest

<div class="tempest-hero">
<p class="tempest-eyebrow">STORM &amp; Co-STORM research agents for R</p>
<h2 data-toc-skip>Research that checks its sources<br>and remembers what your team reviewed.</h2>
<p class="tempest-hero-copy">
Tempest runs multi-perspective research workflows in R, verifies every claim
against exact source quotes, pairs reports with reproducible R analyses, and
connects to <strong>graft</strong> so reviewed findings carry cleanly into your
next session, app, or MCP client.
</p>
<div class="tempest-actions">
<a class="btn btn-primary" href="articles/tempest.html">Get started</a>
<a class="btn btn-outline-secondary" href="articles/research-memory.html">Research that remembers with Graft</a>
<a class="btn btn-outline-secondary" href="live-research/index.html">Read a live report</a>
</div>
</div>

## How Tempest works

<div class="tempest-steps">
<div class="tempest-step">
<span class="tempest-step-num">Step 1</span>
<h3>Investigate from multiple angles</h3>
<p>Run a scripted <strong>STORM</strong> pipeline or hold an interactive <strong>Co-STORM</strong> panel discussion. Specialists search the web, your <code>ragnar</code> store, host R tools, or MCP servers.</p>
</div>
<div class="tempest-step">
<span class="tempest-step-num">Step 2</span>
<h3>Verify claims &amp; run analyses</h3>
<p>Every claim is checked against an exact source span. Optionally add scientific claim review, targeted evidence repair, reproducible R computations, and conceptual illustrations.</p>
</div>
<div class="tempest-step">
<span class="tempest-step-num">Step 3</span>
<h3>Review &amp; remember with Graft</h3>
<p>Nothing becomes organizational knowledge automatically. Review reports in the app, explicitly accept or withdraw streams in <strong>graft</strong>, and let future agents consult only what is currently approved.</p>
</div>
</div>

## Quick start in R

```r
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

Or launch the Researcher app with a persistent report library and Graft project memory:

```r
tempest_researcher_app(
  output_dir = "research-reports",
  report_library = TRUE,
  graft_store = "research-memory"
)
```

## Choose your interactive app

<div class="tempest-apps">
<div class="tempest-app-card">
<h3>Researcher app</h3>
<p>Draft or pick a starter brief, attach host R tools or MCP servers, run optional R analyses, browse saved reports, and accept findings into Graft.</p>
<p><code>tempest_researcher_app(graft_store = "memory")</code></p>
</div>
<div class="tempest-app-card">
<h3>Co-STORM workspace</h3>
<p>Explore an open-ended question interactively with a panel of experts, live mind map, evidence tables, shinychat memory cards, and report publishing.</p>
<p><code>tempest_app(graft_store = "memory")</code></p>
</div>
<div class="tempest-app-card">
<h3>Offline ecosystem demo</h3>
<p>Try real deputy, dsprrr, shinymcp, and graft execution locally with zero API keys or network calls—accept, correct, withdraw, and reopen exact evidence.</p>
<p><code>tempest_app(demo = TRUE)</code></p>
</div>
</div>

## Tempest in the ellmer ecosystem

Tempest focuses on scientific research, claim-centered evidence, and reports, delegating horizontal concerns to companion packages:

| Package | Role | How it pairs with Tempest |
|---|---|---|
| **ellmer** & **shinychat** | LLM calls, tools, and streaming chat UI | Drives every model role and renders interactive Co-STORM conversations and reviewed-memory tool cards |
| **deputy** | Permission-bounded agent runtime | Runs moderator and expert sessions with explicit tool allowlists and terminal execution traces |
| **dsprrr** | Typed structured language programs | Executes structured stages (perspectives, claim extraction, verification, outline, writing) |
| **graft** | Reviewed project memory & persistent artifacts | Stores immutable research candidates, explicit human accept/withdraw decisions, and multi-stream recall tools (`graft_tools()`) |
| **ragnar** | Chunking & semantic retrieval (RAG) | Finds candidate passages across large corpora; pair with Graft so **ragnar finds and graft verifies** |
| **shinymcp** & **mcptools** | MCP Apps & MCP tool servers | Inspect completed research in read-only cards (`tempest_mcp_app()`) and serve reviewed Graft streams to IDE agents |
| **scans** & **vitals** | Trajectory inspection & evaluation | Audit completed trajectories and `@extra$graft` consultations with `scans`; evaluate models offline with `vitals` |

## Guides

<div class="tempest-guides">
<section>
<h3>Get started with Tempest</h3>
<p>Pick a workflow, configure models and search, produce your first cited STORM report, and explore Co-STORM.</p>
<p><a href="articles/tempest.html">Read the getting-started guide</a></p>
</section>
<section>
<h3>Enriched reports &amp; the Researcher app</h3>
<p>Combine cited research with reproducible R analyses, custom tools, MCP servers, and a saved-report library.</p>
<p><a href="articles/enriched-research.html">Build enriched research reports</a></p>
</section>
<section>
<h3>Research that remembers with Graft</h3>
<p>Review, accept, correct, and withdraw research streams, enable multi-stream agent recall, and serve memory over MCP.</p>
<p><a href="articles/research-memory.html">Connect Graft project memory</a></p>
</section>
<section>
<h3>Decision briefings &amp; scientific review</h3>
<p>Run recurring briefings that flag what changed against accepted claims, or enable adversarial claim review and evidence repair.</p>
<p><a href="articles/daily-briefing.html">Daily briefings</a> · <a href="articles/scientific-review.html">Scientific review</a></p>
</section>
</div>
