# Use Open Knowledge Format with Tempest

[Open Knowledge
Format](https://github.com/GoogleCloudPlatform/knowledge-catalog/tree/main/okf)
(OKF) packages concepts as Markdown documents with YAML metadata and
links. Tempest treats those documents as evidence inputs. It does not
treat them as system prompts, capability grants, approvals, or
executable instructions.

The boundary is deliberately staged:

``` text
read -> inspect -> select -> convert -> add to evidence -> use in a workflow
```

Each step is explicit so the host can enforce its own source, retention,
freshness, and approval policy.

## Read a bounded bundle

Point
[`tempest_read_okf()`](https://jameshwade.github.io/tempest/reference/tempest_read_okf.md)
at the bundle root:

``` r

library(tempest)

knowledge <- tempest_read_okf(
  "knowledge/okf",
  max_concepts = 5000,
  max_bytes = 20 * 1024^2
)

knowledge
knowledge$issues
```

The reader enforces the OKF conformance boundary: every concept must
have parseable YAML frontmatter and a non-empty `type`. It safely
accepts unknown types and extension keys, missing indexes, and broken
links so newer or domain-specific bundles remain readable.

The reader does not resolve network resources or execute code. It also
rejects symlinked documents and resolved paths outside the bundle root.
A concept-count limit and an aggregate Markdown byte limit apply before
content becomes available to a workflow.

Optional profile problems are diagnostics rather than parse failures.
For example, a concept can remain readable while `knowledge$issues`
reports an unknown lifecycle status, invalid generation provenance, or
an incomplete Attested Computation declaration.

## Inspect trust and freshness

Use
[`tempest_okf_concepts()`](https://jameshwade.github.io/tempest/reference/tempest_okf_concepts.md)
before selecting evidence:

``` r

catalog <- tempest_okf_concepts(
  knowledge,
  today = as.Date("2026-07-28")
)

catalog[, c(
  "concept_id",
  "type",
  "title",
  "status",
  "trust_tier",
  "stale"
)]
```

Tempest derives three advisory trust tiers from standard OKF provenance:

| Frontmatter                         | Tempest trust tier  |
|-------------------------------------|---------------------|
| `generated` only or no verification | `unverified`        |
| `verified.by` is a process          | `machine-confirmed` |
| `verified.by` is a human            | `human-reviewed`    |

`stale` is derived from `stale_after` and the supplied `today`. Neither
value is an authorization decision. A human-reviewed concept can be
stale; a fresh concept can remain unverified. The host chooses which
combination is fit for a particular workflow.

## Convert selected concepts to evidence

[`tempest_okf_resources()`](https://jameshwade.github.io/tempest/reference/tempest_okf_resources.md)
creates one fingerprinted `TempestResource` per selected concept:

``` r

resources <- tempest_okf_resources(
  knowledge,
  types = c("Assessment", "Business", "Source"),
  include_stale = FALSE
)
```

Each resource has `resource_kind = "okf.concept"` and
`media_type = "text/markdown"`. It preserves the original document as
content, stores parsed frontmatter under `resource@metadata$okf`, and
carries profile and schema identity as scope metadata when present.

Conversion does not mutate a research workspace. Add the selected
resources to a run-scoped `ResearchWorkspace` explicitly:

``` r

workspace <- tempest_research_workspace()
invisible(lapply(resources, workspace$upsert_retrieved_resource))
```

The workspace now has durable resource fingerprints and can be supplied
to a Tempest retriever. Assertions created later still need their own
claim and evidence-span lineage; importing a concept does not convert
its prose into a verified Tempest claim automatically.

## Assemble bounded model context

When an operation needs a readable packet, select it deliberately:

``` r

okf_context <- tempest_okf_context(
  knowledge,
  types = c("Assessment", "Business"),
  include_stale = FALSE,
  max_concepts = 25,
  max_chars = 50000
)

cat(okf_context)
```

Concepts are ordered deterministically. The result records document or
character truncation and begins with an explicit trust notice. That
notice states that the material is evidence, not instructions, and
cannot grant tools, change workflow policy, approve an artifact, or
authorize an external action.

The supported product path gives the selected resources to a run-scoped
retriever, then calls
[`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
directly:

``` r

cfg <- tempest_config()
retriever <- tempest_retriever(
  config = cfg,
  workspace = workspace
)
result <- tempest_run(
  "Assess the accepted organizational evidence",
  config = cfg,
  retriever = retriever
)
```

Tempest does not inject every imported concept into every model turn.
The retriever selects from the explicit provisional workspace as the
STORM product needs evidence.

## Exchange accepted Graft knowledge

Graft can produce a compatible bundle from its accepted revision ledger:

``` r

library(graft)

kg_export_okf(
  store,
  "knowledge/okf",
  classes = c("Business", "Assessment", "Source")
)
```

The Graft profile retains stable record, revision, batch, content, and
schema identity in namespaced frontmatter. Object references become
Markdown links and accepted source relationships become OKF citations. A
historical `as_of` export reproduces the knowledge boundary at a
committed batch or time.

This division keeps each package focused:

| Package | Responsibility                                              |
|---------|-------------------------------------------------------------|
| Graft   | Validate writes and retain accepted revision history        |
| OKF     | Exchange readable knowledge and provenance                  |
| Tempest | Select scientific evidence and preserve research provenance |

## Treat computations as documents

OKF can describe an Attested Computation, including a runtime, source
files, inputs, and expected outputs. Tempest validates useful metadata
when present but never executes the computation. A Tempest product
instead references an exact dsprrr program artifact and preserves
execution identity for correlation and audit joins, not as a claim of
causal content provenance.

Continue with [Get started with
Tempest](https://jameshwade.github.io/tempest/articles/tempest.md) to
use imported context in STORM or Co-STORM research.
