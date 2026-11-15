# Enriching an existing Researcher report with reproducible R analysis

**Date:** 2026-11-14

**Status:** Research findings and implementation requirements
**Scope:** Tempest enrichment of an already-produced research report; not a
claim that Tempest reproduces Microsoft 365 Copilot internally or outperforms
it.

**Implementation boundary:** This document records research-derived target
requirements. The shipped contract is narrower and is specified in
`dev/plans/2026-11-14-enriched-researcher.md`: completed Tempest products,
bounded plans, opt-in local R callbacks, optional image callbacks, and portable
checksummed reports. It does not add an uploaded-data staging API, an OS
sandbox, automatic scientific approval of generated analyses, package lockfile
reconstruction, or Microsoft tenant/Graph integration. Runtime metadata records
R/platform/Deputy versions rather than a complete environment lockfile.

The product facts below come from current Microsoft first-party documentation
and product pages. They describe the user-visible contract, not a benchmark of
quality. The requirements that follow are deliberately narrower: preserve the
existing report and source ledger, add auditable R analysis when data supports
it, and keep illustrative images separate from evidence and data figures.

## Microsoft baseline

### Researcher report workflow

- Microsoft describes Researcher as a multi-step research agent that gathers,
  analyzes, and summarizes web and (where available) work content into a
  structured, source-cited report. The documented flow is: ask a question;
  answer optional clarifying questions; let the agent find and analyze sources;
  receive a report with insights, citations, and next steps; then edit, share,
  or use the report to start a document or presentation. [Microsoft Support:
  Get started with Researcher](https://support.microsoft.com/en-us/microsoft-365-copilot/get-started-with-researcher-in-microsoft-365-copilot)
- Microsoft explicitly lists organized sections, visuals/charts/graphs, and
  cited sources as possible parts of the final Researcher report. “Can include”
  is the documented promise; it does not mean every report has a chart or that
  a visual is reproducible from the cited source. [Microsoft Learn: Researcher
  agent](https://learn.microsoft.com/en-us/copilot/manage/researcher-agent)
- The Researcher FAQ says citations are a core design principle and that
  Researcher uses Microsoft Graph, connectors, and the Bing index for recent
  web data. It also says web search can be disabled at the tenant level and
  there is no granular per-site allowlist in that experience. Tempest should
  therefore expose its own explicit source scope rather than imply that its
  source universe is equivalent to Microsoft’s. [Microsoft Learn: Researcher
  FAQ](https://learn.microsoft.com/en-us/copilot/manage/faq-researcher)

### Analyst and explicit code execution

- Microsoft positions Analyst as the data-analysis agent, not as another name
  for Researcher. Its support workflow has the user ask a data question and
  attach one or more local or OneDrive files; the documented outputs include
  statistics, trends, outliers, and a readable report that can contain charts
  and tables. [Microsoft Support: Get started with Analyst](https://support.microsoft.com/en-us/microsoft-365-copilot/get-started-with-analyst-in-microsoft-365-copilot)
- Microsoft’s product announcement is more specific about implementation:
  Analyst can run Python for complex data queries, and users can view the code
  being run and check its work in real time. This is evidence for a Python
  execution-and-inspection capability in Analyst; it is not evidence that
  Researcher itself executes R or Python. [Microsoft: Researcher and Analyst
  generally available](https://www.microsoft.com/en-us/copilot/blog/2025/06/02/researcher-and-analyst-are-now-generally-available-in-microsoft-365-copilot/)
- Microsoft separately documents Code Interpreter for declarative agents. It
  writes and runs Python in a sandbox, returns the generated code with the
  result, accepts common uploaded document/data formats, and can produce
  downloadable images, charts, spreadsheets, or documents. Generated files are
  hosted only for the active session and are not persisted after it ends. [Microsoft
  Learn: Code Interpreter capability](https://learn.microsoft.com/en-us/microsoft-365/copilot/extensibility/code-interpreter)

For Tempest, the resulting boundary is:

| Concern | Microsoft-documented role | Tempest requirement |
| --- | --- | --- |
| Retrieval and synthesis | Researcher: multi-source research and a structured, cited report | Preserve the incoming report and source ledger; enrich it without silently re-researching or rewriting supported claims |
| Quantitative work | Analyst: statistics, trends, outliers, charts/tables; Microsoft says Analyst runs inspectable Python | Run a separately recorded R analysis stage over explicitly selected data; expose the script and results instead of describing prose as “computed” |
| Generic agent execution | Code Interpreter: sandboxed generated Python with downloadable outputs | Do not claim parity with Microsoft’s Python sandbox; provide a deterministic R path with the package’s own runtime and artifact policy |
| Citation | Researcher cites sources; Microsoft also tells users to review grounding and critical details | Retain source citations and add provenance for every derived statistic/table/figure; flag unknown or unsupported source IDs |

## Requirements for Tempest enrichment

### 1. Keep the existing report as the source of truth

The enrichment input is an existing report plus its source/evidence records and
zero or more attached data files. The default operation appends or inserts an
explicit “Reproducible analysis” section; it must not discard original prose,
source markers, caveats, or unresolved questions. If no analyzable data is
present, return the original report unchanged and explain why enrichment was
not run.

The user must be able to choose the report, data files, and analysis question.
If a variable, unit, time window, grouping, or missing-value rule is ambiguous,
Tempest asks for clarification or records the assumption prominently; it must
not silently infer a scientific meaning from a column name.

### 2. Make R analysis reproducible and inspectable

Each run creates an analysis artifact containing, at minimum:

- the exact R script or generated R code;
- input file names and content hashes (or a local immutable fixture id);
- analysis parameters, seed, package versions, and `sessionInfo()`;
- stdout, warnings, errors, elapsed time, and an explicit success/failure
  status;
- result tables and data-derived figures, with stable artifact ids.

The analysis runner must be deterministic for the same inputs, code,
parameters, and seed. It must work in tests without API keys, network access,
or live provider responses. A failed or unvalidated computation is never
rendered as a successful numeric result.

### 3. Attach provenance to derived claims

Every derived number, table, and data figure in the enriched report links to:

1. the source/data record(s) it used;
2. the analysis-run id and code artifact;
3. the relevant method, parameters, units, and caveats.

Original Researcher citations remain source citations. They must not be
relabelled as proof that a new R calculation is correct. Conversely, a chart
computed from an attached CSV should cite the CSV and analysis artifact even if
the surrounding narrative came from a web source. Missing source ids and
unresolvable citations remain visible as warnings, not dropped references.

Microsoft’s grounding guidance says citations improve relevance and provide a
way to verify, but users should still review sources and confirm critical
details. Tempest should adopt that validation posture rather than presenting a
linked citation as a guarantee of correctness. [Microsoft Support: What
information Copilot uses](https://support.microsoft.com/en-us/microsoft-365-copilot/what-information-does-copilot-use-to-answer-your-prompt)

### 4. Separate data figures from illustrative images

Data figures are generated by R from the selected inputs and must carry a
caption, method, units, source/data ids, and an accessible description.

Illustrative images are optional explanatory artwork. They must be labeled
“AI-generated illustration” (or equivalent), must not encode an unverified
measurement or imply that they are evidence, and must record the provider/model,
prompt, generation time, and returned artifact/provenance metadata when the
provider supplies it.

Microsoft’s image-generator capability is a separate feature: it uses Designer,
generates four candidate images per prompt, and lets users download, copy, and
inspect content credentials. This supports offering a separate illustration
step, not substituting generated artwork for a statistical plot. [Microsoft
Learn: Image Generator capability](https://learn.microsoft.com/en-us/microsoft-365/copilot/extensibility/image-generator)
Microsoft also documents C2PA-based content credentials for AI-generated images
in Copilot. Tempest must not claim to provide C2PA credentials unless the actual
image provider and artifact path return and preserve them. [Microsoft Support:
Copilot Chat FAQ](https://support.microsoft.com/en-us/microsoft-365-copilot/frequently-asked-questions-about-microsoft-365-copilot-chat)

### 5. Fail closed and preserve the original

If parsing, validation, R execution, or image generation fails, the report still
opens with the original content and citations. The enrichment surface shows a
specific status (`not_run`, `failed`, or `succeeded`) and human-readable reason;
it does not invent replacement values, a chart, a source, or a claim that an
analysis completed. The user can retry after correcting the input.

## Acceptance criteria

The implementation is acceptable when all of the following are demonstrated by
deterministic local tests and a visible report/artifact inspection:

1. **Baseline preservation.** A fixture containing a Researcher-style report,
   repeated citations, and an unknown citation id returns the original report
   text and known source links intact; the unknown id remains explicitly
   flagged.
2. **Explicit scope.** The user can select the report and data inputs and can
   see the analysis question, assumptions, and requested output before the R
   run starts.
3. **Real R execution.** A small fixture dataset produces a known statistic,
   table, and plot through R. The report shows the result only after the run is
   successful, and the code/log/input-hash/session-info artifacts are available
   for inspection or download.
4. **Replay.** Re-running with the same fixture, code, parameters, and seed
   reproduces the same canonical result values and equivalent figure output;
   changing an input or parameter changes the run identity and is visible in the
   provenance.
5. **Derived-citation integrity.** Every added number, table, and data plot
   points to source/data ids plus the analysis-run artifact. The report clearly
   distinguishes source-supported statements from Tempest-derived calculations.
6. **Failure behavior.** Missing data, ambiguous mappings, malformed structured
   output, an unknown source id, and an R error each leave the original report
   available and produce an actionable warning; none produces fabricated
   results.
7. **Illustration boundary.** An optional generated image is displayed and
   exported only with an AI-generated-illustration label, prompt/provider
   metadata, and alt text. It is not used as the data figure or as evidence for
   a claim. Image-generation failure does not fail a successful R analysis.
8. **Offline quality gate.** The focused tests run with fake chats/local
   stores/fixtures and no API key, network request, or live provider response.
   The report path and the artifact path are both tested; a green test alone is
   not the acceptance evidence.
9. **Honest comparison language.** Documentation and UI do not say that Tempest
   is “better than Researcher,” “equivalent to Analyst,” or “Microsoft-compatible.”
   Any comparison is limited to the documented role distinction above, and any
   observed Tempest behavior is labeled as an implementation result rather than
   a Microsoft product guarantee.

## Comparison limits and non-goals

- Microsoft’s pages describe intended capabilities and workflows, not a
  head-to-head accuracy, reproducibility, or domain-science benchmark. Tempest
  must not claim superiority from the existence of an R script, a chart, or a
  longer report.
- The Researcher documentation establishes structured reports, source
  citations, clarifying questions, and possible visuals. It does not establish
  that Researcher executes R/Python for every request. The Analyst announcement
  establishes inspectable Python execution for Analyst; it does not establish
  that the Python and R environments, packages, prompts, or results are
  interchangeable.
- A citation is a path to source material, not proof that a sentence is
  supported. A generated visual is not proof of the underlying data. Tempest’s
  evidence ledger and analysis artifacts must make those distinctions visible.
- Microsoft availability, licensing, tenant controls, source permissions, model
  choice, and usage limits can change. Requirements should depend on the local
  Tempest contract and deterministic tests, not on an unverified assumption that
  a particular Copilot feature is enabled.
- Generated images are illustrative and probabilistic. They are not a
  replacement for reproducible R figures, source excerpts, or scientific
  evidence.

## Primary sources consulted

- [Microsoft Support — Get started with Researcher in Microsoft Copilot](https://support.microsoft.com/en-us/microsoft-365-copilot/get-started-with-researcher-in-microsoft-365-copilot)
- [Microsoft Learn — Get started with Researcher agent](https://learn.microsoft.com/en-us/copilot/manage/researcher-agent)
- [Microsoft Learn — Researcher agent FAQ](https://learn.microsoft.com/en-us/copilot/manage/faq-researcher)
- [Microsoft Support — Get started with Analyst in Microsoft Copilot](https://support.microsoft.com/en-us/microsoft-365-copilot/get-started-with-analyst-in-microsoft-365-copilot)
- [Microsoft — Researcher and Analyst generally available](https://www.microsoft.com/en-us/copilot/blog/2025/06/02/researcher-and-analyst-are-now-generally-available-in-microsoft-365-copilot/)
- [Microsoft Learn — Code Interpreter capability](https://learn.microsoft.com/en-us/microsoft-365/copilot/extensibility/code-interpreter)
- [Microsoft Learn — Image Generator capability](https://learn.microsoft.com/en-us/microsoft-365/copilot/extensibility/image-generator)
- [Microsoft Support — What information Copilot uses](https://support.microsoft.com/en-us/microsoft-365-copilot/what-information-does-copilot-use-to-answer-your-prompt)
- [Microsoft Support — Copilot Chat FAQ](https://support.microsoft.com/en-us/microsoft-365-copilot/frequently-asked-questions-about-microsoft-365-copilot-chat)
