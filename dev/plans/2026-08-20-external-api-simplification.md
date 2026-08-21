# Tempest 0.3 product-surface reset

Tracking issue: kata `pjsd`

Status: PR B verified; PR C implemented and locally verified, publication pending

Audit basis: Tempest `main` commit
`21e1794fa9695f383c4b31d413d2c40b58a15ac7` on 2026-08-20

## Goal

Make Tempest one focused research product package rather than a product, an
extension SDK, a workflow compiler front end, an evaluation toolkit, and a
host framework in one namespace.

Tempest is pre-production. The 0.3 line should therefore make one clean break:

- accept only the current public names, inputs, and stored formats;
- provide no deprecated aliases, forwarding wrappers, dual representations,
  old-schema readers, or always-rejected compatibility arguments;
- expose only actions a product user or an acceptance host must perform; and
- keep implementation objects, bundled-app seams, callback machinery, and
  package-to-package adapters internal.

The recommended target is **19 exported functions**, down from 63. The target
keeps STORM, Co-STORM, accepted organizational knowledge, evidence inspection,
session persistence, trajectory review, the bundled app, and the complete
Graft proposal-to-receipt authority chain.

This is a breaking 0.3 redesign. It is not a 0.2 patch and it does not add a
migration layer.

## Product decision

The namespace should describe the Tempest product, not every mechanism used to
build it. A name earns an export only when all of these are true:

1. A package user or acceptance host must call it directly.
2. A retained function or returned object cannot express the same operation
   more coherently.
3. The capability belongs in Tempest rather than Deputy, dsprrr, Graft, vitals,
   btw, or the bundled Shiny implementation.
4. Its arguments and return value form a contract we are willing to maintain.
5. It has source-mode and installed-package contract tests.

Export count is not the only surface metric. The plan also freezes public
formals, returned-object fields, R6 methods and bindings, callback records,
condition classes, and Shiny handles. Moving 40 functions onto an R6 object
would not be simplification.

| Surface | Current | Target |
|---|---:|---:|
| Namespace exports | 63 | 19 |
| Public R6 class generators | 3 | 0 |
| TempestSession public methods | 23 | 6 |
| TempestSession active bindings | 12 | 6 |
| Progress helper exports | 7 | 0 |
| Shiny module/store exports | 3 | 0 |
| Registered S3 methods | 2 | 1 |

The one retained S3 registration is `print.tempest_knowledge`. Both low-level
OKF print registrations disappear with the OKF subsystem.

## Target API — 19 exports

### Research product — 13

```text
tempest_app
tempest_claim_supports
tempest_claims
tempest_config
tempest_expert
tempest_knowledge
tempest_report
tempest_run
tempest_session
tempest_session_resume
tempest_session_save
tempest_sources
tempest_trajectory_review
```

The primary journeys are intentionally short:

```r
cfg <- tempest_config()
result <- tempest_run("Grid-scale battery recycling", config = cfg)
tempest_report(result)

session <- tempest_session("Grid-scale battery recycling", config = cfg)
session$warmup()
session$step("Compare the strongest policy evidence.")
tempest_session_save(session, "research/battery-recycling")

view <- graft::graft_at(store, graft::graft_snapshot(store))
records <- graft::graft_find(view, "battery recycling", limit = 25)
knowledge <- tempest_knowledge(view, record_ids = records$id)
result <- tempest_run(
  "Grid-scale battery recycling",
  config = cfg,
  knowledge = knowledge
)
```

`tempest_knowledge()` remains because bringing accepted organizational
knowledge into a run is a product capability, not an extension-SDK detail. It
is the one strict constructor for a pinned Graft view, an exact accepted-record
allowlist, and optional exact stage-to-governed-procedure record bindings
validated against that view.

Tempest does not parse OKF or any other external knowledge format. External
formats are normalized on the Graft side into a proposal, reviewed, explicitly
committed, and only then exposed to Tempest through an immutable `GraftView`.
The value keeps accepted evidence access separate from executable authority:
ordinary accepted records cannot grant tools, policy, or code, and a governed
procedure is executable only through an explicit stage binding.

### Explicit acceptance authority — 6

```text
tempest_graft_plan
tempest_graft_schema
tempest_promotion_bundle
tempest_promotion_receipt
tempest_read_promotion_bundle
tempest_save_promotion_bundle
```

Do not collapse this chain into `tempest_promote()`. Proposal construction,
durable review, host-owned Graft commit, and receipt verification are distinct
authority transitions. Making them explicit is a safety property, not API
sprawl.

## Exact 0.2-to-0.3 transition inventory

The authoritative transition inventory is
[`tests/testthat/fixtures/public-api-transition-0.3.csv`](../../tests/testthat/fixtures/public-api-transition-0.3.csv).
It maps each of the 63 former exports exactly once to one of four dispositions:
retain, replace, internalize, or delete. The accompanying contract test verifies
the row count, uniqueness, disposition rules, and these 19 unique target names:

```text
tempest_app
tempest_claim_supports
tempest_claims
tempest_config
tempest_expert
tempest_graft_plan
tempest_graft_schema
tempest_knowledge
tempest_promotion_bundle
tempest_promotion_receipt
tempest_read_promotion_bundle
tempest_report
tempest_run
tempest_save_promotion_bundle
tempest_session
tempest_session_resume
tempest_session_save
tempest_sources
tempest_trajectory_review
```

The inventory is a design assertion for the full stack. The separate
`public-api-current` fixture records the exact namespace at each intermediate
PR, so PR A does not pretend that internalization planned for PR E is already
complete.

## Cohesive replacements

The hard cut introduces three names and removes the replaced names
immediately:

| New contract | Replaces | Reason |
|---|---|---|
| `tempest_app()` | `run_app()` | Every Tempest entry point should be visibly namespaced. |
| `tempest_report(x)` | `tempest_report_md()` and `tempest_session_report_md()` | One read operation should work for a completed run or session. |
| `tempest_knowledge()` | Direct `knowledge_view` arguments plus public governed-procedure/ProgramSet assembly | One strict product input preserves pinned accepted evidence and accepted-procedure authority without exposing compiler or workspace internals. |

No deprecated aliases remain. The old names disappear from `NAMESPACE`, help,
pkgdown, examples, tests, and shipped assets in the same change.

The four OKF exports and two OKF print methods are deleted without replacement.
They are not compatibility aliases for `tempest_knowledge()`; pointing Tempest
at an arbitrary document directory is no longer a product capability.

Progress callbacks should receive a canonical plain named record. Hosts can
store the record directly and reduce it with ordinary R code. They should not
need public event constructors, decoders, collectors, replay helpers, or label
tables.

The callback record has exactly these fields, in order:

```text
event_id
run_id
workflow
event_type
stage
step
status
timestamp
message
payload
parent_event_id
correlation_id
```

It contains only canonical plain R values. Payloads carry bounded metadata and
references, never raw prompts, responses, source bodies, URLs with query
strings, or credentials.

## Public signatures

The product entry points should expose product choices, not internal stage or
worker controls.

```r
tempest_run(
  topic,
  config = tempest_config(),
  n_experts = 3,
  experts = NULL,
  research_strategy = c("key_questions", "conversation"),
  knowledge = NULL,
  output_dir = NULL,
  resume = FALSE,
  progress = NULL
)

tempest_session(
  topic,
  config = tempest_config(),
  n_experts = 3,
  experts = NULL,
  knowledge = NULL,
  progress = NULL
)

tempest_expert(
  name,
  title,
  description,
  instructions,
  focus_areas = character(),
  initial_questions = character()
)

tempest_knowledge(
  graft_view,
  record_ids = character(),
  governed_procedures = NULL
)

tempest_session_resume(
  path,
  config = tempest_config(),
  knowledge = NULL,
  progress = NULL
)
```

Exact expert identity and version should be derived from the canonical
profile. The model role is fixed to `expert`; a new profile is active, and
retirement belongs to the session roster. Generated experts use the same
internal constructor and preserve generated `initial_questions`. Selection
provenance belongs in the generation StageRecord/manifest, not generic profile
metadata. Schema version, capability identifiers, model-policy references,
selection metadata, and arbitrary metadata are package-owned details or are
deleted.

`knowledge` accepts exactly `NULL` or a value returned by
`tempest_knowledge()`. It is not a generic list, raw store, snapshot, filesystem
path, or authority collapse. `graft_view` must be a live pinned `GraftView`;
mutable stores and path-free snapshots alone are rejected.

`record_ids` is a unique character vector of exact accepted record IDs, capped
at 1,000 entries. Users discover and filter those records with Graft before
building the value; Tempest does not duplicate Graft's query language with
class, query, trust, or lifecycle selectors. All research reads are confined to
the allowlist, plus bounded claims, evidence, and source records directly
joined to an allowed record. An empty allowlist is valid only when at least one
governed procedure is bound; otherwise callers should pass `knowledge = NULL`.
Construction canonicalizes the allowlist, verifies that every ID resolves to a
public record at the pinned snapshot, and records its digest. Record content is
materialized only if the workflow consumes that record.

Tempest uses only Graft's bounded public read operations against the pinned
view and immutable allowlist. Each accepted record actually consumed by the
research workflow is projected into a `graft.record` Tempest resource with
exact store, snapshot, record, revision, schema, and content identity. Only
public text-bearing records or joined accepted claim/evidence packets are
citeable. A record with no usable public text or evidence excerpt is rejected
as research evidence rather than being represented by its title or locator.
Ordinary Tempest claim-support verification still applies.
Any truncated top-level read or related claim, evidence, or source join is
rejected before its content reaches a provider; Tempest never presents an
incomplete accepted-evidence packet as complete.

Accepted text is inserted only through the delimited evidence/data channel. It
never enters system or developer instructions, tool definitions, execution
policy, or governed-procedure selection. Instruction-shaped record content is
quoted as untrusted evidence and cannot change the available tools or message
roles.

`governed_procedures` is either `NULL` or an exact named character vector of
Tempest stage to accepted Graft `GovernedProcedure` record ID. Every binding is
resolved against the pinned view when the knowledge value is built and again
before execution. Tempest never discovers executable procedures from ordinary
knowledge reads. Unknown stages, duplicate bindings, cross-view records, and
unresolvable records fail closed.
Each accepted procedure must reference the exact current builtin dsprrr
artifact, evaluator, and contract for that Tempest stage. A Graft
`ProgramArtifact` record proves identity but does not carry executable code;
accepted custom artifacts are therefore not executable through the 19-name
product API after the compiler/load SDK is removed.

Consumed Graft resource projections are durable workspace evidence and are
restored only from the product bundle. The live Graft view, store, connection,
and tool definitions are never persisted. An unfinished session or STORM run
must be resumed with a live `tempest_knowledge()` value whose snapshot identity,
record-allowlist digest, and governed-binding digest exactly match the persisted
identity; a newer snapshot from the same store still fails before provider
work. A succeeded immutable product may be resumed for inspection without a
live view. The same rules apply to `tempest_run(resume = TRUE)`.

`tempest_config()` remains the one explicit configuration constructor. Audit
each formal, retain real model, retrieval, budget, caching, and evidence-policy
choices, and remove aliases or duplicate controls. Do not replace explicit
arguments with an unvalidated generic options bag merely to make the signature
look shorter. Research limits such as maximum rounds and questions belong in
that validated configuration rather than the run function.

`output_dir` is the exact bundle directory, not a parent under which Tempest
derives a topic/run directory. A fresh run requires that path to be absent and
never overwrites it; `resume = TRUE` reads that exact path. Product identity is
generated and stored inside the bundle. This removes `run_id` without making
same-topic runs ambiguous.

## Returned-object surface

### Completed STORM result

`tempest_run()` should return one internal validated product class rather than
a public loose list contract. The supported read surface is the retained
accessors:

- `tempest_report()`;
- `tempest_claims()`;
- `tempest_claim_supports()`;
- `tempest_sources()`;
- `tempest_trajectory_review()`; and
- the promotion functions.

Their accepted values are exact:

| Function | Accepted value |
|---|---|
| `tempest_sources()` | completed run or session |
| `tempest_claims()` | completed run or session |
| `tempest_claim_supports()` | completed run or session |
| `tempest_report()` | completed run or finalized session |
| `tempest_trajectory_review()` | completed run or finalized session |
| `tempest_promotion_bundle()` | completed run or finalized session |

The remaining promotion functions consume the exact bundle, path, Graft store,
reviewed plan, or commit result named in their authority step.

`tempest_claim_supports()` must return a complete, resolvable evidence table,
not only foreign keys. Each row contains the support identity and judgment,
claim identity and text, exact evidence-span identity and quote/offset/page,
and source identity. `tempest_sources()` supplies the corresponding source
metadata and locator. This preserves the full claim -> support -> span ->
source inspection chain without exporting span/workspace constructors.

Do not promise direct access to the retriever, mutable workspace, internal
state machine, stage cache, or persistence implementation. Those values remain
available internally to validation, persistence, telemetry, and promotion.

### Co-STORM session

Reduce `TempestSession` from 23 public methods and 12 active bindings to the
small interactive contract:

- operations: `warmup()`, `step()`, `suggest_questions()`, `add_expert()`,
  `retire_expert()`, and `finalize()`;
- read-only projections: `session_id`, `topic`, `status`, `experts`,
  `transcript`, and `mindmap`.

`finalize()` replaces the current mutating `report()` method: it verifies
claims, creates and commits the report, moves the manifest to `succeeded`, and
seals the workspace. `tempest_report()` is the read-only accessor after
finalization. Evidence access, trajectory review, save, and resume use the
top-level product functions. Completion claiming, event emission, Deputy
routing, mind-map maintenance, workspace mutation, manifest mutation, and
async scheduling become private.

Its exact product choices remain public:

```r
session$finalize(
  style = c("technical", "executive"),
  include_references = TRUE
)
```

### Internal product objects

`ResearchWorkspace`, `TempestRetriever`, product manifests, resources,
ProgramSets, progress collectors, Shiny stores, and validation values are not
public construction or subclassing seams. If a future external integration
needs one, it must propose a narrow interface with a real consumer instead of
re-exporting the implementation class.

## Supported condition contract

The current implementation has many specific internal error classes. The 0.3
public contract should guarantee only these catchable categories:

```text
tempest_error
tempest_input_error
tempest_execution_error
tempest_persistence_error
tempest_authority_error
tempest_cancelled
```

Every user-facing failure inherits from `tempest_error` and exactly one more
specific public category. Internal subclasses may remain for package tests and
diagnostics but are not part of the API fixture. Original provider and package
conditions remain available through the ordinary parent condition chain; raw
prompts, responses, credentials, and source bodies never enter messages or
condition metadata.

## Capability disposition

The 19-export target deliberately removes three SDK-like capability groups.
This is the substantive product choice behind the reduction.

### Internalize in Tempest

- Workspace, resource, retriever, manifest, ProgramSet, governed-procedure,
  and progress construction needed by Tempest's own workflows.
- STORM and Co-STORM async machinery used by the bundled app.
- Shiny UI/server/store modules used by `tempest_app()`.
- Bounded Graft-view retrieval and accepted claim/evidence projection used by
  `tempest_knowledge()`.
- Claim verification and report rendering used by the product pipeline.

### Delete from Tempest

- Standalone evaluation constructors and `SimulatedUser`. If reusable product
  evaluation becomes a maintained deliverable, it belongs in a focused
  companion package built on the 19-name product contract.
- Public compiler/save/load helpers for Tempest ProgramSets. dsprrr remains the
  program implementation dependency; Tempest owns and validates the program
  set used by its product.
- Public custom-workspace, custom-resource, and retriever-subclass SDKs. Search
  provider and model choices remain supported through `tempest_config()`.
- Public async-host, progress-framework, and embeddable-Shiny module APIs. The
  bundled app remains supported through `tempest_app()`.
- The complete Tempest OKF subsystem: parser, bundle/context classes, trust and
  staleness heuristics, selectors, resource conversion, fixtures, tests,
  vignette, help, print methods, and the now-unused `yaml` dependency. Direct
  provisional ingestion of arbitrary OKF directories is intentionally retired.
- Both bundled Agent Skills and their R installer/discovery wrappers. `btw`
  already discovers package skills; the generic `conduct-storm-research`
  protocol is not Tempest runtime functionality. Reintroduce agent guidance
  later only as independently maintained documentation or a companion asset
  package.

### Keep outside Tempest's API

- Deputy Agent execution, hooks, permissions, limits, cancellation, and run
  contexts. These are real Co-STORM runtime capabilities.
- dsprrr ProgramArtifact identity and execution.
- Graft's host-owned `graft_commit()` acceptance operation.
- External-format compatibility. An OKF or future adapter belongs on the Graft
  side and may create only a reviewable proposal; import never implies
  acceptance. Graft's existing managed OKF working tree remains a projection
  of accepted revisions, not a Tempest runtime input.
- ellmer Codex authentication and provider clients.
- shinychat's current chat UI contract.

## Backward-compatibility hard cut

### Remove immediately

1. **Always-rejected arguments**
   - Remove `parallel_research` and `remove_duplicate` from
     `tempest_run()` and internal call chains.
   - Remove public `schema_version` arguments from single-version
     constructors.

2. **Permissive aliases**
   - Accept only canonical search-provider names; remove aliases such as
     `ddg`, `google_search`, `azure`, and punctuation variants.
   - Require canonical `anthropic/...` and `google/...` model prefixes rather
     than `claude/...` and `gemini/...`.
   - Require `AZURE_AI_SEARCH_ENDPOINT`; remove
     `AZURE_AI_SEARCH_URL` fallback.
   - Remove transcript, fact-output, and score-field aliases where current
     records already have one exact shape.

3. **Dead normalizers and coercion**
   - Delete unused query/fact normalization helpers and the unused stage fact
     source helper.
   - Replace permissive character coercion with exact current suggested-
     question validation.

4. **Legacy evidence representation**
   - Make `TempestResource` the only internal evidence-resource record.
   - Make retrievers and tools construct it directly.
   - Require it at workspace insertion.
   - Delete the old source-list constructor, validator, identity fallback, and
     source-to-resource adapter.
   - Retain source-shaped read projections only where reports, citations, or
     the UI need that presentation.

5. **Generic expert residue**
   - Remove `skill_ids`, `skill_versions`, `required_capability_ids`,
     `optional_capability_ids`, `model_policy_ref`, and
     `initial_work_items`.
   - Regenerate current expert fingerprints and affected current product
     formats once. Do not add a reader for the prior shape.

6. **Deputy Agent SDK compatibility**
   - Delete the sole `agent$configure_sdk_compat()` call.
   - Record Tempest's own `deputy_session_id` directly in pending and terminal
     records instead of consulting a compatibility hook context.
   - Keep native Deputy Agent execution and correlation. Tempest has no public
     Claude/Anthropic Agent SDK facade to preserve.

7. **Historical surface machinery**
   - Delete the 0.1 and 0.2 export fixtures and tests that freeze removed
     names.
   - Keep history in Git and release notes, not as executable test policy.
   - Replace them with one authoritative current API fixture that includes
     exports, formals, result accessors, session methods/bindings, condition
     classes, and callback fields.

8. **Optional recovery and standalone conveniences**
   - Remove `partial_recovery`; session resume either validates the exact
     current bundle or fails closed.
   - Remove session-free `tempest_suggest_questions()` while retaining the
     session's `suggest_questions()` operation.

### Retain: these are not compatibility code

- Exact current-version and exact-field validation for persisted data.
  Accepting one version and rejecting every other value is integrity, not a
  migration reader.
- Immutable Graft historical reads and accepted-revision validation.
- Stage fallbacks, native-search fallback, mirai sequential fallback, and
  keyword retrieval fallback. These are current execution-resilience policy
  and remain visible in provenance.
- Cross-provider response adapters for real provider protocol differences.
- `/var`/`private/var` and `/tmp`/`private/tmp` canonicalization on macOS.
- Current shinychat signature checks and Shiny session-lifetime guards.
- Validation, authority, privacy, cancellation, and telemetry fail-closed
  behavior.

## Current-format policy

There is one writer and one reader for each persisted product format. When the
expert and product shapes change:

- assign one new current version where a stored shape actually changes;
- update the writer, reader, fixtures, and validators together;
- reject all other versions with one generic wrong-version test;
- do not retain old field maps, default insertion, coercion, or migration
  helpers; and
- do not reset version numbers merely for cosmetic cleanliness.

Old bundles remain recoverable from the tagged 0.2.0 package and Git history.
The 0.3 package does not read them.

## Implementation sequence

Use stacked PRs. Each PR should remove one concept class and leave the stack
tip releasable; no PR adds compatibility wrappers for a later PR to remove.

### PR A — Freeze the 0.3 contract and remove shallow compatibility

- [x] Add a transition inventory that maps each of the current 63 exports
  exactly once to retain, replace, internalize, or delete, and names the final
  19-name target. It is a design assertion, not a premature namespace check.
- [x] Replace old versioned export fixtures with one `public-api-current`
  fixture. It matches the namespace and other supported surfaces after each PR;
  only PR E reaches the final 19-name assertion.
- [x] Remove always-rejected arguments, public schema-version knobs, provider
  and environment aliases, dead normalizers, permissive coercion, and the
  redundant transcript fallback.
- [x] Remove the Deputy SDK-compat call and make Tempest session identity the
  only recorder input.
- [x] Delete Agent Skill exports, implementation, assets, vignette, snapshots,
  and pkgdown entries.
- [x] Delete the complete Tempest OKF subsystem and remove `yaml` from Imports;
  delete `partial_recovery` and the standalone suggestion API.
- [x] Record the breaking 0.3 reset in `NEWS.md`; do not add a compatibility
  table or deprecation aliases.

Gate: canonical inputs behave exactly; removed inputs fail as unknown or
invalid; Deputy correlation remains exact; no stored product shape changes.

### PR B — Use one typed evidence representation

- [x] Make every search, fetch, tool, and restore producer construct a
  `TempestResource` directly.
- [x] Make workspace insertion and identity strict.
- [x] Delete the legacy source constructor, validator, conversion adapter, and
  dual-input branches.
- [x] Keep only intentional source-shaped read projections used by retained
  product accessors and presentation.
- [x] Update focused retrieval, workspace, citation, report, Shiny, and
  persistence tests.

Gate: the current workspace format still stores typed resources; every product
baseline is deterministic; no executable legacy source path remains.

### PR C — Reset the expert and product schemas

- [x] Reduce `tempest_expert()` to human-authored profile fields.
- [x] Remove generic capability, skill, policy, and duplicate work-item fields
  from the S7 value, runtime projection, persistence, prompts, and tests.
- [x] Derive expert identity from canonical content.
- [x] Assign new current versions only to affected stored formats and rebuild
  their exact fixtures.
- [x] Bump the expert record from 1 to 2, STORM state from 4 to 5, STORM bundle
  from 7 to 8, and Co-STORM snapshot/bundle from 9 to 10.
- [x] Audit StageRecord output-digest schema 3 against the reduced persona
  output; bump it only if the digest payload shape, rather than merely its
  value, changes.
- [x] Delete all prior-shape readers and field defaulting.

Gate: selected/generated rosters, Deputy identities, save/resume, STORM,
Co-STORM, and promotion remain deterministic under the new current format.

### PR D — Introduce the cohesive product boundary

- [ ] Add `tempest_app()` and delete `run_app()`.
- [ ] Add `tempest_report()` and delete both specialized report exports.
- [ ] Add the Graft-only `tempest_knowledge()` boundary and remove direct
  `knowledge_view` arguments from public entry points.
- [ ] Expose only bounded public reads against the pinned view; project every
  consumed accepted record or joined claim/evidence packet into an exact
  `graft.record` resource and reject non-materializable records.
- [ ] Enforce the immutable record allowlist on every read, reject truncated
  packets, and cover exact-limit and over-limit joins.
- [ ] Keep accepted text in a delimited evidence/data channel and prove hostile
  instruction-shaped records cannot alter prompts, message roles, tools,
  governed-procedure selection, or executable artifacts.
- [ ] Make `tempest_run()` return one validated internal product class.
- [ ] Make retained evidence, review, persistence, and promotion functions work
  for that result and for a session where appropriate.
- [ ] Make `tempest_sources()` inspect consumed accepted knowledge and product
  evidence, and make `tempest_claim_supports()` return the complete joined
  proof rows.
- [ ] Let `tempest_run()`, `tempest_session()`, and
  `tempest_session_resume()` accept only the validated knowledge value;
  preserve the distinction between accepted evidence reads and explicit
  governed-procedure bindings.
- [ ] Emit the exact canonical plain progress record to callbacks and adapt the
  bundled app and telemetry without changing event order or privacy.
- [ ] Reduce the two public entry-point signatures to the agreed formals.
- [ ] Shrink `TempestSession` to the agreed operations and projections.
- [ ] Remove raw retriever, workspace, manifest, state, event, and config
  bindings from supported result/session surfaces.

Gate: the complete primary journey, current persistence, trajectory review,
and promotion use only the new product contract.

### PR E — Remove the SDK/toolkit namespaces

- [ ] Internalize product-required workspace, resource, retriever,
  ProgramSet, verification, progress, async, and Shiny implementations.
- [ ] Delete standalone extension, evaluation, compiler, Agent Skill, host,
  and convenience code that has no internal product caller.
- [ ] Reduce Shiny's external return handle to no public contract beyond
  `tempest_app()`.
- [ ] Normalize user-facing conditions to the six supported public categories
  while retaining original errors through parent conditions.
- [ ] Add source and stripped-installed tests that trigger every public
  condition category, prove exact inheritance, and prove the original parent
  condition is retained without sensitive metadata.
- [ ] Regenerate `NAMESPACE`, `man/`, `_pkgdown.yml`, README, vignettes, and
  installed examples for exactly 19 exports.
- [ ] Update the architecture boundary document with the current product API
  and make no 63-name compatibility claims.

Gate: source and fresh installed namespaces expose exactly the 19 names and no
unintended class generator, method, binding, or handle surface.

Every PR, not only the stack tip, must regenerate its own roxygen output,
`NAMESPACE`, help, pkgdown index, README/vignette references, current-version
tables, and exact current API fixture. Every PR runs focused tests, the full
suite, pkgdown, a fresh installed-mode check, and ordinary package check. A
temporarily stale public contract is not an acceptable intermediate state.

## Verification

### Contract gates

- [ ] The current API fixture matches source and stripped installed namespaces
  exactly once.
- [ ] Public-formals and object-surface fixtures match exactly.
- [ ] Static search finds no removed export, alias, old field, Agent Skill,
  SDK-compat call, legacy source constructor, or old-format branch in
  executable code or user-facing documentation.
- [ ] `devtools::document()` produces only intended namespace and help changes.
- [ ] `pkgdown::check_pkgdown()` reports no missing or unindexed topics.

### Product gates

- [ ] Deterministic STORM and Co-STORM product tests pass through only retained
  entry points and accessors.
- [ ] A pinned Graft view supplies bounded accepted knowledge to both workflows
  without exposing a workspace or resource constructor; every consumed record
  has exact revision provenance and citeable evidence content.
- [ ] Out-of-scope records and truncated joins fail before provider work;
  hostile accepted text remains quoted evidence and cannot alter instruction or
  execution authority.
- [ ] Session save/resume, report, evidence, trajectory review, and promotion
  pass under the one current format.
- [ ] Deputy session/run correlation remains exact without SDK compatibility.
- [ ] Graft proposal, review pin, host commit, and receipt verification remain
  separate and fail closed.
- [ ] Bundled desktop/mobile Shiny journeys pass through `tempest_app()`.
- [ ] Progress, cancellation, privacy, and telemetry do not change product
  results or persisted bytes.

### Release-quality gates

- [ ] Focused tests pass after each PR.
- [ ] The full parallel package suite passes with no failures, errors,
  warnings, or unexpected skips at the stack tip.
- [ ] Fresh source build, install-with-tests, installed suite, pkgdown, and
  ordinary `R CMD check` are clean.
- [ ] All tracked R files parse.
- [ ] `air format --check .` passes.
- [ ] `git diff --check` passes.
- [ ] No network access, credentials, or provider calls are required by tests.

## Risks and rollback

The main risk is not technical migration; it is retiring a capability that we
later decide belongs in the product. Local and public searches found no known
external consumers beyond an explicitly legacy example package, but private
or unindexed consumers are unknowable.

Mitigations:

- state each capability loss explicitly in the PR that removes it;
- keep one capability group per PR so it can be reverted independently;
- retain implementations internally only when the product uses them;
- use Git history or the 0.2.0 tag to recover removed experimental code;
- require a real consumer and narrow contract before re-exporting a seam; and
- extract evaluation, host, or compiler tooling into a companion package if it
  becomes a maintained product, rather than growing Tempest back into a
  platform namespace.

## Decision log

### 2026-08-20 — Treat 0.3 as a hard contract reset

**Decision:** Use no deprecation wrappers, old readers, aliases, or dual
representations.

**Rationale:** Tempest is not in production. Preserving unused experimental
shapes would make the first intentional public contract harder to learn and
harder to change.

### 2026-08-20 — Target a 19-name product API, not a 42-name toolkit floor

**Decision:** Keep product, knowledge-ingress, evidence, persistence, review,
app, and acceptance-authority operations. Internalize or delete extension,
compiler, evaluation, and host SDKs.

**Rationale:** A 42-name surface preserves nearly every capability but leaves
Tempest responsible for four products. The smaller target states what Tempest
is. Capability loss is explicit rather than hidden behind export-count games.

### 2026-08-20 — Make Graft the sole knowledge compatibility boundary

**Decision:** Delete every OKF parser, helper, class, fixture, document, and
dependency from Tempest. Retain `tempest_knowledge()` as a Graft-only value and
retain all six promotion/review/receipt operations.

**Rationale:** Tempest should research against accepted, revision-pinned
organizational knowledge rather than maintain a second provisional document
ingress path. External formats belong in Graft adapters that produce reviewable
proposals and never commit automatically. A Tempest-owned knowledge value still
keeps snapshot validation, consumed evidence provenance, and executable
procedure bindings out of every workflow signature.

### 2026-08-20 — Remove SDK compatibility, retain Deputy

**Decision:** Delete the one Deputy SDK-compat configuration call and both
Agent Skill APIs/assets. Keep native Deputy execution and Tempest-owned
correlation identity.

**Rationale:** SDK compatibility contributes no behavior Tempest consumes.
Deputy permissions, hooks, limits, run contexts, and cancellation remain core
Co-STORM runtime capabilities.

### 2026-08-20 — Treat strict current validation as integrity

**Decision:** Keep one exact current-format reader and reject every other
version.

**Rationale:** Removing backward compatibility does not mean accepting
ambiguous or malformed state. Fail-closed validation is part of the product
boundary.
