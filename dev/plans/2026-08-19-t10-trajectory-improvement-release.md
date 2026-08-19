# T10 trajectory review, improvement, and 0.2.0 release plan

Tracking issue: unavailable; the kata daemon did not start during planning

Status: in progress

## Goal

Add one bounded, read-only review of a completed STORM or Co-STORM product that
joins the exact Tempest, Deputy, dsprrr, and Graft identities already retained
by that product. Use the review inside the research UI and in an explicit
operator-controlled evaluation and program-improvement loop, then prepare the
verified package as tempest 0.2.0.

T10 completes the 0.2 migration. It does not create another runtime, event
store, optimizer, accepted-knowledge authority, or compatibility layer.

## Non-goals

- Do not persist, save, restore, migrate, or remotely fetch a joined
  trajectory. The review is a reconstructable in-memory projection.
- Do not infer content causation from `correlation_id`, event order, timestamps,
  shared stages, or adjacent records. Correlation is grouping evidence only.
- Do not copy Deputy prompts, responses, tool inputs or results, full events,
  Agents, sessions, or other live capabilities into Tempest state.
- Do not copy dsprrr trial logs or implement a Tempest optimizer, teleprompter,
  metric adapter, candidate frontier, retry loop, or automatic stage compiler.
- Do not copy the Graft revision ledger or let a snapshot, promotion bundle, or
  receipt grant acceptance beyond its existing exact authority.
- Do not add a universal review framework, generic Task wrapper, generic event
  model, or cross-package persistence schema.
- Do not automatically replace an active `TempestProgramSet`, publish a report,
  promote claims, commit Graft records, or mark a procedure governed.
- Do not change the existing eight product schema versions, their strict
  readers, or the current report and publication authority gates.
- Do not redesign shinychat, add another background worker, or expand the
  public Shiny store/server handles.
- Do not submit to CRAN, create a Git tag or GitHub release, or push a release
  without a separate explicit user request. T10 prepares the release commit and
  evidence only.

## Frozen contracts

### Product schemas and authority

Preserve these exact versions and validators:

| Contract | Version |
|---|---:|
| `ResearchWorkspace` snapshot | 5 |
| Co-STORM snapshot and bundle | 9 |
| STORM bundle | 7 |
| STORM product state | 4 |
| `TempestProgramSet` | 2 |
| `TempestResearchManifest` | 3 |
| StageRecord output-digest payload | 3 |
| Promotion bundle | 1 |

The trajectory review validates the completed product through the same sealed
Workspace, exact report reference, terminal StageRecords, Deputy identity,
Manifest/StageRecord program identity, Graft snapshot, and
publication-authority boundary as promotion. Co-STORM additionally validates
its retained live ProgramSet; a completed STORM result does not retain one. The
review grants no new authority.

The review may carry `schema_version = 1L` for its closed in-memory field
shape. That value versions a projection, not a serialized product schema.
There is no reader, writer, compatibility alias, or migration path for it.

### Package boundaries

- Deputy owns agent execution, run/session identity, delegation, and tool-event
  content. Tempest retains only credential-safe terminal identity references.
- dsprrr owns typed programs, compilation, evaluation primitives, metrics, and
  optimization. Tempest binds exact ProgramArtifact and evaluator identities
  and composes an all-or-nothing product ProgramSet.
- Graft owns immutable snapshots, plans, commits, accepted revisions, and
  historical reads. Tempest may display exact references but never reproduce
  the ledger or imply acceptance without a receipt.
- vitals owns Task, solver, scorer, metric, logging, and comparison machinery.
  Tempest supplies scientific product solvers and safe product metadata.
- Tempest owns scientific product composition, report authority, the joined
  read-only review, deterministic attention findings, and the research UI.

### Public namespace

Add exactly one export, `tempest_trajectory_review()`. Keep the internal
`TempestTrajectoryReview` S7 class unexported and add no S3 method. Replace the
public fixture with exactly 63 sorted unique exports and preserve the exact two
registered S3 methods.

Retain all existing public signatures except the intentional additions of
`program_set = NULL` and `knowledge_view = NULL` to `tempest_task()` and
`tempest_costorm_task()`, and the intentional expansion of their existing
`dataset` argument to accept one exact caller data frame. No old argument,
alternate name, or Task overload is retained as a compatibility seam.

Keep `tempest_shiny_store()` at exactly 13 members and
`tempest_shiny_server()` at exactly 10 returned members. `trajectory` is an
in-memory product projection; the internal panel key is `review`, not a new
host-state handle or export.

## Review contract

### Accepted input

`tempest_trajectory_review(
  research,
  promotion_bundle = NULL,
  promotion_receipt = NULL
)` accepts only:

- the exact completed result returned by `tempest_run()`; or
- a succeeded, quiescent `TempestSession` returned by `tempest_session()`.

It reuses a neutral completed-product context shared with promotion. It does
not accept a vitals Task, loose Manifest/Workspace/StageRecord tuple, ambient
Deputy or dsprrr object, live Graft view/store/plan, or free-form event list.

The optional promotion states are closed:

- neither value: no promotion lane;
- bundle only: exact proposed, review-only promotion;
- bundle plus receipt: exact accepted revisions at the receipt snapshot; and
- receipt without bundle: reject.

The bundle is recomputed from the completed product and its exact `claim_ids`.
The receipt must bind the same bundle id. Neither value is mutated.

### Returned projection

The internal S7 value has these exact ordered properties:

1. `schema_version`
2. `review_id`
3. `product`
4. `stages`
5. `agent_runs`
6. `programs`
7. `knowledge`
8. `evidence`
9. `joins`
10. `findings`

Every property contains only primitive, canonical, credential-safe values.
`review_id` is the deterministic digest of the complete closed projection
other than itself.

Every potentially unbounded collection (`stages`, `agent_runs`, `evidence`,
`joins`, and `findings`) is the exact record `total`, `retained`, `omitted`,
`digest`, and `items`. `digest` covers the complete canonical collection, while
`items` retains at most 250 deterministically ordered records. Nested stage
outputs and promotion selections retain only kind, count, and complete digest,
not unbounded identifier arrays. The `knowledge` lane applies the same envelope
to receipt `record_revisions`; accepted revision joins retain at most 250 ids
plus the complete count/digest. The fixed ten-entry `programs` lane remains a
closed complete record. Thus a review is bounded without making omitted state
invisible or changing identity when an omitted record changes.

The lanes are:

- `product`: exact run id, mode, status, config digest, and report reference;
- `stages`: ordered StageRecord summaries, times, output identities, fallback,
  execution path, support, and publication state;
- `agent_runs`: safe terminal Deputy trace identities and status only;
- `programs`: stage, ProgramArtifact, contract, evaluator, and optional
  governed-procedure references from the Manifest;
- `knowledge`: the exact input Graft snapshot and optional proposed/accepted
  promotion references;
- `evidence`: bounded resource, claim, evidence-span, claim-support, and
  dispute identities/counts without source content;
- `joins`: exact typed identity relations; and
- `findings`: deterministic structural attention flags and missing-optional-lane
  notices.

Each finding is the exact fixed record `code`, `severity`, `ref_type`, and
`ref_id`. The initial closed codes are `stage_failed`, `stage_cancelled`,
`fallback_taken`, `exploratory_execution`,
`support_unverified`, `publication_blocked`, and `unmatched_reference`.
Severity is one of `info`, `warning`, or `error`; it is derived from the fixed
code and never from model text.

Product progress events are deliberately outside the public review. Co-STORM's
public progress recorder remains mutable after success, and STORM callback
events are not retained in its completed product. Neither stream can alter the
review id, findings, or joins. The live Shiny module may render a whitelist of
its already captured progress fields beside the review as explicitly untrusted
observations, but it must not add them to the product projection or fabricate a
persisted history.

### Join semantics

Every join record has the exact fields `from_type`, `from_id`, `relation`,
`to_type`, `to_id`, and `proof`. Allowed relations are exactly `contains`,
`executed_as`, `correlated_with`, `read_from`, `proposed_as`, `accepted_as`, and
`parent_of`. `proof` is the exact record `kind`, `matched_fields`; `kind` is one
of `authority_validated`, `exact_identity`, or `correlation_only`.

Use only exact identities:

- product root: `research_run_id`;
- StageRecord: `attempt_id`, `trace_id`, and exact stage;
- dsprrr: `program_artifact_id`, plus contract/evaluator ids and versions;
- Deputy: the required exact `(deputy_run_id, deputy_session_id)` pair, plus the
  complete parent-agent/parent-run/delegation/tool-call tuple when present;
- correlation grouping: exact `correlation_id`, always
  `relation = "correlated_with"` and `proof.kind = "correlation_only"`;
- input knowledge: exact Graft snapshot id, store id, schema-build digest, and
  commit order; and
- accepted output: bundle id, receipt plan id/digest, batch/snapshot identity,
  and exact per-record revision ids.

Never emit `caused_by`, `produced_by`, or another causal relation. Existing
StageRecord output and claim/extraction bindings remain exact identity/content
bindings and use only the closed non-causal relations above.

## Ownership and sequencing

Complete the product-context and review contract before evaluation or UI work.
Complete all runtime and documentation gates while the package remains
`0.2.0.9000`; make the version/NEWS/license release commit last.

| Lane | Exclusive files while active |
|---|---|
| Contracts | New trajectory fixtures/tests, public fixture, schema and absence gates |
| Review core | `R/trajectory-review.R`, completed-product context owner, promotion callers, review tests |
| Evaluation loop | `R/evals.R`, `R/program-set.R`, ProgramSet integration tests, evaluation tests |
| Research UI | `R/app.R`, `R/shiny-adapter.R`, `inst/shiny/`, Shiny tests and styles |
| Docs/integration | roxygen, generated `man/`, README, vignette, skills, architecture, NEWS, pkgdown |
| Release | DESCRIPTION version, license files, release checks, and this plan |

One owner edits `DESCRIPTION`, NAMESPACE, the public fixture, generated docs,
NEWS, architecture, and this plan at a time.

## Batch 0: Freeze T10 boundaries

### Public and schema gates

- [ ] Update the public fixture to exactly 63 sorted unique exports, adding only
  `tempest_trajectory_review`, and retain exactly two S3 registrations.
- [ ] Reassert the exact eight frozen product schema versions and current field
  order without adding a trajectory persistence schema.
- [ ] Assert that no review lane contains a function, environment, external
  pointer, chat, Agent, tool, connection, live Graft view/store, Shiny reactive,
  provider payload, path, prompt, source content, or credential-like scalar.
- [ ] Add a static owner inventory for the review constructor, evaluation task
  additions, internal Shiny module, and final release metadata.

### Join and authority gates

- [ ] Freeze the exact review properties, lane fields, join fields, relation
  names, proof names, finding codes, and deterministic review digest.
- [ ] Assert that every emitted relation is backed by exact stored identities
  and that correlation ids can emit only
  `correlated_with/correlation_only`.
- [ ] Reject malformed products, running/failed products, unsealed Workspaces,
  invalid reports, running StageRecords, pending Deputy/completion/async work,
  changed Manifest/StageRecord program bindings, changed live Co-STORM
  ProgramSets, configs/snapshots, and failed publication authority.
- [ ] Assert proposed and accepted promotion joins, reject receipt-only input,
  and prove review never plans or commits Graft writes.

## Batch 1: Completed-product trajectory review

### Share the exact product context

- [ ] Move the strict completed STORM/Co-STORM product readers out of the
  promotion-specific namespace into one neutral internal owner.
- [ ] Reuse the neutral reader from both promotion and trajectory review, with
  classed boundary-specific errors and no relaxed validation path.
- [ ] Preserve promotion schema 1, public formals, claim closure, publication
  authority, and nonmutation byte-for-byte.

### Build the closed projection

- [ ] Add internal `TempestTrajectoryReview` validation and the one public
  `tempest_trajectory_review()` constructor.
- [ ] Project product, stage, terminal agent, Manifest program, knowledge,
  evidence,
  join, and finding lanes from exact current owners; do not reparse serialized
  bundles, consume mutable progress history, or query sibling packages for
  ambient state.
- [ ] Generate typed identity joins and deterministic attention findings for
  failure/cancellation, fallback, exploratory execution, unverified support,
  publication blocking, and unmatched optional explicit references.
- [ ] Sort all unordered collections canonically and prove equivalent product
  values yield identical review ids across fresh processes.
- [ ] Bound every variable-length lane to 250 retained records with exact
  total/omitted counts and a digest over the complete canonical lane; prove a
  change confined to omitted records still changes the review id.
- [ ] Verify that constructing or copying a review cannot mutate the product,
  Workspace, session, ProgramSet, promotion bundle, or receipt.

### Cross-package contract tests

- [ ] Test exact Deputy terminal run/session/parent/delegation/tool/correlation
  projection without prompt, response, event payload, or Agent leakage.
- [ ] Test exact dsprrr ProgramArtifact/contract/evaluator joins without raw
  module or trial-log leakage.
- [ ] Test exact input Graft snapshot and optional promotion receipt revision
  joins without a live store/view/plan or inferred acceptance.
- [ ] Add ecosystem gates against the exact DESCRIPTION pins for Deputy,
  dsprrr, and Graft.

## Batch 2: Explicit evaluation and improvement loop

### Evaluate an exact ProgramSet

- [ ] Let the existing `dataset` argument accept either the built-in `"qa"`
  smoke dataset or one exact data frame containing only `input`, `target`, and
  optional unique `id` columns. Reject missing/extra columns, duplicated ids,
  empty values, non-scalar cells, and non-canonical rows before execution.
- [ ] Bind a canonical dataset digest and the built-in/caller dataset kind in
  credential-safe solver metadata, and derive the Task name from that normalized
  identity. Do not describe the ten-row built-in QA smoke fixture as a
  scientific benchmark or stage-training set.
- [ ] Add `program_set = NULL` and `knowledge_view = NULL` to
  `tempest_task()` and `tempest_costorm_task()` and route them through the real
  product constructors exactly as `tempest_run()`/`tempest_session()` do.
- [ ] Scope versioned safe product metadata to the built-in Tempest solvers. If
  an injected solver is supplied, reject non-NULL `program_set` or
  `knowledge_view` because Tempest cannot prove the custom solver used them;
  align Co-STORM and STORM custom-solver wrapping without claiming product
  authority over arbitrary solver output.
- [ ] Reject invalid or mismatched ProgramSet/governed-view inputs before
  the affected stage's provider execution and preserve default behavior when
  both are omitted. Do not promise an all-stage preflight that the current
  governed-procedure contract does not provide.
- [ ] Tag Tempest solver metadata with one current internal metadata version and
  include only a bounded plain-list summary: review id/version, run/mode/config
  identity, report reference, knowledge snapshot id, the fixed ten program
  artifact/evaluator identities, and a fixed ten-stage structural summary of
  attempt/fallback/execution/support/publication/finding counts with a complete
  stage digest. Never place the S7 review object, complete lanes, joins,
  evidence ids, or source content in vitals samples/logs.
- [ ] Keep raw report text in the vitals `result` field only; do not duplicate it
  in solver metadata or review lanes.

### Keep improvement explicit

- [ ] Document and test the closed operator loop: evaluate a baseline ProgramSet
  against an explicit caller evaluation set with vitals, provide separate
  explicit stage-labelled train/validation data and
  dsprrr Teleprompters, compile a candidate with
  `tempest_compile_programs()`, evaluate the candidate with the same scientific
  Task/scorer, inspect scores plus safe trajectory metadata, and explicitly pass
  the chosen ProgramSet to a later product run.
- [ ] Prove the Task never synthesizes training data from prompts/responses or
  StageRecord digests, never invokes a dsprrr optimizer automatically, and
  never changes the caller's baseline ProgramSet.
- [ ] Isolate every selected baseline Module before passing it to
  `dsprrr::compile_module()` by cloning or artifact round-trip followed by exact
  identity verification. Compile only the isolated value and prove the
  original ProgramSet artifact ids remain unchanged after both successful and
  throwing/mutating Teleprompters.
- [ ] Prove a candidate ProgramSet is not active, governed, published,
  promoted, or accepted merely because it scores better.
- [ ] When compilation changes a selected stage's `program_artifact_id`, clear
  that stage's old governed-procedure reference in the candidate ProgramSet;
  preserve references only for unchanged artifact identities and for untouched
  stages. A changed candidate remains ungoverned until a separately accepted
  Graft procedure binds its exact new artifact.
- [ ] Require separate baseline and candidate Task instances and document that
  vitals Tasks are mutable; never reuse or reset one Task as both sides of an
  improvement comparison.
- [ ] Leave Task comparison, score aggregation, metrics, logs, uncertainty, and
  scorer explanations under public vitals APIs; do not add a polymorphic Task
  input to `tempest_trajectory_review()`.

## Batch 3: Bounded trajectory research UI

### Add the internal panel

- [ ] Add the internal panel key `review` with the user-facing title “Run
  review” to the panel choices and bundled app navigation, using the active
  Co-STORM session and a separate internal last-successful STORM product
  reactive keyed by run id. Starting, cancelling, or rejecting a later run must
  not erase the last valid product merely to satisfy current-run cleanup.
- [ ] Keep the exact 13-member store and 10-member public server return value;
  the STORM module may expose its latest product only to the internal adapter.
- [ ] Render a product/status summary, deterministic attention counts, bounded
  filters, authoritative StageRecord rows, separately labeled live progress
  observations, and a detail card for fixed opaque references.
- [ ] Display untimed Deputy references beneath an exact matched StageRecord or
  in a separate unlinked-reference section; never fabricate their chronological
  position from list order, stage name, or correlation id.
- [ ] Pair live STORM progress with the product review only in the view; do not
  alter the public review id, product result, or persistence state.

### Security, accessibility, and performance

- [ ] Render only fixed escaped review fields and opaque identifiers. Never
  render arbitrary payloads, prompts, responses, source content, paths,
  credentials, scorer explanations, or HTML from a model/provider.
- [ ] Bound the visible row count and keep projection synchronous over existing
  reactives; cap rendered rows at 250 with an omitted-row count, and do not add
  an ExtendedTask, polling loop, cache, or persisted UI state.
- [ ] Use text plus icons for status, keyboard-operable controls, stable focus,
  labeled tables/details, one polite atomic summary region, and alerts only for
  newly observed failure/cancellation states.
- [ ] Test blank, running, completed, tampered, failed, cancelled, and stale
  product transitions without overwriting an existing valid report/review.

## Batch 4: Documentation and integration

- [ ] Document the review as a non-authoritative, reconstructable projection;
  explain every lane, join proof, exclusion of mutable progress history, and
  correlation-only rule.
- [ ] Document the explicit vitals -> dsprrr compilation -> candidate vitals ->
  operator adoption loop, including data leakage and overfitting cautions.
- [ ] Update `tempest_task()`/`tempest_costorm_task()` examples for exact
  ProgramSet/knowledge-view evaluation without implying automatic improvement.
- [ ] Update README, the main vignette, current NEWS only, package architecture,
  supported research skill references, Shiny docs, and pkgdown reference text.
- [ ] Regenerate roxygen output, keep all public topics indexed, and prove no
  retired generic-kernel or pre-0.2 vocabulary returns.
- [ ] Change the GitHub Pages deploy action to clean the deployment so deleted
  0.1 reference pages cannot survive on `gh-pages`; retain serialized deploy
  concurrency and PR cancellation behavior.
- [ ] Split pkgdown build from deployment permissions so pull-request builds use
  read-only contents access and only the serialized deploy job can write.
- [ ] Require `vitals (>= 0.3.0)` in Suggests, matching the Task/sample metadata
  APIs exercised by the explicit evaluation loop.
- [ ] Exclude ignored rendered vignette HTML from source tarballs with
  `.Rbuildignore`, delete the obsolete shipped `inst/UPSTREAM_COMPARISON.md`,
  repair the dead README Co-STORM and mirai URLs, and audit the built tarball for
  stale generic-kernel or local build output.
- [ ] Resolve release documentation defects found by extrachecks: document the
  `tempest_session()` return value, normalize the internal
  `tempest_make_chat` title, and replace or encode problematic Rd punctuation.

## Batch 5: Release preparation and verification

### Static and focused gates

- [ ] Parse and format every changed R/test file; run `air format --check .`
  and `git diff --check`.
- [ ] Run trajectory, promotion, ProgramSet, evaluation, report authority,
  progress/event, Shiny, persistence, ecosystem, public API, and product-surface
  tests with exact pinned dependencies.
- [ ] Run deterministic fresh-process review-id checks and adversarial tests for
  malformed joins, credentials, mutation, causation labels, and cross-run
  product/promotion mixing.
- [ ] Run `devtools::document()` and `pkgdown::check_pkgdown()` and verify the
  generated namespace is exactly 63 exports/two S3 methods.

### Installed and full gates

- [ ] Build and install the source tarball with exact Deputy, dsprrr, Graft,
  ellmer, and shinychat revisions in an isolated library.
- [ ] Verify the installed namespace, review projection, evaluation candidate
  loop, internal Shiny panel, host example, assets, schemas, and retired-surface
  absence outside the checkout.
- [ ] Run the complete test suite unsandboxed with four workers and report exact
  pass/fail/error/warn/skip counts and wall time.
- [ ] Run ordinary and `--as-cran` package checks with the system-clock
  environment controlled. Leave the ordinary check at 0 errors, 0 warnings,
  and 0 notes; record the expected incoming-check failure caused by required
  non-CRAN Deputy/dsprrr dependencies and `Remotes` without weakening those
  boundaries or describing the package as CRAN-ready.
- [ ] Run release-oriented URL, spelling, lifecycle, documentation, archive,
  license, dependency, and source-cleanliness checks without submitting to an
  external service.

### Final 0.2.0 commit

- [ ] Only after all prior gates pass, set `DESCRIPTION` from `0.2.0.9000` to
  `0.2.0`, change the current NEWS heading to `# tempest 0.2.0`, update the
  tracked MIT license year/holder consistently to `2025-2026 tempest authors`,
  and rerun the release checks against those exact bytes.
- [ ] Confirm no generated `docs/`, vignette HTML, dsprrr cache, temporary
  library, check directory, local credential/config file, or sibling-repository
  change is staged.
- [ ] Mark this plan and the architecture migration table implemented and
  verified only after the final clean installed/full/check evidence exists.
- [ ] Stop at a clean reviewable branch. Tagging, GitHub release publication,
  CRAN submission, merge, and push each require their own explicit authority.

CRAN publication is a separate distribution project: Deputy and dsprrr are
required but unavailable from CRAN/Bioconductor, Graft is also pinned outside
the main repositories, and incoming checks reject `Remotes`. T10 must not make
these dependencies optional or remove their pins to manufacture a CRAN pass.

### Post-merge GitHub release handoff

Release publication is not part of the implementation branch. After the exact
0.2.0 commit is merged, an explicitly authorized release operator must:

1. manually dispatch the full three-OS R-CMD-check workflow and ecosystem
   contracts on the exact final `main` SHA;
2. verify the clean pkgdown deployment removed obsolete 0.1 generic reference
   pages and the retired reusable-workflows article;
3. require every exact-SHA check to finish green;
4. create annotated tag `v0.2.0` and publish the GitHub release; and
5. follow with a separate development commit returning to `0.2.0.9000`.

Do not create the tag or release from the implementation branch, and do not let
the release-triggered workflow substitute for the pre-tag exact-SHA gate.

## Acceptance checklist

- [ ] Exactly one new public export exists: `tempest_trajectory_review()`;
  installed namespace is 63 exports and two S3 registrations.
- [ ] All eight existing product schema versions and readers are unchanged.
- [ ] A review accepts only an exact completed STORM result or quiescent
  succeeded `TempestSession` and reuses full publication authority validation.
- [ ] Review output is deterministic, credential-safe, capability-free,
  nonmutating, and never persisted.
- [ ] Correlation ids are represented only as non-causal grouping evidence.
- [ ] Deputy, dsprrr, and Graft lanes contain only exact safe identity
  references owned by their respective packages.
- [ ] Proposed and accepted promotion states are distinguished and cross-run
  bundle/receipt mixing fails closed.
- [ ] Existing vitals tasks can evaluate exact baseline and candidate
  ProgramSets/knowledge views without changing them automatically.
- [ ] Tempest adds no optimizer, scorer engine, synthetic trainset, automatic
  compilation, automatic adoption, Graft commit, or new governance authority.
- [ ] The Run review panel is bounded, escaped, accessible, read-only, and adds
  no public handle, background worker, or persisted state.
- [ ] Documentation and supported skills explain authority, non-causation,
  explicit improvement, and release boundaries consistently.
- [ ] Clean deployment prevents deleted reference pages surviving on gh-pages.
- [ ] Exact-pin focused, installed, full-suite, pkgdown, ordinary-check, and
  release-oriented gates are clean; the separate CRAN distribution blockers
  are reproduced and documented accurately.
- [ ] Final tracked version/NEWS/license bytes identify tempest 0.2.0.
- [ ] The post-merge exact-SHA workflow, clean-site, tag, GitHub release, and
  development-bump handoff is documented without performing those external
  actions implicitly.
- [ ] Worktree/index are clean and the plan records exact final evidence.
