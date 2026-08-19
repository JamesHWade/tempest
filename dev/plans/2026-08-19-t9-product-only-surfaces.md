# T9 product-only surfaces implementation plan

Tracking issue: unavailable; the kata daemon did not start during planning

Status: implemented and verified

## Goal

Make Tempest persistence, report, evaluation, promotion, and Shiny surfaces
explicitly about the STORM and Co-STORM research products. Remove the remaining
mixed ownership, dead prompt and compatibility seams, generic member names,
and UI-only persistence knowledge left after T8 deleted the generic kernel.

T9 is an ownership and contract cleanup. It preserves the exact public
namespace while narrowing every retained implementation to a current product
contract. It does not add compatibility machinery for pre-0.2 objects.

## Non-goals

- Do not change the 62-name public export surface or the two registered S3
  methods.
- Do not add legacy readers, aliases, deprecated wrappers, ignored arguments,
  dual writers, coercions, or schema migration code.
- Do not add a generic persistence envelope, report abstraction, host-state
  store, workflow runtime, or evaluation engine.
- Do not serialize chats, functions, tools, credentials, clients, connections,
  Deputy Agents, Shiny reactives, completion capabilities, or live Graft views.
- Do not weaken exact dsprrr ProgramArtifact identity, Deputy correlation,
  immutable Graft snapshot identity, quiescence, or publication gates.
- Do not redesign scorers, optimize programs, add joined trajectory review, or
  build improvement loops. T10 owns those changes.
- Do not perform release, version, CRAN, or 0.2.0 publication work. T10 owns the
  release gate.
- Do not broadly redesign the bundled app or replace shinychat. T9 removes dead
  seams and makes current product state correct and accessible.

## Frozen contracts

### Schemas and authority

Preserve these exact versions and their current strict validation:

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

Preserve the current StageRecord fields, execution-path and support-status
rules, exact report content digest, accepted governed-procedure binding,
Deputy terminal identity, immutable Graft snapshot identity, and fail-closed
publication behavior. A report reference proves exact content identity; it
does not prove that provider response bytes caused the report.

The promotion bundle remains an evidence-only schema-1 payload. A terminal
report is an eligibility and authority gate, not a new serialized promotion
record.

### Public namespace

Keep `tests/testthat/fixtures/public-exports-0.2.0.txt` byte-for-byte unchanged.
The installed namespace must contain exactly those 62 exports and exactly two
S3 registrations. In particular, retain:

- `tempest_report_md()` as a non-authoritative Markdown renderer. Rendering
  alone does not publish, finalize a Manifest, or grant promotion authority.
- `tempest_session_report_md()` as the accessor for the exact report already
  committed to a succeeded `TempestSession`.
- `tempest_task()` and `tempest_costorm_task()` as product evaluation task
  constructors.
- `tempest_promotion_bundle()` as the product promotion constructor, with its
  new current-only product input boundary.
- `run_app()`, `tempest_shiny_ui()`, `tempest_shiny_server()`, and
  `tempest_shiny_store()` as the four product UI exports.

Changing an internal helper, a constructor formal, or a Shiny handle member
does not authorize an export change. Do not retain a removed formal or member
as an alias.

## Ownership and sequencing

Write current-only contract tests before moving implementation. Complete the
report/evaluation/promotion lane before the persistence split because the
split needs the final report-reference owner. Complete the persistence split
before the UI lane so archive validation has one product owner.

| Lane | Exclusive files while active |
|---|---|
| Contracts | Current-only fixtures and new boundary tests |
| Reports and evaluation | `R/product-report.R`, `R/costorm-report.R`, `R/storm-polish.R`, `R/evals.R`, report/eval tests, and retired report prompts |
| Promotion | `R/promotion-types.R`, promotion tests, and promotion documentation |
| Persistence | `R/run-persistence.R`, new product persistence files, related owner files, persistence tests, and `DESCRIPTION` `Collate` |
| UI | `R/app.R`, `R/shiny-adapter.R`, `R/shinychat-adapter.R`, `inst/shiny/`, `inst/examples/shiny-host/`, and Shiny tests |
| Docs/integration | roxygen, generated `man/`, README, vignettes, architecture, NEWS, and this plan |

One owner edits a shared file at a time. Rebase or serialize work at the report
reference, `DESCRIPTION`, `NEWS.md`, generated documentation, and plan seams.

## Batch 0: Freeze current-only contracts

### Public, schema, and absence gates

Files: `tests/testthat/test-public-api.R`, create
`tests/testthat/test-product-surface-inventory.R`, and focused existing schema
tests.

- [x] Assert the exact 62 exported names from
  `public-exports-0.2.0.txt` and the exact two S3 registrations.
- [x] Assert absence of every T8 generic export and internal restore/resume
  symbol without carrying a second deletion-inventory fixture.
- [x] Assert the eight frozen schema versions and exact current envelope fields.
- [x] Assert no supported bundle contains a chat, function, tool, credential,
  client, connection, Agent, reactive, live view, or completion capability.
- [x] Add a static source inventory for current report, persistence, promotion,
  evaluation, and UI entry points. Fail on retired prompt seams, generic
  persistence names, global persistence hooks, or old Shiny handle members.
- [x] Record the current expected failures before implementation. Do not commit
  a deliberately failing test state.

### Report and promotion contracts

Files: `tests/testthat/test-product-report.R`,
`tests/testthat/test-run-verification.R`, `tests/testthat/test-evals.R`, and
`tests/testthat/test-promotion-bundle.R`.

- [x] Specify one exact report-reference constructor and validator shared by
  STORM and Co-STORM.
- [x] Prove `tempest_report_md()` is deterministic rendering only and cannot
  create a succeeded Manifest or promotable product.
- [x] Prove `tempest_session_report_md()` accepts only a `TempestSession` with
  an exact committed report/reference binding and fails on absent, running,
  corrupt, or mismatched state.
- [x] Prove the default `tempest_task()` solver runs the real STORM product and
  returns its authoritative report plus credential-safe product metadata.
- [x] Prove the default `tempest_costorm_task()` solver uses a real
  `TempestSession` and its internal moderator chat accessor rather than a
  removed `$chats` projection.
- [x] Specify the exact new promotion signature:
  `tempest_promotion_bundle(research, claim_ids = NULL)`, where `research` is a
  completed STORM result or a succeeded `TempestSession`.
- [x] Reject loose workspace/Manifest/StageRecord tuples, incomplete products,
  report tampering, wrong mode, nonpublishable paths, and claim selections not
  closed over exact verified evidence.
- [x] Assert the promotion payload remains the exact schema-1 evidence-only
  shape and does not acquire report content or generic run state.

### Persistence and UI contracts

Files: focused persistence contract tests, create
`tests/testthat/test-shiny-product-boundary.R`, and update host-example tests.

- [x] Inventory every top-level definition in `R/run-persistence.R`, record its
  final product owner, and fail if an unassigned definition remains after the
  split.
- [x] Assert current schemas are dispatched by explicit STORM, Co-STORM,
  ResearchWorkspace, ProgramSet, Manifest, StageRecord, or product-envelope
  names rather than a generic run path.
- [x] Assert the four UI exports remain and the public Shiny store/server handle
  exposes distinct Co-STORM session state, STORM state/events, and shared
  authoritative report state.
- [x] Assert removed Shiny handle members and formals are absent; add no member
  alias.
- [x] Specify report-publication failure behavior, launch-config binding, exact
  worker result envelope, supported controls, archive validation delegation,
  and accessible live status markup.

Batch gate: the contract suite describes only current schemas and current
product objects. No test proposes a compatibility reader, schema bump, generic
abstraction, or T10 trajectory feature.

## Batch 1: Reports, evaluation, and promotion

### Remove dead prompt and report seams

Files: `R/product-report.R`, `R/costorm-report.R`, `R/storm-polish.R`,
`inst/prompts/polisher_system.md`, `inst/prompts/reporter_system.md`, report
tests, and prompt-aware test helpers.

- [x] Delete `tempest_storm_report_prompt()` and
  `tempest_costorm_report_prompt()`. Remove the unused prompt member from the
  Co-STORM report context.
- [x] Delete `inst/prompts/polisher_system.md` and
  `inst/prompts/reporter_system.md`.
- [x] Remove polisher/reporter prompt dispatch from
  `tests/testthat/helper-product-baseline.R`,
  `tests/testthat/helper-storm-progress.R`,
  `tests/testthat/test-product-boundaries.R`, and
  `tests/testthat/test-storm.R`.
- [x] Move still-valid deterministic report assertions out of
  `tests/testthat/test-polish.R` and
  `tests/testthat/test-deliverables.R`; delete those files after no owned
  behavior remains.
- [x] Keep provider calls only where a current product stage explicitly owns
  them. Do not retain an unused prompt merely because a fake-chat fixture
  recognizes it.

### Split report ownership and unify references

Files: `R/product-report.R`, `R/storm-polish.R`, `R/costorm-report.R`, create
focused report owner files if needed, and report/persistence tests.

- [x] Give the exact `{report_id, sha256}` record one owner used by STORM,
  Co-STORM, persistence, promotion, UI, and verification.
- [x] Keep shared Markdown escaping, citation rendering, final-report
  validation, and execution-review logic in narrowly named report helpers.
- [x] Move STORM-only assembly and validation to a STORM report owner and
  Co-STORM-only assembly/finalization to `R/costorm-report.R`.
- [x] Keep `tempest_report_md()` documented as a renderer over an explicit
  `ResearchWorkspace`; it must not infer authority from loose arguments.
- [x] Keep `tempest_session_report_md()` as a read accessor over the committed
  Co-STORM product report; it must not generate, repair, or silently coerce one.
- [x] Preserve exact canonical report bytes and existing deterministic report
  fixtures where the product behavior has not changed.

### Repair product evaluation tasks

Files: `R/evals.R`, `tests/testthat/test-evals.R`, and generated task topics.

- [x] Replace the lightweight cited-answer default behind `tempest_task()` with
  a solver that invokes `tempest_run()` and returns the authoritative STORM
  report and credential-safe Manifest/workspace/StageRecord summaries.
- [x] Keep explicit solver injection for vitals composition, but remove unused
  or ignored default-solver formals. Do not add a generic solver registry.
- [x] Make the built-in `tempest_costorm_task()` solver create and complete a
  real `TempestSession`, read the moderator chat only through the internal
  current session accessor, and return the committed session report.
- [x] Return no Deputy Agent, live chat, tool, client, or completion capability
  in solver metadata. A `solver_chat` required by vitals stays process-local.
- [x] Use deterministic fake chats and local workspaces in tests; require no
  API key or network access.
- [x] Leave scorer selection and optimization policy unchanged for T10.

### Narrow promotion to completed products

Files: `R/promotion-types.R`, `R/promotion-persistence.R`, promotion helpers,
tests, roxygen, and generated documentation.

- [x] Change `tempest_promotion_bundle()` to accept only
  `research` plus optional `claim_ids`; remove the loose
  `workspace`, `manifest`, and `stage_records` formals without aliases.
- [x] For a STORM result, require the exact completed result shape, succeeded
  STORM Manifest, authoritative StageRecords, sealed Workspace, and exact
  report/reference match before deriving promotion inputs.
- [x] For a Co-STORM input, require a succeeded `TempestSession`, quiescent
  execution, sealed Workspace, exact committed report/reference match, and
  authoritative session StageRecords.
- [x] Reuse the shared product authority and report-reference validators. Do
  not reconstruct success from independently supplied pieces.
- [x] Preserve claim selection, evidence closure, ProgramArtifact identity,
  bundle ID, and schema-1 persistence bytes except where the new constructor
  removes caller ambiguity.
- [x] Update helpers and every promotion test to build a completed STORM result
  or succeeded session. Add adversarial cross-product and tamper cases.

Batch gate: focused report, evaluation, promotion, authority, STORM, and
Co-STORM tests pass. There is one report-reference implementation, no dead
polisher/reporter prompt, and promotion cannot be constructed from loose state.

## Batch 2: Product-specific persistence

### Split the monolith by product owner

Delete `R/run-persistence.R` after moving every retained definition into:

1. `R/product-persistence.R` for canonical product envelope, JSON, checksum,
   safe-path, atomic-write, and condition helpers shared by two or more current
   product formats;
2. `R/research-workspace-persistence.R` for ResearchWorkspace schema-5
   snapshot/restore and its exact record codecs;
3. `R/costorm-persistence.R` for Co-STORM schema-9 snapshot, bundle,
   save/restore/resume, and browser-archive validation helpers; and
4. `R/storm-persistence.R` for STORM schema-7 bundle, schema-4 state,
   continuation, and current artifact paths.

Then update `DESCRIPTION` `Collate` so shared validators load before product
readers. Do not change package dependencies as a side effect.

- [x] Move every top-level definition according to the Batch 0 inventory;
  leave no forwarding wrapper in `R/run-persistence.R`.
- [x] Use explicit `storm`, `costorm`, `research_workspace`, `program_set`, or
  `product` names. Remove surviving internal `run_*` names that refer only to
  STORM, while preserving the public `tempest_run()` product name.
- [x] Keep only genuinely shared serialization primitives in
  `R/product-persistence.R`; product dispatch and schema knowledge belong in
  the relevant product file.
- [x] Preserve exact canonical JSON, file names, checksums, bundle IDs, report
  bytes, snapshot identities, error classes, and atomic replacement behavior.

### Return helpers to their domain owners

Files include `R/product-authority.R`, report owner files,
`R/research-expert.R`, `R/stage-record.R`, `R/program-set.R`, and the new
persistence files.

- [x] Move authority validation and publication gates to
  `R/product-authority.R`.
- [x] Move report-reference and report-integrity helpers to the single report
  owner established in Batch 1.
- [x] Move expert record/fingerprint/restore helpers to
  `R/research-expert.R`.
- [x] Move StageRecord payload, digest, and restoration helpers to
  `R/stage-record.R`.
- [x] Keep ProgramSet and Manifest codecs with their current product owners
  when they are not genuinely shared persistence primitives.
- [x] Prove moved helpers have one definition and no circular `Collate`
  dependency.

### Remove global hooks and unused flexibility

- [x] Inventory and remove package-global persistence options, callback hooks,
  injection points, and mutable registries that exist only for tests or former
  generic hosts.
- [x] Remove unused formals and permissive fallback branches from internal
  readers/writers. Tests inject at a narrow caller boundary rather than through
  production persistence APIs.
- [x] Reject missing, extra, coerced, old, future, wrong-mode, or wrong-status
  envelopes exactly as before. Simplification must not make a reader lenient.
- [x] Preserve explicit partial recovery only where the current Co-STORM
  product contract already permits it; do not generalize recovery.

### Own browser archive validation in Co-STORM persistence

- [x] Add one internal Co-STORM archive-reader helper that validates the
  bounded `session.json`, exact schema-9 manifest, declared file set, safe
  relative paths, and checksums before UI restore.
- [x] Keep browser upload/download policy, size quota, private temporary
  permissions, and zip transport in the Shiny layer.
- [x] Remove duplicated schema and manifest-field knowledge from
  `inst/shiny/R/mod_chat.R`.
- [x] Move exact archive-envelope tests to Co-STORM persistence tests while
  retaining Shiny traversal, duplicate-entry, size, tamper, and cleanup tests.

### Migrate tests by owner

Keep behavioral coverage but split the current persistence test family so each
file names one product contract:

- [x] Move workspace tests to
  `test-research-workspace-persistence-*.R`.
- [x] Move Co-STORM tests to `test-costorm-persistence-*.R`.
- [x] Move STORM tests to `test-storm-persistence-*.R`.
- [x] Move genuinely shared envelope/checksum/path tests to
  `test-product-persistence.R`.
- [x] Rename tests and helpers that still say generic `run` when they exercise
  only STORM. Delete an old file after all of its retained assertions have a
  product owner.
- [x] Preserve frozen baseline snapshots and current schema fixtures
  byte-for-byte unless the plan explicitly changes a caller boundary.

Batch gate: `R/run-persistence.R` no longer exists; every inventoried
definition has one current product owner; focused persistence, authority,
report, workspace, STORM, Co-STORM, promotion, and recovery tests pass; and all
eight schemas remain exact.

## Batch 3: Product-only research UI

### Retain the four exports and narrow the handle

Files: `R/shiny-adapter.R`, `inst/shiny/R/store.R`, bundled modules, Shiny
tests, and generated documentation.

- [x] Keep the four UI exports and their package topics.
- [x] Replace generic store members such as `peek`, `get`, `set`, `touch`,
  `save`, `restore`, `evidence_store`, `report_store`, and mutable
  `set_persistence` with exact Co-STORM session/workspace and authoritative
  report names. Add no old-member aliases.
- [x] Distinguish Co-STORM session/events/evidence from STORM events in the
  `tempest_shiny_server()` return handle. Expose the authoritative shared
  report and report Workspace explicitly.
- [x] Name and document the report navigation signal as a monotonic event
  counter rather than a Boolean readiness value.
- [x] Keep internal mutable UI state product-specific; do not create a generic
  host store or expose arbitrary state slots.

### Make publication fail closed

- [x] Stop converting report-integrity or authority errors to a missing report
  in `inst/shiny/R/store.R`. Only a genuinely absent report becomes `NULL`.
- [x] Announce Co-STORM report readiness only after the exact committed session
  report/reference validates and enters the shared report state.
- [x] Capture the exact config used when a STORM task launches and use that
  config when validating its returned product. A later UI edit cannot change
  the validation identity of an in-flight run.
- [x] Treat STORM product-publication rejection as failure. Do not show
  “Pipeline complete” or a Report link when shared report publication failed.
- [x] Expose only credential-safe error text and keep the previously published
  report unchanged after any rejection.

### Remove dead or unsupported behavior

- [x] Remove the STORM `parallel` control and never request
  `parallel_research = TRUE` while Deputy does not own that path.
- [x] Remove the ineffective browser-temp autosave checkbox, counter, server,
  and tests. Retain safe session download/upload; do not invent a durable host
  filesystem policy.
- [x] Require the exact STORM worker envelope containing `result` and
  `progress`; remove raw-result compatibility branches.
- [x] Remove the redundant internal `tempest_run` injection formal in favor of
  one narrow worker factory seam.
- [x] Delete unused shinychat input/turn text conversion helpers left behind by
  the completion-ID cutover and their direct tests.
- [x] Delete non-`TempestSession` report/count branches and other generic
  object fallbacks. Store validation already makes those branches unreachable.
- [x] Delete production-dead Co-STORM progress-label helpers instead of keeping
  them alive through tests.

### Use Workspace vocabulary and one async host path

- [x] Rename `source_store`, `report_store`, and related UI/citation callback
  variables to `workspace`, `evidence_workspace`, or `report_workspace`.
  Accepted values remain only `ResearchWorkspace` and the current
  `TempestRetriever` workspace adapter.
- [x] Keep the bundled app's modules as presentation/reactivity wrappers over
  package product APIs. Do not duplicate claim extraction, report authority,
  persistence schemas, or STORM/Co-STORM execution logic in `inst/shiny/`.
- [x] Replace `inst/examples/shiny-host/app.R` with an example built from
  `tempest_shiny_ui(..., panels = "storm")` and
  `tempest_shiny_server()`. It must use the maintained asynchronous STORM
  adapter, not call `tempest_run()` synchronously in the Shiny main process.
- [x] Update `test-shiny-host-example.R` to require the exported adapter calls
  and reject direct or generic execution logic.

### Accessible live behavior

- [x] Give STORM progress, persistence state, and successful publication a
  polite atomic status live region.
- [x] Give cancellation, validation failure, and publication failure an alert
  role without exposing provider or credential details.
- [x] Make blank STORM and Co-STORM topics produce an accessible validation
  message rather than silently resetting a task button.
- [x] Preserve the keyboard-operable mind-map outline, source/fact table
  labels, citation safety, escaped model/source content, and report download
  content-security policy.
- [x] Preserve session-end cancellation, stale async guards, bounded archive
  handling, and cleanup behavior.

### UI test migration

- [x] Rename `tests/testthat/test-shiny-generic.R` to
  `tests/testthat/test-shiny-product-boundary.R` and assert only positive
  current-product boundaries plus exact absence of old handle members.
- [x] Update `test-shiny-app.R`, `test-shinychat-adapter.R`,
  `test-shiny-host-example.R`, and `test-app.R` for the narrowed contract.
- [x] Add focused tests for launch-config binding, report publication failure,
  exact worker envelope, removed parallel/autosave controls, distinct STORM and
  Co-STORM handle state, and live-region markup.
- [x] Retain deterministic `testServer` coverage, async cancellation tests,
  archive safety tests, citation escaping, and no-network fake chats.

Batch gate: the example and bundled app use the same exported asynchronous
product adapter; no UI component knows a persistence schema; failed report
authority is never displayed as success; and the UI handle has no generic or
aliased state member.

## Batch 4: Documentation and integration

Files: roxygen sources, generated `man/`, `README.md`, `NEWS.md`,
`vignettes/tempest.Rmd`, `_pkgdown.yml`,
`dev/architecture/package-boundaries.md`, and this plan.

- [x] Update `tempest_report_md()`, `tempest_session_report_md()`, evaluation,
  promotion, persistence, and Shiny documentation for exact current-only
  inputs and authority.
- [x] Regenerate all affected `.Rd` files from roxygen. Do not hand-edit a
  generated topic that still has a source.
- [x] Keep `_pkgdown.yml` complete for all 62 public exports. It has 61 topics
  because `tempest_agent_skills()` and `tempest_install_agent_skills()` share
  one generated topic. Reorganize descriptions only if needed to distinguish
  rendering, committed reports, promotion, and research UI.
- [x] Update the README Shiny section to include STORM, Transcript, safe session
  download/upload, and the embeddable async example.
- [x] Update `vignettes/tempest.Rmd` to show only current STORM/Co-STORM report,
  persistence, evaluation, promotion, and UI paths.
- [x] Add concise, single-line, alphabetically placed `NEWS.md` bullets for
  changed public constructor/handle behavior. Preserve the released 0.1.0
  section as history.
- [x] Update `dev/architecture/package-boundaries.md` to mark T9 implemented
  only after all verification gates pass. Preserve T7/T8 plans and superseded
  historical specifications.
- [x] Mark this plan implemented and verified only after the final installed,
  focused, and full gates pass.

## Batch 5: Static, installed, and full verification

### Static and source audits

From the repository root, review every match from searches covering:

```sh
rg -n 'polisher_system|reporter_system|tempest_(storm|costorm)_report_prompt' R inst tests/testthat
rg -n 'SourceStore|source_store|report_store|autosave_trigger|autosave_session' R inst/shiny inst/examples tests/testthat
rg -n 'run_persistence|tempest_run = NULL|parallel_research = TRUE' R inst/shiny inst/examples tests/testthat
rg -n 'tempest\.run|tempest\.persistence|options\(|getOption\(' R/product-persistence.R R/research-workspace-persistence.R R/costorm-persistence.R R/storm-persistence.R
```

Allow only intentional historical prose, retained user configuration options,
the public STORM product name, and positive absence assertions. Allow no
executable retired prompt, stale workspace alias, ineffective autosave,
generic persistence hook, or compatibility branch.

Also verify:

```sh
test ! -e R/run-persistence.R
test ! -e inst/prompts/polisher_system.md
test ! -e inst/prompts/reporter_system.md
git diff --check
```

### Focused product verification

Run documentation and focused suites after formatting:

```sh
air format .
R -q -e 'devtools::document()'
R -q -e 'devtools::test(filter = "^(public-api|product-surface|product-report|run-verification|evals|promotion|product-persistence|research-workspace-persistence|costorm-persistence|storm-persistence|shiny|app)")'
R -q -e 'pkgdown::check_pkgdown()'
git diff --check
```

Focused acceptance requires:

- exact canonical report/reference behavior for STORM and Co-STORM;
- real product default solvers for both evaluation tasks;
- promotion constructed only from a completed STORM result or succeeded
  `TempestSession`;
- exact current-only persistence schemas and error classes;
- one owner per former `run-persistence.R` definition;
- product-specific Shiny handle members and async host example;
- no false-success report publication; and
- accessible live progress, persistence, and error state.

### Clean installed-package audit

Build and install into temporary directories, then inspect the installed
namespace and assets rather than the checkout:

```sh
tempest_build_dir="$(mktemp -d)"
tempest_library_dir="$(mktemp -d)"
R CMD build --output="$tempest_build_dir" .
R CMD INSTALL --library="$tempest_library_dir" "$tempest_build_dir"/tempest_*.tar.gz
TEMPEST_CHECK_LIB="$tempest_library_dir" Rscript -e 'expected <- readLines("tests/testthat/fixtures/public-exports-0.2.0.txt"); .libPaths(c(Sys.getenv("TEMPEST_CHECK_LIB"), .libPaths())); ns <- loadNamespace("tempest"); stopifnot(identical(sort(getNamespaceExports(ns), method = "radix"), expected), length(getNamespaceInfo(ns, "S3methods")[, 1]) == 2L)'
```

In the installed package, additionally assert:

- all four research UI exports and the asynchronous host example exist;
- retired prompt files and `R/run-persistence.R` do not install;
- the Shiny handle exposes only the exact current product members;
- the eight frozen schemas still reject every other version;
- current STORM and Co-STORM bundles round-trip exact report references; and
- promotion schema 1 contains evidence records only.

### Full gates

Run the complete package gates after the installed audit:

```sh
air format .
R -q -e 'devtools::document()'
R -q -e 'devtools::test()'
R -q -e 'pkgdown::check_pkgdown()'
R -q -e 'devtools::check()'
git diff --check
```

Acceptance requires zero test failures, errors, warnings, or skips attributable
to T9; zero R CMD check errors, warnings, or notes; no pkgdown problems; clean
Air and diff checks; an exact installed 62-export/two-S3 surface; and a
reviewable diff limited to product-only report, evaluation, promotion,
persistence, UI, test, and documentation ownership.

### Final verification evidence

- Exact-pin full suite at `41389d2`: 763 cases and 4,648 expectations; 4,648
  passed, with 0 failures, errors, warnings, or skips, in 480.784 seconds using
  4 workers.
- Clean installed archive: exactly 62 exports and two S3 registrations; exact
  13-member store and 10-member server handle; all eight current schema gates;
  current STORM and Co-STORM report round-trips; and evidence-only promotion.
- Pkgdown reported no problems. R CMD check completed with 0 errors, 0 warnings,
  and 0 notes in 41.9 seconds.
- Air was clean; all 178 R and test files parsed; the diff, index, and worktree
  were clean; and adversarial review returned APPROVE after the async-quiescence
  and product-surface inventory fixes.

## T10 handoff

T9 hands T10 current, product-specific reports, bundles, events, identities,
and UI state. T10 may then add joined trajectory review across Tempest, Deputy,
dsprrr, and Graft; explicit review and improvement loops; scorer or optimizer
redesign; and the 0.2.0 release gate.

T9 must not pre-implement those features, persist a joined trajectory, infer
content causation from correlation IDs, or create a general review framework.
