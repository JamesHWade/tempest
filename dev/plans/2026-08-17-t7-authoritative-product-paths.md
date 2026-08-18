# T7 authoritative product paths implementation plan

Tracking issue: kata `an7p`

Status: implementation-ready

## Goal

Make STORM and Co-STORM the only working Tempest execution paths. Open-ended
agent work runs through Deputy; fixed typed transforms use the exact
`TempestProgramSet`; provisional evidence lives in `ResearchWorkspace`; and
accepted context retains its exact Graft snapshot identity.

T7 is the behavioral cutover. The experimental generic kernel remains
physically present for T8, but its 41 public exports and two internal
restore/resume entry points become uniformly inert.

## Non-goals

- Do not delete generic files, classes, exports, topics, fixtures, or deletion
  inventory. T8 owns physical removal.
- Do not add compatibility readers, dual writers, aliases, ignored arguments,
  test backdoors, or a cutover opt-out.
- Do not add a generic runtime, workflow engine, artifact catalog, connection
  registry, capability resolver, or universal report abstraction.
- Do not persist completion capabilities, content/proof digests, receipts,
  chats, Agents, tools, functions, runtimes, connections, or stores.
- Do not claim durable proof that particular response bytes caused an output.
- Do not move accepted-knowledge write authority out of Graft.
- Do not silently downgrade requested parallel or governed behavior.

## Frozen contracts

### Schemas

| Contract | Version |
|---|---:|
| `ResearchWorkspace` snapshot | 5 |
| Co-STORM snapshot/bundle | 9 |
| STORM bundle | 7 |
| STORM product state | 4 |
| `TempestResearchManifest` | 2 |
| `TempestStageRecord` | Current exact fields |

No migration, proof sidecar, completion field, receipt field, or authority
projection is added. Existing content/report digests remain unchanged.

### Supported surface

- Scripted STORM: `tempest_run()`, `tempest_run_async()`, and
  `tempest_run_cancel()`.
- Co-STORM: `tempest_session()`, product save/restore/resume, and the bundled
  Shiny flow.
- `TempestSession$request_completion_async()` becomes the product-owned Deputy
  moderator seam. A callback/adapter may receive display chunks, but the method
  resolves only to an opaque `completion_id`.
- The exact public processing signature is
  `tempest_session_process_turn_async(session, completion_id, suggest=TRUE, n_suggestions=4L, is_current=function() TRUE)`.
  Remove `user_text`, `assistant_text`, `deputy_execution`, `provider_turn`, and
  `turn_id`; add no alias.
- `tempest_execution_events()` accepts product histories only and rejects
  `TempestRun`.
- All names in `tempest_generic_kernel_exports` remain exported but reject
  immediately. Internal `tempest_run_restore()` and `tempest_run_resume()` do
  the same.
- `NAMESPACE` stays at the current exact 103 exports in T7. The new request
  seam is an R6 method, not a namespace export.

### Live completion boundary

A completion capability is process-local, execution-owned, opaque, and
single-use; callers see only its `completion_id`. Its private registry entry
binds:

- exact scalar UTF-8 prompt and assembled Deputy response, without
  normalization;
- the exact provider Turn;
- terminal run/session/correlation identity;
- the full parent/delegation/tool-call tuple when delegated;
- an internal exact-byte digest for in-process substitution detection; and
- owner identity plus lifecycle state.

Claiming an ID atomically moves `issued -> processing`. A claim/processing
failure before product mutation may atomically release `processing -> issued`;
stale or discarded work detected before mutation moves to `cancelled`. Once the
first transcript, event, or Workspace mutation begins, terminalize the ID as
`consumed` even if later enrichment fails or is cancelled. Consumed/cancelled
IDs always reject replay, as do wrong-owner, unknown, cross-session, and
concurrent claims.

`SessionEnd` captures trace only. Settle the exact prompt, assembled response,
provider Turn, and trace into the owning session registry before clearing the
pending Deputy run; if registry recording fails, roll back settlement and leave
the run pending. Processing obtains all four values only by claiming the ID;
`last_execution` is never a join key. Snapshot, save, report, promotion, and
success fail while a Deputy run, issued/processing capability, queued/active
job, warmup/report job, or running StageRecord exists. Never serialize a
capability or its digest.

After restore, authority is re-derived only from durable execution identities:
Manifest, StageRecords, ProgramSet identities, Workspace records, and Graft
snapshot. `binding_scope` remains `execution_identity`; this does not prove
response-content causation. Quiescent partial bundles remain recoverable but
provisional, report-free, and nonpublishable.

## Ownership and sequencing

Land contract tests and foundations before the parallel execution lanes. Rebase
both lanes before the shared authority/persistence batch. One owner edits a
shared seam at a time.

| Lane | Exclusive files while active |
|---|---|
| Contracts | New contract tests and focused existing tests named in Batch 0 |
| Foundations | New product helper files; `R/config.R`, `R/citations.R`, `R/models.R`, `R/resources.R`, `R/retriever.R`, `R/expert-types.R`, `R/tools.R`, `R/open-knowledge-format.R`, `R/deputy-adapter.R`, plus foundation-only edits to `R/run-persistence.R`, `R/run-accessors.R`, `R/shiny-adapter.R`, and `R/costorm-turn-types.R` |
| Co-STORM | `R/costorm*.R`, `R/verify.R`, `R/deputy-experts.R`, `R/costorm-report.R`, `R/shinychat-adapter.R`, `inst/shiny/R/mod_chat.R`, `inst/shiny/R/store.R`, and Co-STORM tests |
| STORM | `R/storm.R`, `R/storm-research.R`, `R/storm-polish.R`, and STORM tests |
| Shared authority | `R/stage-record.R`, `R/product-authority.R`, `R/run-persistence.R`, `R/promotion-types.R`, `R/evals.R`, and authority/persistence tests |
| Generic hard cut | Generic implementation files and tests named in Batch 5, after detachment passes |
| Docs/integration | Documentation, generated topics, examples, fixtures, and shared baseline tests |

The Co-STORM and STORM owners must not edit shared authority/persistence files.
The hard-cut owner must not start until product-boundary tests prove zero
product dependency on the generic kernel.

## Batch 0: Write the contracts first

Run each new test against pre-cutover code to confirm the intended failure; do
not merge an intentionally failing test commit.

### Frozen shape and live capability

Files: `tests/testthat/test-public-api.R`,
`tests/testthat/test-run-persistence.R`,
`tests/testthat/test-storm-state.R`,
`tests/testthat/test-research-manifest.R`,
`tests/testthat/test-research-workspace.R`, create
`tests/testthat/test-agent-completion.R`, and update Deputy/Co-STORM async tests.

- [ ] Assert the frozen schema versions, exact StageRecord serialized fields,
  103 exports, and exact 41-name retirement vector.
- [ ] Assert no capability, private digest, receipt, proof, or authority
  projection appears in any snapshot/bundle.
- [ ] Specify exact UTF-8 preservation, substitution rejection, lifecycle
  transitions, safe pre-mutation release, stale/discard cancellation,
  mutation-boundary consumption, and replay/wrong-owner/cross-session
  rejection.
- [ ] Specify that `SessionEnd` captures trace only, registry settlement binds
  exact prompt/assembled response/provider Turn/trace before pending clearance,
  and recorder failure leaves the run pending.
- [ ] Specify correct ID-based handling of multiple queued completions and
  prove `last_execution` is not used as a join key.
- [ ] Specify direct and delegated terminal tuples, including parent,
  delegation, tool-call, child run/session, expert, and correlation identity.
- [ ] Assert `request_completion_async()` streams display chunks but resolves
  to an opaque ID, and assert the exact processing signature plus absence of
  all five removed formals.
- [ ] Assert snapshot/save/report rejection for issued/processing capabilities
  and queued/active work.

### Product cutover and authority

Files: `tests/testthat/test-storm.R`, Co-STORM warmup/progress/verify tests,
create `tests/testthat/test-product-boundaries.R`, rename
`test-shadow-provenance.R` to `test-product-authority.R`, and update
stage-record, promotion, persistence, and builtin-workflow tests.

- [ ] Assert every open-ended STORM expert answer resolves one completed Deputy
  terminal trace; fixed typed transforms still resolve their ProgramSet entry.
- [ ] Assert `parallel_research = TRUE` rejects until Deputy manages it.
- [ ] Assert arbitrary assistant text/trace pairs cannot mutate Co-STORM and a
  pre-mutation claim failure leaves transcript, Workspace, StageRecords, and
  Manifest unchanged; post-mutation enrichment failure leaves the ID consumed.
- [ ] Assert product state exposes no chats, generic runtime/catalog/run, or
  capability registry.
- [ ] Specify mode-specific direct/delegated authority, governed exact-program
  success, fallback downgrade, atomic rollback, tamper rejection, publication,
  and restore behavior.
- [ ] Assert quiescent partial bundles restore as nonpublishable.
- [ ] Create a table-driven test for all 41 retirement exports plus internal
  restore/resume: one exact class/message and no argument evaluation.
- [ ] Add a static/call-boundary test for retained product files and the
  forbidden generic dependencies extracted in Batch 1.

Batch gate: record the expected failures and confirm no test proposes a schema
bump, compatibility mode, serialized capability, proof digest, or content-
causation claim.

## Batch 1: Product foundations

### Extract product primitives

Create `R/product-validation.R`, `R/product-hash.R`, `R/product-report.R`,
`R/research-expert.R`, `R/research-tools.R`, and `R/execution-events.R`.

- [ ] Move product scalar/flag/canonical-list validation out of generic
  workflow helpers without changing product validation behavior.
- [ ] Move `TempestValidationResult` and `tempest_validation_result()` out of
  `R/workflow-types.R`.
- [ ] Replace product artifact-codec use with direct canonical content hashing
  and product errors across Workspace resources, retrieval cache identity,
  experts, and report references.
- [ ] Move `TempestExpertProfile`, `tempest_expert()`, and exact product
  record/fingerprint/restore helpers out of `R/expert-types.R`; leave generic
  skill/capability/connection types inert for T8.
- [ ] Move product role/model selection and fixed scientific tool construction
  out of `R/runtime.R`; expose no generic resolver.
- [ ] Move STORM/Co-STORM prompts, rendering, execution review, and final-report
  helpers out of `R/deliverables.R`; use a product report condition.
- [ ] Move `tempest_execution_events()` out of `R/run-accessors.R`, retain only
  product histories, and remove its `TempestRun` branch.
- [ ] Rename internal product ProgramSet/Graft helpers whose
  `tempest_workflow_*` names imply a generic dependency.
- [ ] Keep `tempest_shiny_store()` product-only and remove generic-run behavior.
- [ ] Add fixture tests proving retained canonical hashes/report output remain
  exact.

### Remove generic configuration/expert semantics

Files: `R/config.R`, `R/research-expert.R`, `R/storm-perspectives.R`,
`R/run-persistence.R`, config/expert/host tests, and fixture helpers.

- [ ] Remove `TempestConfig@artifact_store`, constructor input, validation,
  digest input, docs, and fixtures; add no deprecated/ignored argument.
- [ ] Restrict product expert profiles to fixed scientific roles; reject
  nonempty generic skill, capability, model-policy, and connection requests.
- [ ] Retain generic expert-record arrays only where a frozen schema requires
  their fields, force them empty, and never resolve them.
- [ ] Preserve exact persona-to-expert identity through StageRecord and
  persistence.

### Add completion registry and path-derived StageRecords

Files: create `R/agent-completion.R`; update `R/deputy-adapter.R`,
`R/stage-record.R`, and their focused tests.

- [ ] Implement execution-owned `issue`, `claim`, `consume`, `cancel`,
  safe pre-mutation `release`, `active`, and `assert_quiescent` operations.
- [ ] Validate exact prompt, assembled response, provider Turn, and complete
  terminal identity at issue; keep the private digest inaccessible and
  noncanonical.
- [ ] For sync and streaming Deputy calls, make `SessionEnd` trace-only and
  settle all four values into the registry before pending-run clearance.
- [ ] Make settlement transactional: recorder failure rolls back and leaves the
  run pending; remove `last_execution` as a join key.
- [ ] Encode the mutation boundary exactly: pre-mutation retry may release,
  stale/discard cancels, and first transcript/event/Workspace mutation consumes
  even when later enrichment fails or is cancelled.
- [ ] Derive `governed` only from succeeded exact-program execution with exact
  accepted procedure and `fallback_taken = FALSE`; derive other paths from
  stage policy.
- [ ] Remove `execution_path` from same-attempt immutable comparison so running
  attempts can transition to a derived terminal path.
- [ ] Reject forged governed paths or identity mismatches without changing the
  existing StageRecord shape.

Batch gate: focused foundation tests pass; schemas/export count stay frozen;
retained product code no longer calls generic validation, codec, report, event,
artifact-store, runtime, connection, skill, or capability helpers.

## Batch 2: Parallel execution cutovers

### Co-STORM lane

Files: `R/costorm.R`, `R/costorm-async.R`, `R/costorm-turn-types.R`,
`R/verify.R`, create `R/deputy-experts.R` and `R/costorm-report.R`, update
`R/shinychat-adapter.R`, `inst/shiny/R/mod_chat.R`, `inst/shiny/R/store.R`, and
Co-STORM-focused tests.

- [ ] Remove session ownership of generic runtime, connections, artifact
  catalog, workflow run, capability grants, and generic discourse manager.
- [ ] Replace `ExpertSessionManager` with a private Deputy-specific manager
  that attaches fixed Tempest tools and records exact terminal identities.
- [ ] Keep chats, managers, active jobs, and capability registry private;
  expose immutable product records only.
- [ ] Implement `request_completion_async()` through the private Deputy
  moderator; callbacks/adapters receive display chunks, while the promise
  resolves only to `completion_id`.
- [ ] Make `tempest_session_process_turn_async()` claim that ID from its owning
  session and obtain prompt, response, provider Turn, and trace only from the
  registry; accept no independently supplied turn content or identity.
- [ ] Before mutation, atomically release retryable claim failures or cancel
  stale/discarded work. At the first transcript/event/Workspace mutation,
  terminalize consumed; later enrichment failure/cancellation may not release
  or replay the ID.
- [ ] Reject arbitrary assistant `add_turn()`, caller-supplied extraction text,
  native-source harvesting from caller chats, and raw `step(auto = TRUE)`.
- [ ] Persist and validate the full delegation tuple in existing
  Manifest/StageRecord fields; preserve multiple queued completions by ID.
- [ ] Keep failed warmup extraction non-authoritative; record an evidence gap
  only when no claim was asserted.
- [ ] Keep mind map/suggestions as frozen presentation projections, never
  authority or report evidence; prevent expert identity drift after persona.
- [ ] Build reports only from verified Workspace evidence/disputes,
  ProgramSet, and StageRecords; render/validate directly with product helpers.
- [ ] Gate report start/completion and save on quiescence plus authority; remove
  catalog, deliverable-plan, artifact, reporter-chat, and generic Shiny-store
  paths.
- [ ] Update Shiny to display callback/adapter chunks and pass only
  `completion_id` into processing; reset/cancelled work cannot commit later.

### STORM lane

Files: `R/storm.R`, `R/storm-research.R`, `R/storm-polish.R`,
`tests/testthat/test-storm.R`, and `tests/testthat/test-storm-state.R`.

- [ ] Remove runtime/factory, connection permissions, and artifact catalog from
  `tempest_run_internal()` and its callers.
- [ ] Attach fixed writer/evidence and expert web/evidence tools explicitly.
- [ ] Wrap every open-ended expert chat in
  `tempest_deputy_chat_adapter()` with deterministic run/session/expert identity
  and one correlation ID per question.
- [ ] Issue/consume the shared completion capability internally before claim
  extraction; bind retrieval step, Deputy terminal tuple, expert, and
  correlation identity into the extraction StageRecord.
- [ ] Collect terminal traces in the live Manifest and require no pending or
  unconsumed work before checkpoint, return, cancellation, or success.
- [ ] Hard-reject `parallel_research = TRUE`; do not invoke the non-Deputy
  worker path or silently run sequentially.
- [ ] Keep perspectives, personas, query decomposition, extraction,
  verification, outline, and writing as direct exact-program dsprrr transforms.
- [ ] Replace generic polish/catalog behavior with direct product prompt,
  render, execution review, final validation, and Manifest report binding.
- [ ] Finalize authority even with `output_dir = NULL`; returned and persisted
  manifests must be identical.

Batch gate: rebase both lanes on Batch 1, run each focused suite independently,
and confirm neither lane changed shared authority/persistence files.

## Batch 3: Durable execution-identity authority

### Validator and atomic binding

Rename `R/shadow-provenance.R` to `R/product-authority.R` and the matching
helper/test files. Then update `R/verify.R`, `R/storm.R`, `R/costorm.R`, and
`R/run-persistence.R` under the shared owner.

- [ ] Replace observational shadow terminology with a mode-specific authority
  validator; retain `binding_scope = "execution_identity"` and the explicit
  response-content disclaimer.
- [ ] Derive a canonical, nonpersisted projection from candidate Manifest,
  exact ProgramSet, StageRecords, Workspace, optional report, and mode.
- [ ] Validate config/run/mode, program artifacts, required Graft snapshot,
  stage outputs, Workspace coverage/support, report policy, and exact direct or
  delegated terminal traces.
- [ ] Enforce STORM `research/expert` and Co-STORM `dialogue|warmup`
  moderator/expert rules; reject partial delegation tuples and orphan traces.
- [ ] Fail incomplete applicable provenance; allow only explicitly partial,
  quiescent, report-free, nonpublishable state.
- [ ] Create one finalizer that binds candidate StageRecords/traces/report,
  derives run/session references, validates authority, then commits atomically.
- [ ] Co-STORM verification commits candidate support records, successful
  StageRecords, and Manifest only after authority succeeds.
- [ ] STORM finalizes before every durable checkpoint and before partial or
  succeeded return; set success only after authority and quiescence pass.

### Persistence, publication, promotion

Files: `R/run-persistence.R`, `R/promotion-types.R`, `R/evals.R`, and their
focused tests.

- [ ] Replace pending-run-only checks with one quiescence assertion covering
  pending runs, capability lifecycle, queued/active jobs, and running stages.
- [ ] Validate exact direct/delegated terminal records by mode within Manifest
  schema 2.
- [ ] Require authority before saving/restoring succeeded or report-bearing
  state, report publication, promotion, and authoritative evaluation.
- [ ] On restore, re-derive identity authority only; never recreate a
  completion or compare a persisted response digest.
- [ ] Accept current-schema partial bundles only when consistent, quiescent,
  provisional, and report-free.
- [ ] Require loaded/returned Manifests to equal persisted canonical records,
  including report and trace references; retain exact version rejection.

Batch gate: authority, verification, persistence, promotion, and evaluation
tests pass; every agent-derived durable claim resolves one terminal Deputy
identity and every governed stage resolves exact procedure/program identity.

## Batch 4: Product detachment and host cutover

Files: retained product files found by the boundary test, `R/shiny-adapter.R`,
`inst/examples/shiny-host/app.R`, product tests, and fixtures.

- [ ] Remove remaining product construction/calls/fields for `TempestRuntime`,
  `TempestRun`, `TempestArtifact`, `TempestArtifactCatalog`, WorkflowSpec,
  operation registries, deliverable plans, artifact stores, generic capability
  or connection providers, and generic skill registries.
- [ ] Restore only product chats/adapters, fixed tools, ProgramSet, retriever,
  Workspace, Manifest, and Graft view.
- [ ] Narrow Shiny host/example to `tempest_run()` and `tempest_session()`;
  remove generic workflow/run controls and artifact inspection.
- [ ] Convert fixtures to exact Workspace/report/Manifest state without generic
  runtime, catalog, connection, expert, or artifact-store setup.
- [ ] Run product baseline, Shiny, ecosystem, and persistence tests while the
  boundary test instruments forbidden generic entry points.

Gate: zero retained product call reaches the generic kernel before Batch 5.

## Batch 5: Hard-cut the generic kernel

### One condition, every entry point

Files: `R/package-lifecycle.R`, `R/capabilities.R`, `R/config.R`,
`R/operation-registry.R`, `R/runtime.R`, `R/run-accessors.R`,
`R/workflow-spec.R`, `R/artifact-codecs.R`, `R/deliverables.R`,
`R/workflow-types.R`, `R/tempest-run.R`, `R/expert-types.R`,
`R/artifact-catalog.R`, `R/builtin-workflows.R`, and create
`tests/testthat/test-generic-kernel-cutover.R`.

- [ ] Add internal `tempest_generic_kernel_abort()` with exact class
  `tempest_generic_kernel_cutover_error` and message:
  `Tempest 0.2 supports only the STORM and Co-STORM product APIs; the experimental generic kernel is unavailable.`
- [ ] Make that abort the first executable expression in all 41 functions in
  `tempest_generic_kernel_exports`, plus internal `tempest_run_restore()` and
  `tempest_run_resume()`.
- [ ] Preserve lazy arguments so even missing/default/side-effecting arguments
  are not evaluated before the cutover condition.
- [ ] Add no option, environment flag, test hook, alias, or private route back
  to generic execution.
- [ ] Leave all names/classes/exports physically present; keep product
  `tempest_execution_events()` outside the hard cut.

### Retire behavior tests without deleting files

Update generic tests for artifact bundle/catalog/codecs, capabilities,
deliverables, operation registry, run accessors, runtime, generic/host Shiny,
TempestRun/bundle, WorkflowSpec/types, and builtin workflows.

- [ ] Replace generic behavior assertions with small local hard-cut assertions;
  do not skip or retain a test-only working kernel.
- [ ] Keep the table-driven 41-export test as the canonical complete gate and
  `test-public-api.R` as the T7 export-presence gate.
- [ ] Verify all 41 exports and both internal functions return identical
  class/message before argument evaluation.
- [ ] Run the complete product suite after the cut; any residual dependency
  must now fail immediately.

## Batch 6: Documentation

Files: `NEWS.md`, `README.md`, `_pkgdown.yml`, changed roxygen/generated topics,
`vignettes/reusable-workflows.Rmd`, `vignettes/tempest.Rmd`,
`vignettes/open-knowledge-format.Rmd`, host example, and workflow skill files.

- [ ] Add one unwrapped `NEWS.md` bullet for `an7p` covering authoritative
  product paths and immediate generic rejection.
- [ ] Document `tempest_run()` and `tempest_session()` as the only working
  execution paths and the capability-only turn-processing break.
- [ ] State that live capability consumption detects in-process substitution,
  while durable provenance proves execution identity only.
- [ ] Give generic API topics the exact hard-cut message/T8 removal status;
  remove generic runtime/workflow/artifact guidance and examples.
- [ ] Keep historical specifications unchanged; this accepted plan and package
  boundary govern conflicts.
- [ ] Run `devtools::document()` and inspect generated topics; do not delete
  generic topics before T8.

## Batch 7: Verification and acceptance

Run from the repository root.

```sh
R -q -e 'devtools::test(filter = "^(agent-completion|deputy-adapter|costorm-async|costorm-deputy|costorm-warmup)$")'
R -q -e 'devtools::test(filter = "^(storm|storm-state|stage-record)$")'
R -q -e 'devtools::test(filter = "^(product-authority|verify|run-persistence|promotion-bundle)$")'
R -q -e 'devtools::test(filter = "^(generic-kernel-cutover|public-api|product-boundaries|ecosystem-contracts)$")'
R -q -e 'devtools::test(filter = "^(product-baseline|shiny-app|costorm-progress)$")'
```

The first audit must return no product dependency match. Review the second and
allow only inert wrappers and retirement documentation.

```sh
rg -n 'tempest_(workflow_(scalar|flag|serializable_list)|artifact_codec_encode|deliverable_abort|runtime_model)|Tempest(Runtime|Run|ArtifactCatalog)' R/storm*.R R/costorm*.R R/verify.R R/citations.R R/models.R R/resources.R R/retriever.R R/open-knowledge-format.R R/shiny-adapter.R inst/shiny
rg -n 'tempest_(runtime|artifact_catalog|run_workflow|deliverable_spec|operation_registry|capability_resolver|connection_provider|skill_registry)\(' R inst/shiny inst/examples
```

Full gates:

```sh
air format .
R -q -e 'devtools::document()'
R -q -e 'devtools::test()'
R -q -e 'pkgdown::check_pkgdown()'
R -q -e 'devtools::check()'
git diff --check
```

Acceptance checklist:

- [ ] STORM and Co-STORM baselines pass without constructing or invoking
  generic runtime/artifact/workflow state.
- [ ] Every open-ended durable result resolves exact Deputy
  run/session/correlation identity; delegated results resolve the full tuple.
- [ ] Every fixed transform resolves its exact ProgramSet artifact; every
  governed result resolves accepted procedure/program/Graft snapshot with no
  fallback.
- [ ] Wrong/replayed/cross-session capabilities and terminal-to-consumption
  races fail atomically; retry release is pre-mutation only, and the first
  transcript/event/Workspace mutation makes the ID consumed.
- [ ] Snapshot/save/report/promotion/success fail until fully quiescent.
- [ ] Reports use verified Workspace state and direct product rendering only;
  transcript/mind-map prose cannot become report evidence.
- [ ] Succeeded/report-bearing restore and promotion re-derive durable identity
  authority; partial quiescent state remains provisional.
- [ ] Returned and persisted Manifests agree exactly.
- [ ] Workspace 5, Co-STORM 9, STORM bundle 7/state 4, Manifest 2, and current
  StageRecord fields remain exact.
- [ ] No bundle contains a completion/digest/proof sidecar, chat, function,
  tool, Agent, runtime, connection, or store handle.
- [ ] All 41 retirement exports plus internal restore/resume hard-reject through
  the shared condition while names remain physically present/exported.
- [ ] `tempest_execution_events()` works for product state and rejects
  `TempestRun`.
- [ ] Tests make no response-content-causation claim and need no API key,
  network, or live provider.
- [ ] Formatting, full tests, pkgdown, R CMD check, and `git diff --check` pass.

## T8 exclusions

T8, not T7, deletes generic implementation files; removes the 41 exports and
internal generic restore/resume; deletes generic classes, topics, vignettes,
skills, examples, fixtures, persistence/adapters, and temporary hard-cut tests;
and makes the explicit namespace-removal diff.

T7 stops when product paths verify independently and the still-visible generic
surface is uniformly inert.
