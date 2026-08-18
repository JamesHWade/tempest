# T8 physical generic-kernel removal plan

Tracking issue: kata `an7p`

Status: in progress

## Goal

Delete the inert Tempest 0.1 application-neutral kernel now that T7 made the
STORM and Co-STORM product paths authoritative. The result has exactly 62 public
exports, no generic restore/resume backdoor, no shadow-only implementation, and
no deletion-inventory documentation, tests, fixtures, skills, or assets.

This is a physical breaking removal. Do not add aliases, deprecated wrappers,
compatibility readers, ignored arguments, migration helpers, or replacement
generic infrastructure.

## Frozen product contracts

Preserve the authoritative product implementations in `R/run-persistence.R`,
`R/storm-state.R`, and `R/product-authority.R`. Preserve the bundled Shiny app,
the product-only host example, and the supported `use-tempest-research` and
`conduct-storm-research` skills.

Do not change these current schemas or their exact validation:

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

Retain the current StageRecord fields, execution-identity authority rules,
Deputy completion boundary, exact dsprrr program identities, immutable Graft
snapshot identity, and quiescence/publication gates.

## Batch 1: Delete the implementation

Delete these 18 whole files:

1. `R/artifact-bundle.R`
2. `R/artifact-catalog.R`
3. `R/artifact-codecs.R`
4. `R/builtin-workflows.R`
5. `R/capabilities.R`
6. `R/deliverables.R`
7. `R/expert-types.R`
8. `R/generic-kernel-cutover.R`
9. `R/operation-registry.R`
10. `R/package-lifecycle.R`
11. `R/run-accessors.R`
12. `R/runtime.R`
13. `R/tempest-run.R`
14. `R/workflow-spec.R`
15. `R/workflow-types.R`
16. `R/costorm-discourse.R`
17. `R/costorm-mindmap.R`
18. `R/shadow-provenance.R`

Then edit `R/costorm.R` to remove the obsolete public R6 methods
`extract_facts()`, `harvest_native_sources()`, and `execute_turn_decision()`.
Remove the `auto` compatibility argument and branch from `step()`; an explicit
user prompt remains required. Edit `R/execution-events.R` to remove its special
`TempestRun` branch and document only `TempestSession` event histories.

Remove all 41 public exports below from roxygen and the generated `NAMESPACE`:

```text
tempest_artifact
tempest_artifact_catalog
tempest_artifact_catalog_restore
tempest_artifact_codec
tempest_artifact_codec_definition
tempest_artifact_codec_registry
tempest_artifact_representation
tempest_artifact_store
tempest_builtin_operation_registry
tempest_builtin_workflow_operation_registry
tempest_capability_resolver
tempest_capability_spec
tempest_connection_provider
tempest_connection_ref
tempest_costorm_workflow_adapter
tempest_costorm_workflow_run
tempest_costorm_workflow_spec
tempest_deliverable_spec
tempest_generate_deliverable
tempest_memory_artifact_store
tempest_objective
tempest_operation_registry
tempest_run_approvals
tempest_run_artifact
tempest_run_artifacts
tempest_run_capability_grants
tempest_run_events
tempest_run_record_approval
tempest_run_request_cancel
tempest_run_save
tempest_run_snapshot
tempest_run_status
tempest_run_workflow
tempest_runtime
tempest_skill
tempest_skill_registry
tempest_storm_workflow_adapter
tempest_storm_workflow_run
tempest_storm_workflow_spec
tempest_workflow_spec
tempest_workflow_step
```

Also remove the internal `tempest_run_restore()` and
`tempest_run_resume()` functions. Update `DESCRIPTION` `Collate` entries after
the source deletions; do not disturb package dependencies or product files.

Gate: the package loads with none of the 43 removed entry points or definitions,
and retained product code contains no call to a deleted binding.

## Batch 2: Replace temporary tests and fixtures

Delete:

- `tests/testthat/test-generic-kernel-cutover.R`;
- `tests/testthat/test-discourse.R`;
- `tests/testthat/test-costorm-mindmap.R`;
- `tests/testthat/test-shadow-provenance.R`;
- `tests/testthat/helper-shadow-provenance.R`;
- `tests/testthat/fixtures/generic-kernel-exports-0.1.0.txt`; and
- `tests/testthat/fixtures/generic-kernel-definitions-0.1.0.txt`.

Keep `tests/testthat/fixtures/public-exports-0.1.0.txt` byte-for-byte unchanged.
Add `tests/testthat/fixtures/public-exports-0.2.0.txt` containing the sorted exact
62-name product surface. Update `test-public-api.R` to compare the live namespace
directly with that fixture, expect 62 exports, retain the exact two S3 print
registrations, and remove the temporary retirement-vector test.

The new fixture is exactly:

```text
ResearchWorkspace
SimulatedUser
TempestRetriever
run_app
tempest_agent_skills
tempest_cache_clear
tempest_claim_support
tempest_claim_supports
tempest_claims
tempest_compile_programs
tempest_config
tempest_costorm_task
tempest_create_ragnar_store
tempest_execution_events
tempest_expert
tempest_generate_experts
tempest_governed_procedure_ref
tempest_graft_plan
tempest_graft_schema
tempest_install_agent_skills
tempest_load_program_set
tempest_okf_concepts
tempest_okf_context
tempest_okf_resources
tempest_program_set
tempest_progress_collector
tempest_progress_event
tempest_progress_event_data
tempest_progress_filter
tempest_progress_labels
tempest_progress_replay
tempest_progress_state
tempest_promotion_bundle
tempest_promotion_receipt
tempest_read_okf
tempest_read_promotion_bundle
tempest_report_md
tempest_research_manifest
tempest_research_workspace
tempest_resource
tempest_retriever
tempest_run
tempest_run_async
tempest_run_cancel
tempest_save_program_set
tempest_save_promotion_bundle
tempest_session
tempest_session_process_turn_async
tempest_session_report_md
tempest_session_restore
tempest_session_resume
tempest_session_save
tempest_session_snapshot
tempest_session_warmup_async
tempest_shiny_server
tempest_shiny_store
tempest_shiny_ui
tempest_sources
tempest_suggest_questions
tempest_task
tempest_validation_result
tempest_verify_claims
```

Remove obsolete mocks or tombstone assertions while retaining their positive
product checks in:

- `test-product-boundaries.R`;
- `test-product-validation.R`;
- `test-product-report.R`;
- `test-research-expert.R`;
- `test-research-tools.R`;
- `test-resources.R`;
- `test-costorm-async.R`;
- `test-execution-events.R`;
- `test-ecosystem-contracts.R`;
- `test-costorm-deputy.R`;
- `test-agent-skills.R`; and
- `helper-costorm-warmup.R`.

Retain product coverage in `test-shiny-generic.R`, but rename/reword it as
product-boundary coverage if useful. Retain `test-shiny-host-example.R`, all
product baseline tests and snapshots, persistence tests, authority tests,
schema tests, and deterministic no-network fixtures.

Gate: focused public API, product boundary, Co-STORM, STORM, persistence,
authority, skills, and Shiny tests pass without defining or mocking a deleted
generic symbol.

## Batch 3: Delete retirement documentation and assets

Delete:

- `vignettes/reusable-workflows.Rmd`;
- `man/tempest-generic-kernel-retirement.Rd`;
- `man/ExpertSessionManager.Rd`;
- `man/tempest_create_expert_delegation_tool.Rd`;
- `man/DiscourseManager.Rd`;
- `man/tempest_type_turn_policy.Rd`;
- `man/tempest_mindmap_expand_node.Rd`;
- `man/tempest_mindmap_node_sizes.Rd`;
- `man/tempest_mindmap_oversized_nodes.Rd`;
- `man/tempest_type_node_expansion.Rd`;
- `inst/prompts/discourse_manager_system.md`;
- `inst/prompts/node_expansion_system.md`;
- `inst/prompts/unseen_information_system.md`; and
- the complete `inst/skills/build-tempest-workflow/`,
  `inst/skills/design-tempest-workflow/`, and
  `inst/skills/verify-tempest-workflow/` trees.

Edit `_pkgdown.yml` to remove the deleted article and generic-cutover reference
section. Update `AGENTS.md`, `README.md`, `NEWS.md`, `vignettes/tempest.Rmd`,
`vignettes/agent-skills.Rmd`, `vignettes/open-knowledge-format.Rmd`, package and
Agent Skill roxygen, and `DESCRIPTION` to describe completed removal and only
the two supported product execution paths and skills. Regenerate
`man/TempestSession.Rd`, `man/tempest_execution_events.Rd`,
`man/tempest_agent_skills.Rd`, and `man/tempest-package.Rd` from retained
roxygen; do not hand-edit generated documentation that still has a source.

Update `dev/architecture/package-boundaries.md` from future-tense retirement to
completed removal, record 41 public plus two internal removed symbols, and
record the exact 62-export product fixture. Preserve
`dev/plans/2026-08-17-t7-authoritative-product-paths.md`, the six superseded
historical specifications it names, the released `NEWS.md` 0.1.0 section, and
all historical product fixtures unchanged.

The historical specifications are:

- `dev/specs/2026-07-18-reusable-tempest-workflows.md`;
- `dev/specs/2026-06-28-host-app-modularity.md`;
- `dev/specs/2026-06-28-session-persistence.md`;
- `dev/specs/2026-06-27-evidence-ledger-s7-design.md`;
- `dev/specs/2026-06-29-api-lifecycle-style.md`; and
- `dev/specs/2026-06-29-data-dict-evaluation.md`.

Do not delete or stage ignored local vignette HTML. Preserve
`inst/examples/shiny-host/app.R`, all of `inst/shiny/`, and both supported skill
trees.

Gate: `devtools::document()` does not recreate a retired export or topic, and
`pkgdown::check_pkgdown()` reports no missing or unindexed product topic.

## Batch 4: Static, installed, and full verification

Run the static audit from the repository root:

```sh
rg -n 'tempest_generic_kernel|Tempest(Runtime|Run|Artifact|Workflow)|tempest_(artifact|artifact_catalog|artifact_codec|artifact_store|capability_resolver|connection_provider|deliverable_spec|operation_registry|run_workflow|runtime|skill_registry|workflow_spec)' R inst/shiny inst/examples tests/testthat
rg -n 'build-tempest-workflow|design-tempest-workflow|verify-tempest-workflow|generic-kernel cutover|until T8' README.md NEWS.md _pkgdown.yml vignettes R inst tests/testthat
```

Review every match. Allow only intentional historical prose, frozen schema field
names, and positive product-boundary exclusions; allow no executable reference,
retirement inventory, inactive skill, or generated topic.

Verify the source tree and a clean installed package separately:

```sh
R -q -e 'devtools::document()'
R -q -e 'devtools::test(filter = "^(public-api|product-boundaries|product-validation|product-report|research-expert|research-tools|execution-events|ecosystem-contracts|agent-skills|shiny)")'
tempest_build_dir="$(mktemp -d)"
tempest_library_dir="$(mktemp -d)"
R CMD build --output="$tempest_build_dir" .
R CMD INSTALL --library="$tempest_library_dir" "$tempest_build_dir"/tempest_*.tar.gz
TEMPEST_CHECK_LIB="$tempest_library_dir" Rscript -e 'expected <- readLines("tests/testthat/fixtures/public-exports-0.2.0.txt"); .libPaths(c(Sys.getenv("TEMPEST_CHECK_LIB"), .libPaths())); ns <- loadNamespace("tempest"); stopifnot(identical(sort(getNamespaceExports(ns), method = "radix"), expected), length(getNamespaceInfo(ns, "S3methods")[, 1]) == 2L, !exists("tempest_run_restore", ns, inherits = FALSE), !exists("tempest_run_resume", ns, inherits = FALSE))'
```

In the installed package, assert exactly 62 namespace exports, exactly the
fixture names, two S3 registrations, and absence of all 41 retired exports plus
the two internal restore/resume symbols. Confirm the supported skills and host
example install, while the three deleted skills and retirement vignette/topic
do not.

Run full gates:

```sh
air format .
R -q -e 'devtools::document()'
R -q -e 'devtools::test()'
R -q -e 'pkgdown::check_pkgdown()'
R -q -e 'devtools::check()'
git diff --check
```

Acceptance requires all product baselines and frozen schemas to remain exact,
no generic or shadow execution path to exist in the source or installed
namespace, no stale documentation or skill inventory, and a reviewable diff
limited to T8 removal and the product-surface migration.
