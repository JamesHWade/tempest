# Package index

## Research products

Run scripted STORM, hold a Co-STORM session, or open the bundled app.

- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  : Run the STORM pipeline
- [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  : Create a Co-STORM session
- [`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md)
  : Run the Tempest research application
- [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  : Create a STORM configuration

## Scientific experts

- [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  **\[experimental\]** : Create a Tempest expert profile

## Host retrievers

Connect host search and fetch adapters to the research workspace.

- [`tempest_research_workspace()`](https://jameshwade.github.io/tempest/reference/tempest_research_workspace.md)
  : Create a provisional research workspace
- [`tempest_resource()`](https://jameshwade.github.io/tempest/reference/tempest_resource.md)
  **\[experimental\]** : Create a typed evidence resource
- [`tempest_source_id()`](https://jameshwade.github.io/tempest/reference/tempest_source_id.md)
  : Create a deterministic source ID from a URL

## Accepted organizational knowledge

Bring retained artifact evidence or reviewed Graft knowledge into a run.

- [`tempest_publish_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_publish_artifact_research.md)
  : Preserve completed research in a Graft artifact store
- [`tempest_read_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_read_artifact_research.md)
  : Inspect retained research without admitting it to execution
- [`tempest_reuse_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_reuse_artifact_research.md)
  : Admit a current accepted research decision
- [`tempest_artifact_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_knowledge.md)
  : Bring a retained artifact selection into a research run
- [`print(`*`<tempest_knowledge>`*`)`](https://jameshwade.github.io/tempest/reference/print.tempest_knowledge.md)
  : Print accepted organizational knowledge

## Reading a completed product

Read the committed report and inspect the evidence behind it.

- [`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md)
  : Read the committed Markdown report from a Tempest product
- [`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md)
  : Return evidence resources as a tibble
- [`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md)
  : Return claims as a tibble
- [`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md)
  : List explicit claim-support assessments
- [`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md)
  **\[experimental\]** : A bounded review of one completed Tempest
  product
- [`tempest_trajectory_review_data()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review_data.md)
  : Extract a validated trajectory review projection

## Session persistence

Save and resume the exact current session product.

- [`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
  **\[experimental\]** : Save a Co-STORM session bundle
- [`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md)
  : Resume a saved Co-STORM session bundle

## Graft review and promotion

Propose, review, commit, and verify accepted research evidence.

- [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  : Build a deterministic proposal for reviewed Graft promotion
- [`tempest_save_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_save_promotion_bundle.md)
  : Save a Tempest promotion bundle atomically
- [`tempest_read_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_read_promotion_bundle.md)
  : Read and validate a current Tempest promotion bundle
