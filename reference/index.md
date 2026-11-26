# Package index

## Research workflows & apps

Run scripted STORM, hold an interactive Co-STORM session, or launch a
research app.

- [`tempest_run()`](https://jameshwade.github.io/tempest/reference/tempest_run.md)
  : Run the STORM pipeline
- [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
  : Create a Co-STORM session
- [`tempest_researcher()`](https://jameshwade.github.io/tempest/reference/tempest_researcher.md)
  : Research a topic and produce an enriched report
- [`tempest_researcher_app()`](https://jameshwade.github.io/tempest/reference/tempest_researcher_app.md)
  : Launch the enriched research application
- [`tempest_app()`](https://jameshwade.github.io/tempest/reference/tempest_app.md)
  : Run the Tempest research application
- [`tempest_config()`](https://jameshwade.github.io/tempest/reference/tempest_config.md)
  : Create a STORM configuration
- [`tempest_expert()`](https://jameshwade.github.io/tempest/reference/tempest_expert.md)
  **\[experimental\]** : Create a Tempest expert profile

## Inspect reports & evidence

Read committed reports, verify claim-by-span support, inspect scientific
reviews, or share an MCP card.

- [`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md)
  : Read the committed Markdown report from a Tempest product
- [`tempest_sources()`](https://jameshwade.github.io/tempest/reference/tempest_sources.md)
  : Return evidence resources as a tibble
- [`tempest_claims()`](https://jameshwade.github.io/tempest/reference/tempest_claims.md)
  : Return claims as a tibble
- [`tempest_claim_supports()`](https://jameshwade.github.io/tempest/reference/tempest_claim_supports.md)
  : List explicit claim-support assessments
- [`tempest_claim_reviews()`](https://jameshwade.github.io/tempest/reference/tempest_claim_reviews.md)
  : Read scientific claim reviews
- [`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md)
  **\[experimental\]** : A bounded review of one completed Tempest
  product
- [`tempest_trajectory_review_data()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review_data.md)
  : Extract a validated trajectory review projection
- [`tempest_mcp_app()`](https://jameshwade.github.io/tempest/reference/tempest_mcp_app.md)
  **\[experimental\]** : Review a completed research product in an MCP
  App

## Enriched research reports

Pair cited research with reproducible R analyses, illustrations, and
integrity-checked bundles.

- [`tempest_enrich_report()`](https://jameshwade.github.io/tempest/reference/tempest_enrich_report.md)
  : Enrich completed research with computations and illustrations
- [`tempest_read_enriched_report()`](https://jameshwade.github.io/tempest/reference/tempest_read_enriched_report.md)
  : Read and verify a portable enriched report
- [`tempest_r_executor()`](https://jameshwade.github.io/tempest/reference/tempest_r_executor.md)
  : Create a Deputy-backed R execution callback
- [`tempest_image_generator()`](https://jameshwade.github.io/tempest/reference/tempest_image_generator.md)
  : Create an OpenAI image generation callback

## Reviewed memory with Graft

Publish candidates, review and accept findings in Graft, and admit
verified evidence into later runs.

- [`tempest_publish_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_publish_artifact_research.md)
  : Preserve completed research in a Graft artifact store
- [`tempest_read_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_read_artifact_research.md)
  : Inspect retained research without admitting it to execution
- [`tempest_reuse_artifact_research()`](https://jameshwade.github.io/tempest/reference/tempest_reuse_artifact_research.md)
  : Admit a current accepted research decision
- [`tempest_artifact_knowledge()`](https://jameshwade.github.io/tempest/reference/tempest_artifact_knowledge.md)
  : Bring a retained artifact selection into a research run
- [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  : Build a deterministic proposal for reviewed Graft promotion
- [`tempest_save_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_save_promotion_bundle.md)
  : Save a Tempest promotion bundle atomically
- [`tempest_read_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_read_promotion_bundle.md)
  : Read and validate a current Tempest promotion bundle
- [`print(`*`<tempest_knowledge>`*`)`](https://jameshwade.github.io/tempest/reference/print.tempest_knowledge.md)
  : Print accepted organizational knowledge

## Persistence & host retrievers

Save and resume Co-STORM sessions or connect custom host retrievers and
source records.

- [`tempest_session_save()`](https://jameshwade.github.io/tempest/reference/tempest_session_save.md)
  **\[experimental\]** : Save a Co-STORM session bundle
- [`tempest_session_resume()`](https://jameshwade.github.io/tempest/reference/tempest_session_resume.md)
  : Resume a saved Co-STORM session bundle
- [`tempest_research_workspace()`](https://jameshwade.github.io/tempest/reference/tempest_research_workspace.md)
  : Create a provisional research workspace
- [`tempest_resource()`](https://jameshwade.github.io/tempest/reference/tempest_resource.md)
  **\[experimental\]** : Create a typed evidence resource
- [`tempest_source_id()`](https://jameshwade.github.io/tempest/reference/tempest_source_id.md)
  : Create a deterministic source ID from a URL
