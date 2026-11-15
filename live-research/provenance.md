# Live report provenance

Generated on November 15, 2026 with Tempest. Open `index.html` for the reader edition or `report/index.html` for the evidence briefing, executed methods, and downloadable outputs.

## Models and execution

- All research language roles (coordinator, expert, writer, mind map and judge), enrichment planning, and reader synthesis used `gpt-6.1-sol` through the OpenAI API.
- The conceptual illustration used `gpt-image-2.6-flare`, 1536 × 1024, medium quality.
- The analysis ran in a fresh Deputy RSession using R 4.6.1 and the real `datasets::CO2`. It produced two PNG charts and five CSV files. No simulated measurements or authored replay were supplied.
- The completed research retained seven source records (including mirrors), eighteen claims and eighteen exact evidence spans. Verification classified twelve claims as supported and six as partially supported. Five claims received advisory scientific review. These are run records, not an independent scientific certification.
- Numerical validation independently recomputed all twelve plant means and both group contrasts from the installed dataset, matching the exported values within 1e-12. The raw CSV values also match the dataset.

## Review and interventions

This was a supervised live qualification, not an unattended benchmark. An initial sixteen-source budget was exhausted; a second run exposed a missing evidence-capture handoff. Those attempts are retained in the local working artifacts. The final run used three research perspectives, one question per perspective, a sixty-four-source ceiling and a retained extraction program bounded to six source-quoted claims per answer. The exact program was bound into the normal STORM execution ledger through its internal runner; verification and completed-product checks were retained.

The original 1990 paper is scanned. Its actual PDF was rendered and transcribed with Tesseract OCR, then admitted as source text before the final run. OCR can introduce transcription errors. This report does not claim to have established a complete plant-ID crosswalk or resolved the concentration-unit discrepancy.

The first model plan proposed PDF charts and no illustration. The model revised it after review to produce embedded PNG charts, CSV exports and one conceptual image. Its R code was reviewed before execution. The reader synthesis was generated in an additional live model call from the completed briefing and actual numerical outputs. A rounding error was returned to the model for correction. The original proposals and review prompts remain in the local audit artifacts.

The reader edition is a derived narrative and presentation, not a replacement for Tempest's evidence authority. The portable `report/` bundle preserves the completed briefing and executed enrichment. Its `manifest.json` verifies file integrity. The reader edition links to that bundle rather than modifying it. Source downloads contain citation metadata and links. Complete captured article text remains in the local research workspace and is not included in this portable report bundle.

## Implementation used

The recorded run used [`JamesHWade/tempest` PR #191](https://github.com/JamesHWade/tempest/pull/191),
branch `feat/enriched-researcher`, based on commit
`cbaf3faf9e6946e84797b5726686d4c2a7cf5060`. The run surfaced fixes to fetch
URL-only citations before extraction, pass bounded captured text to the
extractor, preserve full text during repeated native citations, and export
citation metadata rather than complete source documents in portable reports.
Those fixes are included in PR #191 but were not part of the recorded base
commit.

Validation: 331 assertions in the broader focused suite, 20 in the source-capture/context suite, and 69 in the enrichment suite passed (the broader run included two environment disk-cache fallback warnings). `pkgdown::check_pkgdown()` and `git diff --check` passed. Test totals overlap and should not be added as unique coverage. Browser checks and file integrity results are recorded in `result-validation.json` and `delivery-validation.json`.

## Files

- `report/analyses/plant_level_descriptive_profiles/analysis.R`: exact executed R code.
- `report/report.json`: execution metadata and original-to-archived filename mapping.
- `report/plan.json`: reviewed model-generated plan.
- `reader-synthesis.json`: final model-generated reader prose.
- `report/sources.json`: source reference index.
- `report/manifest.json`: checksums for the enrichment bundle.

The full local research checkpoint and provider run logs are retained outside this portable report bundle. Re-running the executed `analysis.R` requires only base R and its bundled datasets package. Keep the report folder beside `index.html` when moving the portable report.
