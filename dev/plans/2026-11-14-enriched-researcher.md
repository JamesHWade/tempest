# Enriched research reports

Build on the scientific review/repair branch without changing its publication
authority. Existing STORM and Co-STORM products remain the evidence authority.
Enrichment is a derived, portable report, never automatic accepted knowledge.

## Product flow

1. `tempest_researcher(topic, output_dir, ..., enrichment = list())` calls
   `tempest_run()` then `tempest_enrich_report()` with the enrichment options.
2. `tempest_enrich_report(x, output_dir, plan = NULL, planner = NULL,
   executor = NULL, image_generator = NULL, allow_code = FALSE,
   max_analyses = 3L, max_illustrations = 2L)` validates a completed product.
   A supplied plan is deterministic; otherwise a dsprrr structured planner
   proposes bounded analyses and illustrations from the report and sources.
3. Validate the entire plan before side effects. Code execution is opt-in.
   Providers are injected; `tempest_r_executor()` supplies a Deputy-backed
   trusted R process, and `tempest_image_generator()` supplies an OpenAI image
   adapter. These factories return callbacks; tests use offline callbacks.
4. Execute each task once, retaining succeeded/failed/skipped status. Failures
   are visible and never silently promoted into findings. No autonomous retry.
5. Write a new directory atomically with HTML, original research Markdown,
   plan, sources, analysis code/output/data/plots, illustrations and manifest.
   Refuse overwrites. Validate every returned file against the task directory;
   reject links, escapes, missing files, oversized files, active image formats.
6. Return an S7 `TempestEnrichedReport` with `path`, `status`, `manifest`.
   `tempest_read_enriched_report(path)` verifies hashes before reading.

## Shared implementation contract

Plan is a named list with exactly `title`, `summary`, `analyses`, `illustrations`.
Both collections are lists (empty allowed). Analysis fields are exactly `id`,
`title`, `question`, `code`, `source_ids`, `assumptions`. Illustration fields
are exactly `id`, `title`, `prompt`, `alt`, `caption`. All scalar strings must
be nonempty; source_ids and assumptions are character vectors. IDs must match
`^[a-z][a-z0-9_-]{0,47}$` and be unique across both collections. Analysis source
IDs must exist in the product. Summary is labeled a proposed synthesis, not a
new verified claim. No model-provided filenames or HTML are trusted.

Executor callback: `function(task, directory)` returns list with exactly
`output` (one string), `files` (relative paths), `metadata` (plain JSON list).
Writes only into its provided task directory by contract. A failed computation
must throw, not return success-looking error output. Core catches and records
a bounded safe error label. Built-in executor uses Deputy, not a new runtime.
`tempest_r_executor(timeout = 60)` returns this callback.

Image callback: `function(task, directory)` returns list with exactly `file`
(relative PNG/JPEG/WebP path) and `metadata` (plain JSON list with provider,
model, revised_prompt as available). `tempest_image_generator(model =
"gpt-image-2.6-flare", size = "1536x1024", quality = "medium")` returns this
callback; model is explicitly configurable. Credentials are resolved at
invocation and never retained. The default follows current OpenAI Images API
model guidance.

Renderer internal: `tempest_enriched_render(bundle, directory)` writes
`index.html` and returns invisibly its path. Bundle plain list fields: `title`,
`summary`, `research_md`, `sources` (data frame), `analyses`, `illustrations`,
`status`. Each task result combines the plan fields with `status`, `output`
(analysis only), `files` (relative bundle paths), `metadata`, `error` (NULL or
safe text). Image tasks also have `file` (NULL or relative bundle path).
Renderer must escape all strings, render Markdown safely, disable raw HTML and
remote image embeds, use only validated local images, include methods/source
links and downloadable code/manifest. No CDN or external runtime required.

## Acceptance

Offline tests cover complete STORM and finalized Co-STORM input, invalid plans,
unknown sources, budget bounds, opt-in execution, failed callbacks, unsafe paths,
tampered files, and preserved research authority. A worked example executes
real R analysis with charts and uses a genuinely generated illustration.
Inspect its desktop/mobile report, run focused/full tests, Air, pkgdown and R
CMD check, obtain independent Astra review, then publish a native stacked PR
and follow CI. Comparison with Copilot is capability-based, not an unsupported
claim of measured superiority.

## Qualification record

The implementation is based on parent PR #190 at
`8de5d1a570a379865db7e87dcdef3e427bd5482c`. Its public surface adds five functions
and preserves the parent's scientific review and publication contracts.

- The focused enrichment suite passes 150 expectations without warnings or
  skips, including real offline Deputy sessions, both product modes, bounded
  structured planning, unsafe files, integrity checks and the botanical example.
- R CMD check passed with zero errors, warnings and notes after replacing raw
  non-ASCII source strings with Unicode escapes, including the final filename
  provenance addition. The full test suite is running independently.
- The full pkgdown site builds. Browser inspection at 390 px identified a
  min-content overflow; the corrected report has equal viewport and scroll
  widths, including with expanded methods. Citation fragments resolve, and
  images retain explicit computed/illustrative labels.
- An independent Astra review accepted the core/adapters/renderer. Its separate
  scientific review recomputed the paired botanical contrasts and requested
  prominent offline-replay disclosure and opt-out code execution in the live
  vignette example; both were corrected.
- The actual `gpt-image-2.6-flare` image API and real Deputy executor completed
  together: two succeeded analyses and one succeeded image, followed by a
  successful integrity read. Live credentials are never in report metadata.
- Live structured planning was exercised using `gpt-5-mini` and the package's
  default writer, `gpt-5.6-sol`. Shape validation passed, but review rejected
  unsuitable inference or an unsupported plant-pair crosswalk. These proposed
  analyses were not executed or accepted as evidence. Planning instructions
  now explicitly forbid inferred pairing and unsupported inference. This is
  an observed limit of model-proposed analysis, not a completed accuracy
  benchmark; reviewed code remains necessary for scientific acceptance.
- The distributed botanical example uses an authored offline research replay,
  actual seeded R calculations, and a genuinely generated illustration. It
  records the original paper's pair mapping and conflicting units/protocol
  descriptions, and archives the exact 84-row input table and output filename
  crosswalk. It is not a live research-quality evaluation or a Copilot benchmark.
