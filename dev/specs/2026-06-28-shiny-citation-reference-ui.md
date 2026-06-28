# Shiny citation and reference UI

## Context

Tempest report and Co-STORM answer text can contain Tempest source markers such
as `[Sxxxxxxxxxxxx]`. Package reports can also contain Markdown footnote markers
such as `[^Sxxxxxxxxxxxx]` when `tempest_report_md()` has already assembled a
plain Markdown report. The Shiny app needs a readable rendering that keeps those
markers linked to the `SourceStore` without changing the package-level Markdown
contract.

## Display contract

1. Parse inline Tempest source markers from the rendered Markdown surface. The
   app accepts both `[Sxxxxxxxxxxxx]` and `[^Sxxxxxxxxxxxx]` as citations.
2. Strip Tempest-generated Markdown footnote definitions before app rendering
   when a report already has a `## References` section made only of Tempest
   footnotes. The app replaces that plain Markdown reference section with its
   richer panel.
3. Deduplicate source ids in first-appearance order. The first cited source is
   reference `[1]`, the next distinct source is `[2]`, and repeated citations
   reuse the same number.
4. Resolve each source id against the relevant `SourceStore`. Known sources show
   title, domain or URL, snippet, provenance, and an open-source link. Missing
   sources remain visible as flagged references rather than being dropped.
5. Render inline citations as links to the matching reference item. Each marker
   keeps a stable source id in its DOM id/href and exposes a title preview for
   hover/focus inspection.
6. Render the references panel only from the cited ids on that surface, not from
   every source collected in the session. The existing Sources tab remains the
   all-sources inventory.

## Surfaces

- STORM reports store `result$store` alongside the generated Markdown so the
  Report tab and HTML download can render linked references.
- Co-STORM generated reports store `ses$store` alongside the generated Markdown
  for the same Report tab and HTML download behavior.
- Co-STORM transcript turns render through the same citation-aware Markdown
  helper after a turn completes.
- App-authored chat messages use the helper when appended to shinychat. Live
  streamed moderator responses remain owned by shinychat until a documented
  post-render/update hook exists.

## Tests

- Unit-level Shiny helper tests cover single, repeated, multiple, and unknown
  source ids.
- Report module tests verify that only cited sources appear in the reference
  panel and that inline markers link to the matching reference.
- Transcript module tests verify that completed Co-STORM answer text renders
  citation markers through the shared helper.
