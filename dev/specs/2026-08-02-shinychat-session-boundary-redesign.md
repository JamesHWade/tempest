# Simplify the Tempest session and shinychat boundary

## Outcome

Tempest owns Co-STORM execution, semantic transcript state, evidence enrichment,
mind-map updates, artifacts, events, cancellation, and restoration. `shinychat`
owns chat input, message presentation, and streaming. The bundled Shiny app is a
thin adapter between those two contracts.

Backward compatibility is not required. Existing behavior remains protected by
contract tests while redundant implementation paths are removed.

## Work items

- [x] Define a package-owned asynchronous turn-processing contract with typed,
  validated results and deterministic fake-chat tests.
- [x] Move asynchronous Co-STORM warmup orchestration from `mod_chat.R` into the
  package, including timeouts, cancellation guards, evidence commitment,
  mind-map updates, and progress events.
- [x] Introduce a narrow internal shinychat adapter for client switching,
  transcript restoration, post-turn dispatch, and compatibility checks.
- [x] Rewire the bundled Chat module to call the package-owned session APIs and
  remove its duplicate async orchestration.
- [x] Make one artifact catalog the canonical owner of Co-STORM reports and
  mind-map artifacts; remove new writes to the session artifact environment.
- [x] Unify generic run and Co-STORM progress behind an adapter-friendly event
  query without changing the UI event presentation in this slice.
- [x] Update documentation and `NEWS.md` for the breaking session/Shiny API.
- [x] Run formatting, focused tests, the full test suite, pkgdown validation,
  and `devtools::check(document = FALSE, error_on = "warning")`.

## Constraints

- Do not modify or stage the pre-existing deleted `stormr.Rproj` or
  `tempest_recommendations.md` files.
- Do not modify or stage the pre-existing untracked `.github` instruction
  files.
- Tests must not use API keys, network access, or live provider responses.
- Keep provider clients and other live services out of serializable S7 values.
- Keep mutable execution state in small, encapsulated R6 objects.
- Do not retain deprecated wrappers solely for compatibility.
