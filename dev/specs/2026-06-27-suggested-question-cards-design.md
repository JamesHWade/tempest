# Suggested-question cards — design

Date: 2026-06-27
Status: Proposed
Branch: evidence-ledger-s7

## Goal

In the tempest Chat tab, show clickable cards of suggested research questions so the
user always has a good next question to ask. The first set appears once the expert
panel has assembled; a fresh set is generated after each Moderator answer. Clicking a
card sends that question to the Moderator immediately.

## Decisions (settled during brainstorming)

- **Source:** a dedicated LLM call generates the questions (not the experts'
  `initial_questions`). The questions are phrased for the user, not for an expert's
  internal research.
- **Refresh:** regenerate after every Moderator answer, conditioned on the conversation
  so far, so suggestions act as follow-ups.
- **Click behaviour:** submit the question to the Moderator immediately.
- **Generation:** synchronous, reusing the existing `coordinator` model (`gpt-5-mini`),
  run in the same post-answer step as fact extraction and the mind-map update.
- **First cards:** generated after the panel assembles, so they can reflect the topic
  and the assembled experts. They appear with the "session started" intro.
- **Rendering:** shinychat's native suggestion cards (Approach A below).

## Non-goals (YAGNI)

- No reuse of persona `initial_questions` for the cards.
- No async/background generation in v1 (the post-answer pipeline is already synchronous).
- No custom card UI or CSS — we use shinychat's built-in rendering.
- No persistence of suggestions across sessions.
- No per-expert grouping of cards; a single flat set.

## Approach

shinychat 0.4.0.9000 renders a markdown list as a grid of clickable cards when every
item is a `<span class="suggestion">`. The span's text becomes both the label and the
value sent on click. Adding the `submit` class makes the click send immediately rather
than only filling the input. We emit such a list as a chat message and let shinychat do
the rest.

Approach A (chosen): native in-chat cards. Append the suggestion list as an assistant
message; refresh by appending a new list after each answer. Older sets scroll up and
stay clickable. Minimal code, native click→submit wiring.

Approach B (rejected): a fixed card strip above the input that replaces its contents
each turn. Avoids stale cards but requires re-implementing both the card styling and the
click→submit JS that shinychat already provides. More surface, more to break.

## Components

### 1. `tempest_suggest_questions()` — `R/suggestions.R` (new, exported)

Pure, testable generator.

```r
tempest_suggest_questions(
  topic,
  context = NULL,   # character: recent conversation, or NULL for the first set
  n = 4,
  chat = NULL,      # an ellmer Chat; if NULL, built from `config`
  config = tempest_config()
) -> character()     # length <= n, may be character(0)
```

Behaviour:

- Builds a prompt from `topic` + optional `context` and requests a structured result
  with `ellmer::type_array(ellmer::type_string())` via `chat$chat_structured()`.
- Returns a trimmed, de-duplicated, non-empty character vector (at most `n`).
- Attaches **no web/retrieval tools** to the chat — this is a plain completion, so it
  cannot reach the `ragnar::read_as_markdown()` path that has no timeout.
- The chat is configured with a bounded request timeout (see Error handling).
- On any error/timeout, returns `character(0)` (caller shows no cards).

System prompt: `inst/prompts/question_suggester_system.md` (new). Instructs the model to
produce `n` concise, specific, **answerable** questions a curious reader would ask next;
diverse across sub-topics; not already answered in `context`; no numbering or preamble.

### 2. `ses$suggest_questions(n = 4)` — `R/costorm.R` (new method on TempestSession)

Thin convenience wrapper: assembles recent transcript context from the session, then
calls `tempest_suggest_questions(topic = self$topic, context = ..., n = n, chat = <coordinator>, config = self$config)`.
Keeps the Shiny module thin and gives one place to decide how much context to pass.

### 3. Shiny integration — `inst/shiny/R/mod_chat.R`

- `suggestion_cards(questions)`: helper that returns the markdown list, each item
  `<span class="suggestion submit">QUESTION</span>`, under a short lead-in
  (e.g. "**You might ask:**"). Returns `NULL`/empty when `questions` is empty.
- `append_suggestions(ses, append_chat, n)`: calls `ses$suggest_questions(n)` inside
  `tryCatch` and appends the cards via `append_chat()`; does nothing on failure.
- **Initial set:** in `observeEvent(input$start)`, immediately after `create_session()`
  succeeds and the intro is appended, call `append_suggestions()` — gated on the toggle.
  This is independent of warmup: if warmup runs (an async promise chain), its progress
  messages stream in afterward; the cards still appear with the intro.
- **Refresh:** inside the existing `observeEvent(chat$last_turn())`, after recording the
  turn and running extract/mindmap, call `append_suggestions()` — gated on the toggle
  and on `nzchar(msg)` (see Data flow).
- **Toggle:** sidebar `checkboxInput(ns("suggest"), "Suggest follow-up questions", TRUE)`.

## Data flow

1. User clicks Start → session + personas created → intro appended → first card set
   appended.
2. User clicks a card → shinychat submits it as a user message to the Moderator client.
3. Moderator answers → `chat$last_turn()` fires → observer records the turn, extracts
   facts, updates the mind map, then appends a fresh card set built from the updated
   conversation.
4. Repeat from step 2.

### Loop guard (critical)

Appending cards must not re-trigger the per-turn observer and cause infinite
regeneration. The observer already computes `msg <- chat_input_text(chat$last_input())`.
Card appends carry no user input, so we gate refresh generation on `nzchar(msg)` — i.e.
only regenerate in response to a real user turn. The initial set is produced directly in
the Start handler, not via the observer, so it is unaffected.

(Assumption to verify in implementation: `chat$append(role = "assistant")` does not
itself fire `chat$last_turn()`. The `nzchar(msg)` gate makes the loop safe even if it
does.)

## Error handling / safety

This feature adds an LLM call to each turn. Applying the lesson from the warmup hang
(an untimed `ragnar` call blocking the event loop):

- The suggester chat sets an explicit request **timeout** so a stalled call cannot freeze
  the app.
- Generation is wrapped in `tryCatch`; any error or timeout yields **no cards** and never
  interrupts the answer or the post-turn pipeline — the same quiet-failure pattern used
  by `extract_facts()` and `update_mindmap()` in `record_warmup_turn()`.
- The suggester has no retrieval tools, so it cannot enter the untimed fetch path.

## Toggle behaviour

`Suggest follow-up questions` (default on). When off, neither the initial set nor the
per-turn refresh runs, so users can avoid the extra per-turn model cost.

## Testing

- `tempest_suggest_questions()` with a mocked ellmer chat (`tests/testthat/helper-mocks.R`):
  - normal structured result → trimmed, de-duplicated vector of length ≤ `n`;
  - malformed/empty model output → `character(0)`;
  - error from the chat → `character(0)` (no throw).
- `suggestion_cards()` builder: produces a markdown list whose items each carry
  `class="suggestion submit"`; returns empty for no questions.
- Light module check in `tests/testthat/test-shiny-app.R` that the toggle exists and the
  helpers wire up (consistent with existing app tests).
- `NEWS.md` entry; roxygen for the exported function; `NAMESPACE` regenerated.

## Files

New:
- `R/suggestions.R`
- `inst/prompts/question_suggester_system.md`
- tests in `tests/testthat/` (suggestions; builder)

Edited:
- `R/costorm.R` (add `suggest_questions()` method)
- `inst/shiny/R/mod_chat.R` (toggle, helpers, initial + refresh calls)
- `NEWS.md`
- `NAMESPACE` (via roxygen)

## Risks / open items

- Verify the `chat$last_turn()` vs `chat$append()` interaction (covered by the
  `nzchar(msg)` guard regardless).
- Confirm `chat$chat_structured()` exists in the installed ellmer; if not, fall back to a
  plain completion returning one question per line, parsed into a vector.
- Per-turn latency: cards appear a beat after the answer finishes streaming. Acceptable
  per the chosen sync design; revisit async only if it feels slow in practice.
