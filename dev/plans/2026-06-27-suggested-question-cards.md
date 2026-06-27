# Suggested-Question Cards Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Show clickable cards of LLM-suggested research questions in the tempest Chat tab — an initial set after the panel assembles and a fresh set after every Moderator answer — where clicking a card submits that question to the Moderator.

**Architecture:** A new exported generator `tempest_suggest_questions()` makes a tool-free `chat_structured()` call (coordinator model) and returns a character vector. A thin `TempestSession$suggest_questions()` method supplies conversation context. The Shiny chat module renders the vector as shinychat native suggestion cards (`<span class="suggestion submit">`) and refreshes them, gated by a sidebar toggle.

**Tech Stack:** R, ellmer 0.4.1 (`chat_structured`, `type_object`/`type_array`), shinychat 0.4.0.9000 (suggestion cards), bslib, R6, S7, testthat 3.

Spec: `dev/specs/2026-06-27-suggested-question-cards-design.md`

---

## File Structure

- **Create `R/suggestions.R`** — the generator and its helpers:
  - `tempest_suggest_questions()` (exported) — topic + optional context → character vector.
  - `tempest_suggest_questions_prompt()` (internal) — builds the user-turn prompt.
  - `tempest_type_suggested_questions()` (internal) — the ellmer structured-output schema.
- **Create `inst/prompts/question_suggester_system.md`** — system prompt for the generator.
- **Create `tests/testthat/test-suggestions.R`** — unit tests for the generator and the session method.
- **Modify `R/costorm.R`** — add the `suggest_questions()` method to `TempestSession`.
- **Modify `inst/shiny/R/mod_chat.R`** — sidebar toggle, `suggestion_cards()` builder, and the initial + refresh wiring.
- **Modify `tests/testthat/test-shiny-app.R`** — tests for `suggestion_cards()` and the toggle.
- **Modify `NEWS.md`** — changelog entry.

Existing helpers reused (no changes): `tempest_make_chat()`, `tempest_prompt()`, `tempest_trim()` (vectorized, `R/utils.R`), `tempest_as_character_vector()` (`R/storm.R`), `%||%`, `tempest_require()`.

Key facts that make this safe:
- The Shiny config (`inst/shiny/R/mod_config.R`) sets no `tools`, so `config@tools` is NULL and `tempest_make_chat(config, "coordinator", ...)` produces a **tool-free** chat — it cannot reach the untimed `fetch_url`/`ragnar` path.
- `chat$append(role = "assistant")` does **not** fire `chat$last_turn()` (the existing module already appends intro/warmup/report messages without the per-turn observer running), so appending cards will not recurse. The `nzchar(msg)` gate is an extra guard.

---

## Task 1: Generator `tempest_suggest_questions()`

**Files:**
- Create: `R/suggestions.R`
- Create: `inst/prompts/question_suggester_system.md`
- Test: `tests/testthat/test-suggestions.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-suggestions.R`:

```r
test_that("tempest_suggest_questions trims, drops empties, and de-duplicates", {
  chat <- fake_chat(structured = list(list(questions = c(
    " What is animatronics? ",
    "What is animatronics?",
    "How do servos work?",
    ""
  ))))
  out <- tempest_suggest_questions("Animatronics", chat = chat, n = 4)
  expect_equal(out, c("What is animatronics?", "How do servos work?"))
})

test_that("tempest_suggest_questions caps the result at n", {
  chat <- fake_chat(structured = list(list(questions = paste0("Q", 1:10))))
  out <- tempest_suggest_questions("Topic", chat = chat, n = 3)
  expect_equal(out, c("Q1", "Q2", "Q3"))
})

test_that("tempest_suggest_questions returns empty for a blank topic", {
  expect_equal(tempest_suggest_questions("", chat = fake_chat()), character())
  expect_equal(tempest_suggest_questions("   ", chat = fake_chat()), character())
})

test_that("tempest_suggest_questions returns empty when the chat errors", {
  # fake_chat with an empty structured queue stops() on chat_structured()
  expect_equal(tempest_suggest_questions("Topic", chat = fake_chat()), character())
})

test_that("tempest_suggest_questions returns empty when 'questions' is missing", {
  chat <- fake_chat(structured = list(list(other = "x")))
  expect_equal(tempest_suggest_questions("Topic", chat = chat), character())
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-suggestions.R")'`
Expected: FAIL — `could not find function "tempest_suggest_questions"`.

- [ ] **Step 3: Create the system prompt**

Create `inst/prompts/question_suggester_system.md`:

```markdown
You suggest follow-up research questions for a user who is exploring a topic with a panel of AI experts.

Given a topic and, when available, the conversation so far, propose concise, specific questions the user could ask next to deepen or broaden their understanding.

Guidelines:
- Each question must be self-contained and answerable through research.
- Favor diversity: cover different sub-topics, angles, or tensions rather than minor variations of one idea.
- Do not repeat questions that have already been answered in the conversation.
- Keep each question under about 15 words and phrase it the way the user would ask it.
- Do not number the questions or add any commentary.

Return the questions as structured data.
```

- [ ] **Step 4: Write the implementation**

Create `R/suggestions.R`:

```r
# Suggested follow-up questions for the tempest Chat tab.

#' @keywords internal
tempest_type_suggested_questions <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    questions = ellmer::type_array(
      ellmer::type_string(
        "A concise, self-contained question the user could ask next."
      )
    )
  )
}

#' @keywords internal
tempest_suggest_questions_prompt <- function(topic, context = NULL, n = 4) {
  parts <- c(paste0("Topic: ", topic), "")
  if (!is.null(context) && nzchar(context)) {
    parts <- c(
      parts,
      "Conversation so far:",
      context,
      "",
      paste0("Suggest ", n, " follow-up questions the user could ask next.")
    )
  } else {
    parts <- c(
      parts,
      paste0(
        "Suggest ",
        n,
        " questions a curious newcomer could ask to start exploring this topic."
      )
    )
  }
  paste(parts, collapse = "\n")
}

#' Suggest follow-up research questions for a topic
#'
#' Generates a short list of concise, user-facing questions to ask next about a
#' topic, optionally conditioned on the conversation so far. The tempest Shiny
#' app uses this to render clickable suggestion cards. The call is a single
#' tool-free structured completion: any error yields an empty result.
#'
#' @param topic The research topic.
#' @param context Optional character string with the recent conversation. When
#'   `NULL`, questions are generated from the topic alone.
#' @param n Maximum number of questions to return.
#' @param chat Optional ellmer Chat to use. When `NULL`, a tool-free chat is
#'   created from `config` using the coordinator model.
#' @param config A `TempestConfig` object.
#' @return A character vector of at most `n` questions (possibly empty).
#' @examples
#' \dontrun{
#' tempest_suggest_questions("History of jazz", n = 4)
#' }
#' @export
tempest_suggest_questions <- function(
  topic,
  context = NULL,
  n = 4,
  chat = NULL,
  config = tempest_config()
) {
  topic <- tempest_trim(topic %||% "")
  if (length(topic) != 1L || is.na(topic) || !nzchar(topic)) {
    return(character())
  }
  n <- as.integer(n)
  if (is.na(n) || n < 1L) {
    n <- 4L
  }
  tryCatch(
    {
      if (is.null(chat)) {
        tempest_require("ellmer", "Question suggestions require ellmer.")
        chat <- tempest_make_chat(
          config,
          "coordinator",
          system_prompt = tempest_prompt("question_suggester_system"),
          echo = "none"
        )
      }
      result <- chat$chat_structured(
        tempest_suggest_questions_prompt(topic, context, n),
        type = tempest_type_suggested_questions(),
        echo = "none",
        convert = FALSE
      )
      qs <- tempest_as_character_vector(result$questions %||% character())
      qs <- tempest_trim(qs)
      qs <- qs[!is.na(qs) & nzchar(qs)]
      qs <- unique(qs)
      utils::head(qs, n)
    },
    error = function(e) character()
  )
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-suggestions.R")'`
Expected: PASS (5 passing tests).

- [ ] **Step 6: Commit**

```bash
git add R/suggestions.R inst/prompts/question_suggester_system.md tests/testthat/test-suggestions.R
git commit -m "feat: add tempest_suggest_questions generator"
```

---

## Task 2: Card builder `suggestion_cards()`

**Files:**
- Modify: `inst/shiny/R/mod_chat.R` (add file-scope helper near `warmup_prompt`, ~line 311)
- Test: `tests/testthat/test-shiny-app.R`

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-shiny-app.R`:

```r
test_that("suggestion_cards builds a shinychat submit-card list", {
  app <- source_shiny_modules()
  md <- app$suggestion_cards(c("What is X?", "How does Y work?"))
  expect_type(md, "character")
  expect_match(md, "You might ask")
  expect_match(md, '- <span class="suggestion submit">What is X\\?</span>')
  expect_match(md, '- <span class="suggestion submit">How does Y work\\?</span>')
})

test_that("suggestion_cards returns NULL for no questions", {
  app <- source_shiny_modules()
  expect_null(app$suggestion_cards(character()))
  expect_null(app$suggestion_cards(c("", NA_character_)))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-shiny-app.R")'`
Expected: FAIL — `attempt to apply non-function` / `app$suggestion_cards` is NULL.

- [ ] **Step 3: Write the implementation**

In `inst/shiny/R/mod_chat.R`, add this helper immediately before `warmup_prompt <- function(...)` (currently ~line 311):

```r
# Build a shinychat suggestion-card block from a vector of questions. shinychat
# renders a markdown list whose items are all `<span class="suggestion">` as a
# grid of cards; the `submit` class makes a click send the question immediately.
# Returns NULL when there are no usable questions.
suggestion_cards <- function(questions, lead = "**You might ask:**") {
  questions <- questions[!is.na(questions) & nzchar(questions)]
  if (length(questions) == 0) {
    return(NULL)
  }
  items <- paste0(
    "- <span class=\"suggestion submit\">",
    questions,
    "</span>"
  )
  paste0(lead, "\n\n", paste(items, collapse = "\n"))
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-shiny-app.R")'`
Expected: PASS (including the two new tests).

- [ ] **Step 5: Commit**

```bash
git add inst/shiny/R/mod_chat.R tests/testthat/test-shiny-app.R
git commit -m "feat: add suggestion_cards builder for the chat module"
```

---

## Task 3: Session method `TempestSession$suggest_questions()`

**Files:**
- Modify: `R/costorm.R` (add method to `TempestSession` public list, after `extract_facts`, ~line 352)
- Test: `tests/testthat/test-suggestions.R`

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-suggestions.R`:

```r
test_that("TempestSession$suggest_questions delegates to the generator", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(list(questions = c("Q1", "Q2"))))
    }
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    personas = list(list(id = 1, name = "Dr. A", title = "Sci", perspective = "X"))
  )
  out <- ses$suggest_questions(n = 2)
  expect_equal(out, c("Q1", "Q2"))
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-suggestions.R")'`
Expected: FAIL — `attempt to apply non-function` (no `suggest_questions` method).

- [ ] **Step 3: Write the implementation**

In `R/costorm.R`, find the `extract_facts` method (ends at the `},` after `invisible(TRUE)`, ~line 352). Insert this method immediately after it:

```r
    #' @description
    #' Suggest follow-up questions for the user based on the conversation so far.
    #' @param n Maximum number of questions to return.
    #' @return A character vector of questions (possibly empty).
    suggest_questions = function(n = 4) {
      context <- if (length(self$transcript) > 0) {
        self$transcript_markdown(max_turns = 12)
      } else {
        NULL
      }
      tempest_suggest_questions(
        topic = self$topic,
        context = context,
        n = n,
        config = self$config
      )
    },
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-suggestions.R")'`
Expected: PASS (all 6 tests).

- [ ] **Step 5: Commit**

```bash
git add R/costorm.R tests/testthat/test-suggestions.R
git commit -m "feat: add TempestSession\$suggest_questions method"
```

---

## Task 4: Wire cards into the chat module (toggle + initial + refresh)

**Files:**
- Modify: `inst/shiny/R/mod_chat.R` (UI checkbox; `maybe_append_suggestions` closure; calls in the `input$start` and `chat$last_turn()` observers)
- Test: `tests/testthat/test-shiny-app.R` (toggle presence) + manual verification

- [ ] **Step 1: Write the failing test (toggle present in the UI)**

Add to `tests/testthat/test-shiny-app.R`:

```r
test_that("the chat sidebar offers a follow-up-questions toggle", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )
  expect_match(html, "chat-suggest")
  expect_match(html, "Suggest follow-up questions")
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-shiny-app.R")'`
Expected: FAIL — no match for `chat-suggest`.

- [ ] **Step 3: Add the toggle to the UI**

In `inst/shiny/R/mod_chat.R`, in `mod_chat_ui`, insert this checkbox immediately after the existing `warmup` `checkboxInput(...)` block (the one ending `FALSE\n        ),`, ~line 28) and before the `input_task_button(ns("start"), ...)`:

```r
        shiny::checkboxInput(
          ns("suggest"),
          "Suggest follow-up questions",
          TRUE
        ),
```

- [ ] **Step 4: Add the `maybe_append_suggestions` closure**

In `inst/shiny/R/mod_chat.R`, in `mod_chat_server`, immediately after the line `append_chat <- function(text) chat$append(text, role = "assistant")` (~line 89), add:

```r

    # Generate + append suggestion cards, gated on the sidebar toggle. Quiet on
    # failure: a stalled or erroring generator simply shows no cards.
    maybe_append_suggestions <- function(ses, n = 4) {
      if (is.null(ses) || !isTRUE(input$suggest)) {
        return(invisible(NULL))
      }
      questions <- tryCatch(
        ses$suggest_questions(n),
        error = function(e) character()
      )
      cards <- suggestion_cards(questions)
      if (!is.null(cards)) {
        append_chat(cards)
      }
      invisible(NULL)
    }
```

- [ ] **Step 5: Call it on session start**

In `inst/shiny/R/mod_chat.R`, replace the body of `observeEvent(input$start, {...})` (currently ~lines 125-140) with:

```r
    shiny::observeEvent(input$start, {
      topic <- stringi::stri_trim_both(input$topic %||% "")
      if (!nzchar(topic)) {
        bslib::update_task_button("start", state = "ready")
        return()
      }
      ses <- create_session(topic, input$n_experts %||% 3)
      if (is.null(ses)) {
        bslib::update_task_button("start", state = "ready")
        return()
      }
      maybe_append_suggestions(ses)
      if (!isTRUE(input$warmup) || length(ses$personas) == 0) {
        bslib::update_task_button("start", state = "ready")
        return()
      }
      promises::finally(
        run_warmup(ses, store, append_chat),
        function() bslib::update_task_button("start", state = "ready")
      )
    })
```

- [ ] **Step 6: Refresh after each Moderator answer**

In `inst/shiny/R/mod_chat.R`, in `observeEvent(chat$last_turn(), {...})`, the handler ends with `store$touch()` (~line 176). Append a refresh call right after it, still inside the observer:

```r
      store$touch()
      if (nzchar(msg)) {
        maybe_append_suggestions(ses)
      }
```

(`msg` and `ses` are already in scope in that observer. The `nzchar(msg)` gate means cards regenerate only after a real user turn, never from the card append itself.)

- [ ] **Step 7: Run the UI test + full module tests**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-shiny-app.R")'`
Expected: PASS (including the toggle test).

- [ ] **Step 8: Manual verification (interactive behavior can't be unit-tested)**

Run the app and confirm the loop end-to-end:

Run: `R -q -e 'devtools::load_all(quiet = TRUE); tempest::run_app()'`
Then in the browser:
1. Enter a topic (e.g. "Animatronics past, present, and future"), leave "Suggest follow-up questions" on, leave warmup off, click **Start Session**.
2. Confirm a "You might ask:" card grid appears under the session intro.
3. Click a card — confirm the question is sent to the Moderator and an answer streams in.
4. After the answer, confirm a **new** card set appears, and that clicking another card does not trigger runaway regeneration.
5. Start a fresh session with the toggle **off** — confirm no cards appear and no extra model calls happen per turn.

Expected: all five behaviors hold. Note in the commit/PR if step 4 ever loops (would indicate `chat$append` unexpectedly firing `last_turn()` — the `nzchar(msg)` guard should prevent it).

- [ ] **Step 9: Commit**

```bash
git add inst/shiny/R/mod_chat.R tests/testthat/test-shiny-app.R
git commit -m "feat: render and refresh suggestion cards in the chat tab"
```

---

## Task 5: Docs, changelog, and full verification

**Files:**
- Modify: `NEWS.md`
- Generated: `NAMESPACE`, `man/tempest_suggest_questions.Rd`

- [ ] **Step 1: Add a NEWS entry**

Open `NEWS.md`, and under the top development-version heading add this as the first bullet:

```markdown
* The Chat tab now suggests follow-up questions as clickable cards. A set appears
  when the expert panel assembles and refreshes after each answer; clicking a card
  sends that question to the Moderator. Toggle it off with "Suggest follow-up
  questions" in the sidebar. New exported helper `tempest_suggest_questions()`.
```

- [ ] **Step 2: Regenerate documentation**

Run: `R -q -e 'devtools::document()'`
Expected: writes `man/tempest_suggest_questions.Rd` and adds `export(tempest_suggest_questions)` to `NAMESPACE`. No errors.

- [ ] **Step 3: (Optional) format changed files if `air` is available**

Run: `air format R/suggestions.R R/costorm.R inst/shiny/R/mod_chat.R 2>/dev/null || echo "air not available; skipping"`
Expected: files formatted, or a skip message. Re-run the relevant tests if anything reformatted.

- [ ] **Step 4: Run the full test suite**

Run: `R -q -e 'devtools::test()'`
Expected: all tests pass (0 failures), including the new `test-suggestions.R` and the additions to `test-shiny-app.R`.

- [ ] **Step 5: Commit**

```bash
git add NEWS.md NAMESPACE man/
git commit -m "docs: document tempest_suggest_questions and note the feature in NEWS"
```

- [ ] **Step 6: (Recommended) R CMD check**

Run: `R -q -e 'devtools::check(args = "--no-manual")'`
Expected: 0 errors, 0 warnings. (A NOTE about the new export is acceptable.) Fix any new check findings before opening a PR.

---

## Self-review notes (author)

- **Spec coverage:** generator (Task 1), session sugar (Task 3), card rendering + toggle + initial + refresh + loop guard (Tasks 2, 4), error handling/no-tools safety (Task 1 implementation), testing + NEWS + docs (Tasks 1-5). All spec sections map to a task.
- **Type/name consistency:** `tempest_suggest_questions`, `tempest_suggest_questions_prompt`, `tempest_type_suggested_questions`, `suggestion_cards`, `maybe_append_suggestions`, `suggest_questions` (method), input id `suggest` (namespaced `chat-suggest`) used consistently across tasks.
- **No placeholders:** every code and test block is complete; commands include expected output.
