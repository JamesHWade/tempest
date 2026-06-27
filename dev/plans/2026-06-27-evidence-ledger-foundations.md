# Evidence Ledger + S7 Records Implementation Plan (Phase 0 + Phase 1)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn tempest's untyped fact list into a validated, claim-centered evidence ledger with claim-level citation verification, on a foundation of split modules and CI-runnable mock tests.

**Architecture:** New value records (claim, evidence span, dispute, source) become S7 classes with property validation. `SourceStore` (still R6) gains claim/evidence/dispute containers and indexes. A judge-backed verifier labels each claim's citation support. `TempestConfig` becomes S7 with validated enum fields driving a `citation_policy` that gates verification and report rendering. The four stateful R6 classes stay R6.

**Tech Stack:** R6 (stateful classes), S7 0.2.x (value records + config), ellmer (LLM + structured output), dsprrr (optimizable modules), testthat 3e (tests), digest (IDs).

**Spec:** `dev/specs/2026-06-27-evidence-ledger-s7-design.md`

---

## Conventions for every task

- **Run one test file:** `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-<name>.R", reporter = "summary")'`
- **Run the whole suite:** `R -q -e 'devtools::test()'`
- **Regenerate docs/NAMESPACE after roxygen changes:** `R -q -e 'devtools::document()'`
- **Commit cadence:** one commit per task (after its tests pass). Stage only the files the task names.
- **Branch:** all work lands on `evidence-ledger-s7` (already checked out).
- **TDD:** write the failing test first, watch it fail, implement the minimum, watch it pass, commit.

---

# Phase 0 — Foundations

## Task 0A: Mock-LLM test harness

Build the shared fakes first; every later task tests against them. (We do the harness before the `storm.R` split because the harness has no dependency on file layout and unblocks all Phase 1 testing.)

**Files:**
- Create: `tests/testthat/helper-mocks.R` (testthat auto-sources `helper-*.R` before tests)

- [ ] **Step 1: Write the helper file**

```r
# tests/testthat/helper-mocks.R
# Reusable fakes so ledger/verification logic is testable without network or API keys.

# A fake ellmer-style chat. $chat_structured() pops from a scripted queue of
# return values; $chat() returns scripted plain text. Records calls for asserts.
fake_chat <- function(structured = list(), text = list()) {
  state <- new.env(parent = emptyenv())
  state$structured <- structured
  state$text <- text
  state$calls <- list()
  list(
    chat_structured = function(prompt, type = NULL, ...) {
      state$calls <- c(state$calls, list(list(kind = "structured", prompt = prompt)))
      if (length(state$structured) == 0) {
        stop("fake_chat: structured queue exhausted")
      }
      out <- state$structured[[1]]
      state$structured <- state$structured[-1]
      out
    },
    chat = function(prompt, ...) {
      state$calls <- c(state$calls, list(list(kind = "text", prompt = prompt)))
      if (length(state$text) == 0) return("")
      out <- state$text[[1]]
      state$text <- state$text[-1]
      out
    },
    register_tools = function(...) invisible(NULL),
    .calls = function() state$calls
  )
}

# A fake judge that returns a fixed verification verdict for every claim.
fake_judge <- function(status = "supported", score = 0.9, rationale = "matches source") {
  fake_chat(structured = list())  # placeholder; verdict queue set per-test via fake_verdicts()
}

# Build a queue of verdicts (one per claim) for tempest_verify_claims tests.
fake_verdicts <- function(...) {
  verdicts <- list(...)
  fake_chat(structured = verdicts)
}

# Fixture builders.
fake_source <- function(url = "https://example.org/a", title = "Example A",
                        content_text = "Photosynthesis converts light to chemical energy.") {
  tempest_source(url = url, title = title, content_text = content_text,
                 fetched_at = "2026-01-01 00:00:00 UTC")
}

fake_store_with_sources <- function(n = 2) {
  store <- SourceStore$new()
  for (i in seq_len(n)) {
    store$upsert_source(fake_source(
      url = paste0("https://example.org/", i),
      title = paste("Example", i),
      content_text = paste("Body text for source", i)
    ))
  }
  store
}
```

- [ ] **Step 2: Write a smoke test for the harness**

Create `tests/testthat/test-helper-mocks.R`:

```r
test_that("fake_chat scripts structured and text returns", {
  ch <- fake_chat(structured = list(list(facts = list())), text = list("hi"))
  expect_equal(ch$chat_structured("p", type = NULL), list(facts = list()))
  expect_equal(ch$chat("p"), "hi")
  expect_length(ch$.calls(), 2)
})

test_that("fake_store_with_sources builds a populated store", {
  store <- fake_store_with_sources(3)
  expect_s3_class(store, "SourceStore")
  expect_length(store$list_sources(), 3)
})
```

- [ ] **Step 3: Run the smoke test**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-helper-mocks.R", reporter = "summary")'`
Expected: PASS (2 tests).

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/helper-mocks.R tests/testthat/test-helper-mocks.R
git commit -m "test: add mock-LLM harness and fixtures"
```

---

## Task 0B: Split `storm.R` by pipeline stage

Pure code move — R sources every `R/*.R` file into one namespace, so relocating function definitions changes nothing at runtime. Verification is: `devtools::load_all()` succeeds and the full suite still passes.

**Files:**
- Create: `R/storm-types.R`, `R/storm-perspectives.R`, `R/storm-research.R`, `R/storm-outline.R`, `R/storm-write.R`, `R/storm-polish.R`, `R/dsprrr.R`
- Modify: `R/storm.R` (remove moved functions; keep `tempest_run`, `tempest_run_async`)

- [ ] **Step 1: Move function definitions into new files**

Cut each function definition (the full `name <- function(...) { ... }` block plus its roxygen comment) from `R/storm.R` and paste it into the target file below. Add a one-line header comment at the top of each new file (e.g. `# STORM research stage`). Move these exact functions:

`R/storm-types.R`: `tempest_type_perspectives`, `tempest_type_personas`, `tempest_type_outline`, `tempest_type_fact_extract`, `tempest_type_next_question`, `tempest_type_query_decomposition`

`R/storm-perspectives.R`: `tempest_generate_personas`, `tempest_format_persona_details`, `tempest_render_expert_prompt`, `tempest_generate_perspectives`, `tempest_normalize_personas`, `tempest_normalize_perspectives`

`R/storm-research.R`: `tempest_generate_next_question`, `tempest_decompose_query`, `tempest_normalize_query_decomposition`, `tempest_normalize_fact_output`, `tempest_extract_facts_from_answer`, `tempest_research_one_perspective`, `tempest_research_parallel`

`R/storm-outline.R`: `tempest_draft_outline`, `tempest_refine_outline`, `tempest_prompt_lines`, `tempest_outline_summary`, `tempest_subsections_markdown`, `tempest_normalize_outline`

`R/storm-write.R`: `tempest_write_section`, `tempest_should_skip_section`, `tempest_sections_to_write`, `tempest_section_facts_text`, `tempest_section_jobs`, `tempest_run_section_job`, `tempest_write_sections_sequential`, `tempest_parallel_workers`, `tempest_setup_daemons`, `tempest_collect_parallel`, `tempest_write_sections_parallel`, `tempest_write_section_jobs`, `tempest_write_lead_section`, `tempest_summarize_facts_for_prompt`, `tempest_keyword_filter_facts`, `tempest_semantic_filter_facts`

`R/storm-polish.R`: `tempest_polish_rules`, `tempest_normalize_text_output`

`R/dsprrr.R`: `tempest_run_dsprrr_module`, `tempest_make_dsprrr_modules`, `tempest_default_dsprrr_teleprompter`, `tempest_call_dsprrr_teleprompter_factory`, `tempest_select_dsprrr_teleprompter`, `tempest_dsprrr_modules_path`, `tempest_save_dsprrr_modules`, `tempest_load_dsprrr_modules`, `tempest_optimize_dsprrr_modules`

Leave in `R/storm.R`: `tempest_run`, `tempest_run_async`, and `tempest_as_character_vector` (shared helper used across stages and by run-persistence).

- [ ] **Step 2: Verify the package still loads**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); message("loaded OK")'`
Expected: prints "loaded OK" with no errors about undefined functions.

- [ ] **Step 3: Run the full suite to confirm no behavior change**

Run: `R -q -e 'devtools::test()'`
Expected: same pass/fail counts as before the split (no new failures).

- [ ] **Step 4: Commit**

```bash
git add R/storm.R R/storm-types.R R/storm-perspectives.R R/storm-research.R R/storm-outline.R R/storm-write.R R/storm-polish.R R/dsprrr.R
git commit -m "refactor: split storm.R by pipeline stage"
```

---

## Task 0C: Surface dsprrr fallback instead of swallowing it

The current `tempest_run_dsprrr_module` warns then returns `NULL` on failure, but the warning text says "falling back" only when a module is *present*. Add a quiet, inspectable signal so evals can later tell when modules were used vs. skipped. Minimal change, no new behavior on the happy path.

**Files:**
- Modify: `R/dsprrr.R` (`tempest_run_dsprrr_module`)
- Test: `tests/testthat/test-dsprrr-fallback.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("tempest_run_dsprrr_module signals fallback on error", {
  skip_if_not_installed("dsprrr")
  bad_module <- structure(list(), class = "dsprrr_module")
  local_mocked_bindings(
    .package = "dsprrr",
    run = function(...) stop("boom")
  )
  expect_warning(
    out <- tempest:::tempest_run_dsprrr_module(bad_module, fake_chat(), list(x = 1), "test step"),
    "falling back"
  )
  expect_null(out)
})
```

- [ ] **Step 2: Run it to verify it fails**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-dsprrr-fallback.R", reporter = "summary")'`
Expected: FAIL only if `dsprrr` isn't installed (then it errors on `local_mocked_bindings`); if `dsprrr` is present it should already pass. If it fails for any other reason, fix before continuing.

- [ ] **Step 3: Confirm the implementation already satisfies it**

`tempest_run_dsprrr_module` already calls `tempest_warn("dsprrr {step} failed, falling back to ellmer: ...")`. No code change needed unless Step 2 failed; if it failed because the warning text differs, align the message to include the word "falling back". Keep the `NULL` return.

- [ ] **Step 4: Run the test to verify it passes**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-dsprrr-fallback.R", reporter = "summary")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/dsprrr.R tests/testthat/test-dsprrr-fallback.R
git commit -m "test: lock in dsprrr fallback warning"
```

---

# Phase 1 — Evidence ledger and citation verification

## Task 1A: S7 infrastructure

Wire S7 into the package so classes/methods register at load time.

**Files:**
- Modify: `DESCRIPTION` (add `S7` to Imports)
- Modify: `R/tempest-package.R` (add `.onLoad` calling `S7::methods_register()`)
- Test: `tests/testthat/test-s7-setup.R`

- [ ] **Step 1: Add S7 to DESCRIPTION Imports**

In `DESCRIPTION`, under `Imports:`, add `S7` (alphabetical position, after `R6`):

```
Imports:
    cli,
    digest,
    dsprrr,
    ellmer,
    fs,
    glue,
    httr2,
    purrr,
    R6,
    rlang (>= 1.0.0),
    S7,
    stringi,
    tibble
```

- [ ] **Step 2: Add `.onLoad` to register S7 methods**

In `R/tempest-package.R`, before the trailing `NULL`, add:

```r
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
```

- [ ] **Step 3: Write a smoke test**

```r
test_that("S7 is available and a trivial class validates", {
  cls <- S7::new_class("tmp_cls", properties = list(x = S7::class_numeric))
  obj <- cls(x = 1)
  expect_equal(obj@x, 1)
})
```

- [ ] **Step 4: Run it**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-s7-setup.R", reporter = "summary")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION R/tempest-package.R tests/testthat/test-s7-setup.R
git commit -m "feat: wire S7 into the package"
```

---

## Task 1B: S7 record classes (claim, evidence span, dispute, source)

**Files:**
- Create: `R/ledger-types.R`
- Test: `tests/testthat/test-ledger-types.R`

- [ ] **Step 1: Write failing tests for validation**

```r
test_that("tempest_claim validates enums and scores", {
  cl <- tempest_claim(claim_text = "Water boils at 100C", source_ids = "S0123456789ab")
  expect_equal(cl@claim_type, "finding")
  expect_equal(cl@verification_status, "unverified")
  expect_true(startsWith(cl@claim_id, "C"))

  expect_error(tempest_claim(claim_text = "x", claim_type = "nonsense"), "claim_type")
  expect_error(tempest_claim(claim_text = "x", verification_status = "bogus"), "verification_status")
  expect_error(tempest_claim(claim_text = "x", support_score = 1.5), "\\[0, 1\\]")
  expect_error(tempest_claim(claim_text = ""), "claim_text")
})

test_that("tempest_evidence_span and tempest_dispute construct", {
  sp <- tempest_evidence_span(source_id = "S0123456789ab", quote = "boils at 100C")
  expect_true(startsWith(sp@evidence_span_id, "E"))

  d <- tempest_dispute(topic = "boiling point", claim_ids = c("C1", "C2"),
                       evidence_balance = "mixed")
  expect_true(startsWith(d@dispute_id, "D"))
  expect_error(tempest_dispute(topic = "x", evidence_balance = "weird"), "evidence_balance")
})

test_that("tempest_source_record carries fields and meta", {
  s <- tempest_source_record(url = "https://example.org", title = "Ex")
  expect_true(startsWith(s@id, "S"))
  expect_type(s@meta, "list")
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ledger-types.R", reporter = "summary")'`
Expected: FAIL ("could not find function tempest_claim").

- [ ] **Step 3: Implement the records**

```r
# R/ledger-types.R
# S7 value records for the evidence ledger. Validation happens at construction:
# illegal enum values and out-of-range scores throw immediately.

# --- property helpers ---------------------------------------------------------

prop_enum <- function(choices, default = choices[[1]]) {
  S7::new_property(
    S7::class_character,
    default = default,
    validator = function(value) {
      if (length(value) != 1 || !value %in% choices) {
        sprintf("must be one of: %s", paste(choices, collapse = ", "))
      }
    }
  )
}

prop_score <- function() {
  S7::new_property(
    S7::class_numeric,
    default = NA_real_,
    validator = function(value) {
      if (length(value) != 1) return("must be length 1")
      if (!is.na(value) && (value < 0 || value > 1)) return("must be in [0, 1]")
    }
  )
}

prop_chr <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

prop_chr_vec <- function() {
  S7::new_property(S7::class_character, default = character())
}

# --- enums --------------------------------------------------------------------

tempest_claim_types <- function() {
  c("finding", "definition", "method", "limitation", "contradiction", "open_question")
}

tempest_verification_statuses <- function() {
  c("unverified", "supported", "partially_supported", "unsupported",
    "contradicted", "unverifiable")
}

# --- claim --------------------------------------------------------------------

#' Evidence ledger claim record (S7)
#' @keywords internal
tempest_claim <- S7::new_class(
  "tempest_claim",
  properties = list(
    claim_id = prop_chr(),
    claim_text = S7::class_character,
    claim_type = prop_enum(tempest_claim_types(), "finding"),
    source_ids = prop_chr_vec(),
    evidence_span_ids = prop_chr_vec(),
    supporting_quotes = S7::new_property(S7::class_list, default = list()),
    contradicting_source_ids = prop_chr_vec(),
    contradiction_note = prop_chr(),
    confidence = prop_enum(c("low", "medium", "high"), "medium"),
    support_score = prop_score(),
    contradiction_score = prop_score(),
    source_quality_score = prop_score(),
    retrieval_query = prop_chr(),
    retrieval_step_id = prop_chr(),
    perspective_id = prop_chr(),
    persona_id = prop_chr(),
    section_id = prop_chr(),
    created_at = prop_chr(),
    verified_at = prop_chr(),
    verifier_model = prop_chr(),
    verification_status = prop_enum(tempest_verification_statuses(), "unverified")
  ),
  constructor = function(claim_text,
                         source_ids = character(),
                         claim_type = "finding",
                         confidence = "medium",
                         verification_status = "unverified",
                         support_score = NA_real_,
                         contradiction_score = NA_real_,
                         source_quality_score = NA_real_,
                         evidence_span_ids = character(),
                         supporting_quotes = list(),
                         contradicting_source_ids = character(),
                         contradiction_note = NA_character_,
                         retrieval_query = NA_character_,
                         retrieval_step_id = NA_character_,
                         perspective_id = NA_character_,
                         persona_id = NA_character_,
                         section_id = NA_character_,
                         verified_at = NA_character_,
                         verifier_model = NA_character_,
                         claim_id = NULL,
                         created_at = NULL) {
    S7::new_object(
      S7::S7_object(),
      claim_id = claim_id %||% tempest_uuid("C"),
      claim_text = claim_text,
      claim_type = claim_type,
      source_ids = unique(source_ids),
      evidence_span_ids = evidence_span_ids,
      supporting_quotes = supporting_quotes,
      contradicting_source_ids = contradicting_source_ids,
      contradiction_note = contradiction_note,
      confidence = confidence,
      support_score = support_score,
      contradiction_score = contradiction_score,
      source_quality_score = source_quality_score,
      retrieval_query = retrieval_query,
      retrieval_step_id = retrieval_step_id,
      perspective_id = perspective_id,
      persona_id = persona_id,
      section_id = section_id,
      created_at = created_at %||% tempest_now_utc(),
      verified_at = verified_at,
      verifier_model = verifier_model,
      verification_status = verification_status
    )
  },
  validator = function(self) {
    if (length(self@claim_text) != 1 || is.na(self@claim_text) || !nzchar(self@claim_text)) {
      return("claim_text must be a single non-empty string")
    }
  }
)

# --- evidence span ------------------------------------------------------------

#' Evidence span record (S7)
#' @keywords internal
tempest_evidence_span <- S7::new_class(
  "tempest_evidence_span",
  properties = list(
    evidence_span_id = prop_chr(),
    source_id = S7::class_character,
    chunk_id = prop_chr(),
    quote = prop_chr(),
    start_offset = S7::new_property(S7::class_integer, default = NA_integer_),
    end_offset = S7::new_property(S7::class_integer, default = NA_integer_),
    page = S7::new_property(S7::class_integer, default = NA_integer_),
    section_heading = prop_chr(),
    relevance_score = prop_score(),
    extracted_by = prop_chr("expert"),
    created_at = prop_chr()
  ),
  constructor = function(source_id,
                         quote = NA_character_,
                         chunk_id = NA_character_,
                         start_offset = NA_integer_,
                         end_offset = NA_integer_,
                         page = NA_integer_,
                         section_heading = NA_character_,
                         relevance_score = NA_real_,
                         extracted_by = "expert",
                         evidence_span_id = NULL,
                         created_at = NULL) {
    S7::new_object(
      S7::S7_object(),
      evidence_span_id = evidence_span_id %||% tempest_uuid("E"),
      source_id = source_id,
      chunk_id = chunk_id,
      quote = quote,
      start_offset = as.integer(start_offset),
      end_offset = as.integer(end_offset),
      page = as.integer(page),
      section_heading = section_heading,
      relevance_score = relevance_score,
      extracted_by = extracted_by,
      created_at = created_at %||% tempest_now_utc()
    )
  }
)

# --- dispute ------------------------------------------------------------------

#' Dispute record (S7)
#' @keywords internal
tempest_dispute <- S7::new_class(
  "tempest_dispute",
  properties = list(
    dispute_id = prop_chr(),
    topic = S7::class_character,
    claim_ids = prop_chr_vec(),
    axis_of_disagreement = prop_chr(),
    likely_explanation = prop_chr(),
    evidence_balance = prop_enum(c("agreement", "mixed", "conflict"), "mixed"),
    unresolved_questions = prop_chr_vec(),
    created_at = prop_chr()
  ),
  constructor = function(topic,
                         claim_ids = character(),
                         axis_of_disagreement = NA_character_,
                         likely_explanation = NA_character_,
                         evidence_balance = "mixed",
                         unresolved_questions = character(),
                         dispute_id = NULL,
                         created_at = NULL) {
    S7::new_object(
      S7::S7_object(),
      dispute_id = dispute_id %||% tempest_uuid("D"),
      topic = topic,
      claim_ids = claim_ids,
      axis_of_disagreement = axis_of_disagreement,
      likely_explanation = likely_explanation,
      evidence_balance = evidence_balance,
      unresolved_questions = unresolved_questions,
      created_at = created_at %||% tempest_now_utc()
    )
  }
)

# --- source record ------------------------------------------------------------

#' Source record (S7) — typed replacement for the plain source list
#' @keywords internal
tempest_source_record <- S7::new_class(
  "tempest_source_record",
  properties = list(
    id = prop_chr(),
    url = S7::class_character,
    title = prop_chr(),
    snippet = prop_chr(),
    content_text = prop_chr(),
    fetched_at = prop_chr(),
    content_hash = prop_chr(),
    meta = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(url,
                         title = NA_character_,
                         snippet = NA_character_,
                         content_text = NA_character_,
                         fetched_at = NA_character_,
                         content_hash = NA_character_,
                         meta = list(),
                         id = NULL) {
    S7::new_object(
      S7::S7_object(),
      id = id %||% tempest_source_id(url),
      url = url,
      title = title,
      snippet = snippet,
      content_text = content_text,
      fetched_at = fetched_at,
      content_hash = content_hash,
      meta = meta
    )
  }
)
```

Note: `tempest_uuid()` lives in `R/utils.R` and yields `"<prefix>_<hash>"`, so `tempest_uuid("C")` starts with `"C"`. `%||%` is already imported.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ledger-types.R", reporter = "summary")'`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add R/ledger-types.R tests/testthat/test-ledger-types.R
git commit -m "feat: add S7 evidence-ledger record classes"
```

---

## Task 1C: Claim conversion helpers (record <-> list)

The ledger and persistence need to round-trip claims/spans to plain lists (for JSON) and tibbles (for export). Add S7 generics.

**Files:**
- Modify: `R/ledger-types.R`
- Test: `tests/testthat/test-ledger-types.R` (extend)

- [ ] **Step 1: Add failing tests**

Append to `tests/testthat/test-ledger-types.R`:

```r
test_that("claim round-trips through list", {
  cl <- tempest_claim(claim_text = "x", source_ids = c("S1", "S2"), claim_type = "method")
  lst <- tempest_claim_to_list(cl)
  expect_equal(lst$claim_text, "x")
  expect_equal(lst$claim_type, "method")
  cl2 <- tempest_claim_from_list(lst)
  expect_equal(cl2@claim_id, cl@claim_id)
  expect_equal(cl2@source_ids, c("S1", "S2"))
})

test_that("claims convert to a tibble", {
  cls <- list(
    tempest_claim(claim_text = "a", source_ids = "S1"),
    tempest_claim(claim_text = "b", source_ids = "S2", verification_status = "supported")
  )
  tb <- tempest_claims_tibble(cls)
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 2)
  expect_true(all(c("claim_id", "claim_text", "verification_status", "support_score") %in% names(tb)))
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ledger-types.R", reporter = "summary")'`
Expected: FAIL ("could not find function tempest_claim_to_list").

- [ ] **Step 3: Implement the converters**

Append to `R/ledger-types.R`:

```r
#' @keywords internal
tempest_claim_to_list <- function(claim) {
  stopifnot(S7::S7_inherits(claim, tempest_claim))
  props <- S7::prop_names(claim)
  stats::setNames(lapply(props, function(p) S7::prop(claim, p)), props)
}

#' @keywords internal
tempest_claim_from_list <- function(x) {
  do.call(tempest_claim, c(
    list(claim_text = x$claim_text %||% ""),
    x[setdiff(names(x), "claim_text")]
  ))
}

#' @keywords internal
tempest_claims_tibble <- function(claims) {
  if (length(claims) == 0) {
    return(tibble::tibble(
      claim_id = character(), claim_text = character(), claim_type = character(),
      source_ids = list(), confidence = character(),
      verification_status = character(), support_score = numeric(),
      created_at = character()
    ))
  }
  tibble::tibble(
    claim_id = purrr::map_chr(claims, ~ .x@claim_id),
    claim_text = purrr::map_chr(claims, ~ .x@claim_text),
    claim_type = purrr::map_chr(claims, ~ .x@claim_type),
    source_ids = purrr::map(claims, ~ .x@source_ids),
    confidence = purrr::map_chr(claims, ~ .x@confidence),
    verification_status = purrr::map_chr(claims, ~ .x@verification_status),
    support_score = purrr::map_dbl(claims, ~ .x@support_score),
    created_at = purrr::map_chr(claims, ~ .x@created_at)
  )
}
```

- [ ] **Step 4: Run to verify pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ledger-types.R", reporter = "summary")'`
Expected: PASS (5 tests).

- [ ] **Step 5: Commit**

```bash
git add R/ledger-types.R tests/testthat/test-ledger-types.R
git commit -m "feat: add claim list/tibble converters"
```

---

## Task 1D: Extend `SourceStore` into the ledger container

Replace the fact API with a claim API and add evidence/dispute storage plus a source-id index.

**Files:**
- Modify: `R/models.R`
- Test: `tests/testthat/test-ledger-store.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("SourceStore stores and indexes claims", {
  store <- fake_store_with_sources(2)
  id1 <- store$add_claim(tempest_claim(claim_text = "a", source_ids = "S1"))
  id2 <- store$add_claim(tempest_claim(claim_text = "b", source_ids = c("S1", "S2")))
  expect_length(store$list_claims(), 2)
  expect_equal(store$get_claim(id1)@claim_text, "a")
  by_src <- store$claims_for_source("S1")
  expect_length(by_src, 2)
})

test_that("SourceStore links evidence spans and verifies claims", {
  store <- SourceStore$new()
  cid <- store$add_claim(tempest_claim(claim_text = "a", source_ids = "S1"))
  sid <- store$add_evidence_span(tempest_evidence_span(source_id = "S1", quote = "q"))
  store$link_evidence(cid, sid)
  expect_equal(store$get_claim(cid)@evidence_span_ids, sid)
  expect_length(store$get_evidence_for_claim(cid), 1)

  store$verify_claim(cid, status = "supported", score = 0.8, verifier = "judge/x")
  cl <- store$get_claim(cid)
  expect_equal(cl@verification_status, "supported")
  expect_equal(cl@support_score, 0.8)
  expect_false(is.na(cl@verified_at))
})

test_that("to_tibbles includes claims and keeps content_text/meta on sources", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(claim_text = "a", source_ids = "S1"))
  tb <- store$to_tibbles()
  expect_true("claims" %in% names(tb))
  expect_equal(nrow(tb$claims), 1)
  expect_true(all(c("content_text", "meta") %in% names(tb$sources)))
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ledger-store.R", reporter = "summary")'`
Expected: FAIL ("attempt to apply non-function" / no `add_claim`).

- [ ] **Step 3: Rewrite `R/models.R`**

Remove `tempest_fact()`. Keep `tempest_source_id()` and `tempest_source()` (the plain-list source constructor stays — the retriever still builds plain-list sources; the S7 `tempest_source_record` is for typed export/validation). Replace the `SourceStore` R6 class body with claim-aware containers:

```r
#' SourceStore (evidence ledger)
#'
#' In-memory store for sources, claims, evidence spans, disputes, and
#' artifacts. Stays an R6 reference object: it is the mutable accumulator
#' threaded through every pipeline stage and Co-STORM turn.
#'
#' @field sources Environment of source objects keyed by id.
#' @field claims Environment of claim records keyed by claim_id.
#' @field evidence_spans Environment of evidence-span records keyed by id.
#' @field disputes Environment of dispute records keyed by id.
#' @field artifacts Environment of arbitrary artifacts.
#'
#' @export
SourceStore <- R6::R6Class(
  "SourceStore",
  public = list(
    sources = NULL,
    claims = NULL,
    evidence_spans = NULL,
    disputes = NULL,
    artifacts = NULL,

    #' @description Create a new SourceStore.
    initialize = function() {
      self$sources <- new.env(parent = emptyenv())
      self$claims <- new.env(parent = emptyenv())
      self$evidence_spans <- new.env(parent = emptyenv())
      self$disputes <- new.env(parent = emptyenv())
      self$artifacts <- new.env(parent = emptyenv())
      private$claims_by_source <- new.env(parent = emptyenv())
      invisible(self)
    },

    #' @description Insert or update a source.
    #' @param source A source list with an `id` field.
    upsert_source = function(source) {
      stopifnot(is.list(source), !is.null(source$id))
      self$sources[[source$id]] <- source
      invisible(source$id)
    },

    #' @description Get a source by id.
    #' @param source_id The source id.
    get_source = function(source_id) {
      self$sources[[source_id]] %||% NULL
    },

    #' @description List all sources.
    list_sources = function() {
      ids <- ls(self$sources, all.names = TRUE)
      purrr::map(ids, ~ self$sources[[.x]])
    },

    #' @description Add a claim record to the ledger.
    #' @param claim A `tempest_claim` S7 record.
    add_claim = function(claim) {
      stopifnot(S7::S7_inherits(claim, tempest_claim))
      id <- claim@claim_id
      self$claims[[id]] <- claim
      for (sid in claim@source_ids) {
        existing <- private$claims_by_source[[sid]] %||% character()
        private$claims_by_source[[sid]] <- unique(c(existing, id))
      }
      invisible(id)
    },

    #' @description Get a claim by id.
    #' @param claim_id The claim id.
    get_claim = function(claim_id) {
      self$claims[[claim_id]] %||% NULL
    },

    #' @description List all claims.
    list_claims = function() {
      ids <- ls(self$claims, all.names = TRUE)
      purrr::map(ids, ~ self$claims[[.x]])
    },

    #' @description Claims that cite a given source.
    #' @param source_id Source id.
    claims_for_source = function(source_id) {
      ids <- private$claims_by_source[[source_id]] %||% character()
      purrr::map(ids, ~ self$claims[[.x]])
    },

    #' @description Add an evidence span.
    #' @param span A `tempest_evidence_span` S7 record.
    add_evidence_span = function(span) {
      stopifnot(S7::S7_inherits(span, tempest_evidence_span))
      id <- span@evidence_span_id
      self$evidence_spans[[id]] <- span
      invisible(id)
    },

    #' @description Link an evidence span to a claim.
    #' @param claim_id Claim id.
    #' @param span_id Evidence span id.
    link_evidence = function(claim_id, span_id) {
      claim <- self$get_claim(claim_id)
      if (is.null(claim)) tempest_abort("Unknown claim id: {.val {claim_id}}")
      self$claims[[claim_id]] <- S7::set_props(
        claim,
        evidence_span_ids = unique(c(claim@evidence_span_ids, span_id))
      )
      invisible(claim_id)
    },

    #' @description Evidence spans linked to a claim.
    #' @param claim_id Claim id.
    get_evidence_for_claim = function(claim_id) {
      claim <- self$get_claim(claim_id)
      if (is.null(claim)) return(list())
      purrr::map(claim@evidence_span_ids, ~ self$evidence_spans[[.x]])
    },

    #' @description Update a claim's verification status.
    #' @param claim_id Claim id.
    #' @param status One of the verification status labels.
    #' @param score Support score in [0, 1] or NA.
    #' @param verifier Verifier model id.
    verify_claim = function(claim_id, status, score = NA_real_, verifier = NA_character_) {
      claim <- self$get_claim(claim_id)
      if (is.null(claim)) tempest_abort("Unknown claim id: {.val {claim_id}}")
      self$claims[[claim_id]] <- S7::set_props(
        claim,
        verification_status = status,
        support_score = score,
        verifier_model = verifier,
        verified_at = tempest_now_utc()
      )
      invisible(claim_id)
    },

    #' @description Add a dispute.
    #' @param dispute A `tempest_dispute` S7 record.
    add_dispute = function(dispute) {
      stopifnot(S7::S7_inherits(dispute, tempest_dispute))
      id <- dispute@dispute_id
      self$disputes[[id]] <- dispute
      invisible(id)
    },

    #' @description List all disputes.
    list_disputes = function() {
      ids <- ls(self$disputes, all.names = TRUE)
      purrr::map(ids, ~ self$disputes[[.x]])
    },

    #' @description Store an artifact by name.
    #' @param name Artifact name.
    #' @param value Artifact value.
    set_artifact = function(name, value) {
      self$artifacts[[name]] <- value
      invisible(name)
    },

    #' @description Retrieve an artifact by name.
    #' @param name Artifact name.
    get_artifact = function(name) {
      self$artifacts[[name]] %||% NULL
    },

    #' @description Convert sources, claims, and disputes to tibbles.
    to_tibbles = function() {
      s <- self$list_sources()
      sources <- if (length(s) == 0) {
        tibble::tibble(
          id = character(), url = character(), title = character(),
          snippet = character(), content_text = character(),
          fetched_at = character(), meta = list()
        )
      } else {
        tibble::tibble(
          id = purrr::map_chr(s, "id"),
          url = purrr::map_chr(s, "url"),
          title = purrr::map_chr(s, ~ .x$title %||% NA_character_),
          snippet = purrr::map_chr(s, ~ .x$snippet %||% NA_character_),
          content_text = purrr::map_chr(s, ~ .x$content_text %||% NA_character_),
          fetched_at = purrr::map_chr(s, ~ .x$fetched_at %||% NA_character_),
          meta = purrr::map(s, ~ .x$meta %||% list())
        )
      }
      list(
        sources = sources,
        claims = tempest_claims_tibble(self$list_claims())
      )
    }
  ),
  private = list(
    claims_by_source = NULL
  )
)
```

`S7::set_props()` returns a modified copy (S7 value semantics); re-assigning into the environment keeps the store's reference behavior.

- [ ] **Step 4: Run the tests to verify pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ledger-store.R", reporter = "summary")'`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add R/models.R tests/testthat/test-ledger-store.R
git commit -m "feat: extend SourceStore into a claim-aware evidence ledger"
```

---

## Task 1E: Rewire claim extraction and the fact-API call sites

The clean break removes `tempest_fact`/`tempest_facts`/`add_fact`. Update every caller to the claim API.

**Files:**
- Modify: `R/storm-research.R` (`tempest_extract_facts_from_answer`)
- Modify: `R/citations.R` (`tempest_facts` -> `tempest_claims`)
- Modify: `R/run-persistence.R` (facts -> claims)
- Modify: `R/storm.R` and any other call sites found by grep
- Test: `tests/testthat/test-claim-extraction.R`

- [ ] **Step 1: Write a failing test for extraction**

```r
test_that("extraction adds validated claims and rejects unknown sources", {
  store <- fake_store_with_sources(1)  # has source id for https://example.org/1
  s1 <- store$list_sources()[[1]]$id
  chat <- fake_chat(structured = list(list(facts = list(
    list(claim = "real claim", sources = list(list(source_id = s1)), confidence = "high"),
    list(claim = "ghost claim", sources = list(list(source_id = "Sdeadbeef0000")))
  ))))
  tempest_extract_facts_from_answer(chat, "answer text", store)
  claims <- store$list_claims()
  texts <- vapply(claims, function(c) c@claim_text, character(1))
  expect_true("real claim" %in% texts)
  expect_false("ghost claim" %in% texts)  # unknown source id is dropped/flagged, not stored
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-claim-extraction.R", reporter = "summary")'`
Expected: FAIL (still calls `store$add_fact`).

- [ ] **Step 3: Update `tempest_extract_facts_from_answer`**

In `R/storm-research.R`, replace the storing loop (the `for (f in facts)` block) so it constructs claims, validates source ids against the store, and adds them:

```r
  for (f in facts) {
    claim <- tempest_trim(f$claim %||% "")
    if (is.na(claim) || !nzchar(claim)) {
      next
    }
    src_ids <- purrr::map_chr(
      f$sources %||% list(),
      ~ .x$source_id %||% NA_character_
    )
    src_ids <- unique(src_ids[nzchar(src_ids) & !is.na(src_ids)])
    # Drop citations to sources that are not in the store (hallucinated ids).
    known <- src_ids[!purrr::map_lgl(src_ids, ~ is.null(store$get_source(.x)))]
    if (length(known) == 0) {
      next
    }
    store$add_claim(tempest_claim(
      claim_text = claim,
      source_ids = known,
      claim_type = "finding",
      confidence = f$confidence %||% "medium"
    ))
  }
  invisible(TRUE)
```

Note: drop the `note = f$note` argument (claims carry `contradiction_note`, not a free `note`; the extraction `note` field is no longer stored). If you want to preserve nuance text, append it to `supporting_quotes`; for Phase 1 we drop it.

- [ ] **Step 4: Update the other call sites**

Run `grep -rn "add_fact\|tempest_fact\b\|tempest_facts\|list_facts\|\$facts" R/` and update each:

- `R/citations.R`: rename `tempest_facts()` to `tempest_claims()`, returning `store$to_tibbles()$claims`; update its roxygen `@return`. Keep `tempest_sources()` as-is.
- `R/run-persistence.R`: in `tempest_run_artifact_paths`, rename `facts = .../facts.json` to `claims = .../claims.json`. Replace `tempest_restore_facts` with `tempest_restore_claims` (build `tempest_claim_from_list` records, skip ones whose id already exists, call `store$add_claim`). In `tempest_save_run_artifacts`, write `tempest_write_json(paths$claims, lapply(store$list_claims(), tempest_claim_to_list))`. In `tempest_load_run_artifacts`, load `paths$claims`. In `tempest_infer_completed_stages`, change the research check to `file.exists(paths$sources) && file.exists(paths$claims)`.
- `R/storm.R` (`tempest_run`) and `R/costorm*.R`: any `store$get_artifact("facts")` / `store$list_facts()` references become `store$list_claims()`; any place that set `store$set_artifact("facts", ...)` should set `"claims"`.

- [ ] **Step 5: Update the NAMESPACE export and roxygen**

Change `@export` on the renamed function and run `R -q -e 'devtools::document()'`. Confirm `NAMESPACE` now exports `tempest_claims` and no longer exports `tempest_facts`.

- [ ] **Step 6: Run extraction test + full suite**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-claim-extraction.R", reporter = "summary")'`
Expected: PASS.

Run: `R -q -e 'devtools::test()'`
Expected: Failures only in old tests that reference `tempest_facts`/`add_fact` — fix those in the next task.

- [ ] **Step 7: Commit**

```bash
git add R/storm-research.R R/citations.R R/run-persistence.R R/storm.R NAMESPACE man tests/testthat/test-claim-extraction.R
git commit -m "feat: route extraction through the claim API; retire facts"
```

---

## Task 1F: Update existing tests for the claim API

**Files:**
- Modify: any test files referencing the fact API
- Test: the full suite goes green

- [ ] **Step 1: Find references**

Run `grep -rln "tempest_facts\|add_fact\|tempest_fact(\|\$facts\b\|list_facts" tests/`.

- [ ] **Step 2: Update each reference**

For each hit: replace `tempest_facts(store)` with `tempest_claims(store)` and assert on claim columns (`claim_text`, `verification_status`); replace `store$add_fact(tempest_fact(claim = ..., source_ids = ...))` with `store$add_claim(tempest_claim(claim_text = ..., source_ids = ...))`; replace `store$list_facts()` with `store$list_claims()` and adjust field access (`$claim` -> `@claim_text`). Update `tests/testthat/test-run-persistence.R` expectations for `claims.json` instead of `facts.json`.

- [ ] **Step 3: Run the full suite**

Run: `R -q -e 'devtools::test()'`
Expected: PASS (no failures referencing the fact API).

- [ ] **Step 4: Commit**

```bash
git add tests/
git commit -m "test: migrate suite to the claim API"
```

---

## Task 1G: `TempestConfig` to S7 with citation-policy fields

**Files:**
- Modify: `R/config.R`
- Test: `tests/testthat/test-config.R` (extend), `tests/testthat/test-config-s7.R` (new)

- [ ] **Step 1: Write failing tests for the new fields and validation**

Create `tests/testthat/test-config-s7.R`:

```r
test_that("citation policy fields validate", {
  cfg <- tempest_config()
  expect_equal(cfg@citation_policy, "source_attributed")
  expect_equal(cfg@min_support_score, 0.7)
  expect_equal(cfg@on_unsupported_claim, "flag")

  expect_error(tempest_config(citation_policy = "nope"), "citation_policy")
  expect_error(tempest_config(on_unsupported_claim = "nope"), "on_unsupported_claim")
  expect_error(tempest_config(min_support_score = 2), "\\[0, 1\\]")
})

test_that("config remains usable by make_chat", {
  cfg <- tempest_config(chat_fn = function(role, model, system_prompt, echo) {
    list(role = role, model = model, mock = TRUE, register_tools = function(...) NULL)
  })
  chat <- tempest_make_chat(cfg, "coordinator")
  expect_true(chat$mock)
})
```

Note the existing `tests/testthat/test-config.R` uses `expect_s3_class(cfg, "TempestConfig")` and `cfg$models`. Update those: S7 objects are not S3, and properties use `@`. Change `expect_s3_class(cfg, "TempestConfig")` to `expect_true(S7::S7_inherits(cfg, TempestConfig))`, and `cfg$models$coordinator` to `cfg@models$coordinator`, etc., throughout that file. `make_chat` becomes the function `tempest_make_chat(cfg, role)`.

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-config-s7.R", reporter = "summary")'`
Expected: FAIL.

- [ ] **Step 3: Convert `TempestConfig` to S7**

Replace the R6 `TempestConfig` in `R/config.R` with an S7 class. Keep `tempest_config()` as the user-facing constructor (now wrapping the S7 class), keep `tempest_normalize_search_provider()` and the choices helper unchanged, and convert `make_chat` into a function `tempest_make_chat(config, role, system_prompt = NULL, echo = "none")` with the same body (replacing `self$` with `config@`).

**Load-order requirement:** `TempestConfig`'s definition runs at package load and uses the property helpers (`prop_enum`, `prop_chr`, `prop_score_default`) defined in `R/ledger-types.R`. R sources files alphabetically, so `config.R` loads *before* `ledger-types.R` and the helpers would not yet exist. Fix this with a roxygen `@include` so `devtools::document()` writes a `Collate:` field ordering `ledger-types.R` first. Add the directive immediately above the class definition:

```r
#' TempestConfig (S7)
#' @include ledger-types.R
#' @keywords internal
TempestConfig <- S7::new_class(
  "TempestConfig",
  properties = list(
    models = S7::new_property(S7::class_list, default = list()),
    params = S7::new_property(S7::class_list, default = list()),
    chat_fn = S7::new_property(S7::class_function | NULL, default = NULL),
    tools = S7::new_property(S7::class_any, default = NULL),
    embed_fn = S7::new_property(S7::class_function | NULL, default = NULL),
    ragnar_store = S7::new_property(S7::class_any, default = NULL),
    search_provider = prop_chr("native"),
    cache_dir = prop_chr(),
    max_search_results = S7::new_property(S7::class_numeric, default = 8),
    max_search_queries_per_turn = S7::new_property(S7::class_integer, default = 3L),
    retrieve_top_k = S7::new_property(S7::class_integer, default = 25L),
    max_sources = S7::new_property(S7::class_numeric, default = 24),
    user_agent = prop_chr("tempest (R; +https://github.com/JamesHWade/tempest)"),
    node_expansion_trigger_count = S7::new_property(S7::class_any, default = NULL),
    enable_discourse_manager = S7::new_property(S7::class_logical, default = FALSE),
    max_active_experts = S7::new_property(S7::class_integer, default = 5L),
    enable_unseen_surfacing = S7::new_property(S7::class_logical, default = FALSE),
    citation_policy = prop_enum(
      c("none", "source_attributed", "claim_verified", "strict"), "source_attributed"),
    min_support_score = prop_score_default(0.7),
    on_unsupported_claim = prop_enum(
      c("flag", "drop", "revise", "keep_with_warning"), "flag")
  )
)

#' Create a STORM configuration
#' @param ... Configuration fields (see [TempestConfig]).
#' @return A `TempestConfig` S7 object.
#' @examples
#' cfg <- tempest_config()
#' cfg <- tempest_config(search_provider = "wikipedia")
#' @export
tempest_config <- function(
  models = NULL,
  params = NULL,
  chat_fn = NULL,
  tools = NULL,
  embed_fn = NULL,
  ragnar_store = NULL,
  search_provider = "native",
  cache_dir = NULL,
  max_search_results = 8,
  max_search_queries_per_turn = 3,
  retrieve_top_k = 25,
  max_sources = 24,
  user_agent = "tempest (R; +https://github.com/JamesHWade/tempest)",
  node_expansion_trigger_count = NULL,
  enable_discourse_manager = FALSE,
  max_active_experts = 5L,
  enable_unseen_surfacing = FALSE,
  citation_policy = "source_attributed",
  min_support_score = 0.7,
  on_unsupported_claim = "flag"
) {
  default_models <- list(
    coordinator = "openai/gpt-5-mini",
    writer = "openai/gpt-5-mini",
    expert = "openai/gpt-5-mini",
    mindmap = "openai/gpt-5-mini",
    judge = "openai/gpt-5-mini"
  )
  models <- if (is.null(models)) {
    default_models
  } else if (is.character(models) && length(models) == 1) {
    lapply(default_models, function(x) models)
  } else {
    models
  }

  mq <- as.integer(max_search_queries_per_turn)
  if (is.na(mq) || mq < 1) mq <- 3L
  rk <- as.integer(retrieve_top_k)
  if (is.na(rk) || rk < 1) rk <- 25L

  cache_dir <- tempest_cache_dir(cache_dir)
  if (is.null(ragnar_store) && !is.null(embed_fn)) {
    ragnar_store <- tempest_create_ragnar_store(embed_fn, cache_dir)
  }

  TempestConfig(
    models = models,
    params = params %||% list(),
    chat_fn = chat_fn,
    tools = tools,
    embed_fn = embed_fn,
    ragnar_store = ragnar_store,
    search_provider = tempest_normalize_search_provider(search_provider),
    cache_dir = cache_dir %||% NA_character_,
    max_search_results = max_search_results,
    max_search_queries_per_turn = mq,
    retrieve_top_k = rk,
    max_sources = max_sources,
    user_agent = user_agent,
    node_expansion_trigger_count = node_expansion_trigger_count,
    enable_discourse_manager = enable_discourse_manager,
    max_active_experts = as.integer(max_active_experts),
    enable_unseen_surfacing = enable_unseen_surfacing,
    citation_policy = citation_policy,
    min_support_score = min_support_score,
    on_unsupported_claim = on_unsupported_claim
  )
}

#' Create a chat object for a given role.
#' @keywords internal
tempest_make_chat <- function(config, role, system_prompt = NULL, echo = "none") {
  model <- config@models[[role]] %||% config@models[["coordinator"]] %||% "openai/gpt-5-mini"
  if (is.null(system_prompt)) {
    prompt_name <- paste0(role, "_system")
    system_prompt <- tryCatch(tempest_prompt(prompt_name), error = function(e) NULL) %||%
      "You are a helpful assistant."
  }
  if (!is.null(config@chat_fn)) {
    chat <- config@chat_fn(role = role, model = model, system_prompt = system_prompt, echo = echo)
  } else {
    tempest_require("ellmer", "LLM orchestration for STORM/Co-STORM.")
    chat <- ellmer::chat(name = model, system_prompt = system_prompt,
                         params = config@params, echo = echo)
  }
  if (!is.null(config@tools)) {
    tools_to_register <- if (is.function(config@tools)) config@tools() else config@tools
    chat$register_tools(tools_to_register)
  }
  chat
}
```

Add `prop_score_default()` next to the other helpers in `R/ledger-types.R`:

```r
#' @keywords internal
prop_score_default <- function(default) {
  S7::new_property(
    S7::class_numeric,
    default = default,
    validator = function(value) {
      if (length(value) != 1) return("must be length 1")
      if (!is.na(value) && (value < 0 || value > 1)) return("must be in [0, 1]")
    }
  )
}
```

- [ ] **Step 4: Replace every `config$...` and `cfg$make_chat(...)` call site**

Run `grep -rn "\\$make_chat\|config\\$\|cfg\\$\|\\$models\|\\$search_provider\|\\$retrieve_top_k\|\\$max_search\|\\$ragnar_store\|\\$embed_fn\|\\$max_sources\|\\$chat_fn\|\\$tools\b\|\\$max_active_experts\|\\$enable_discourse_manager\|\\$enable_unseen_surfacing\|\\$node_expansion_trigger_count\|\\$user_agent\|\\$params" R/`.

For TempestConfig objects: replace `<cfg>$make_chat(role, ...)` with `tempest_make_chat(<cfg>, role, ...)` and `<cfg>$<field>` with `<cfg>@<field>`. Do NOT change `$` access on R6 objects (SourceStore, TempestRetriever, TempestSession) — only on config. The retriever and session hold the config; update their internal `self$config$x` to `self$config@x` and `self$config$make_chat(...)` to `tempest_make_chat(self$config, ...)`.

- [ ] **Step 5: Document and run tests**

Run: `R -q -e 'devtools::document()'`
Expected: `DESCRIPTION` gains a `Collate:` field listing `ledger-types.R` before `config.R` (from the `@include`). If `devtools::load_all()` errors with "could not find function 'prop_enum'", the Collate is missing — confirm the `@include` line and re-run `document()`.
Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-config-s7.R", reporter = "summary")'`
Expected: PASS.
Run: `R -q -e 'devtools::test()'`
Expected: PASS across the suite (fix any missed `$`->`@` config access the failures point to).

- [ ] **Step 6: Commit**

```bash
git add R/config.R R/ledger-types.R R/retriever.R R/costorm.R R/storm.R R/tools.R NAMESPACE man tests/testthat/test-config-s7.R tests/testthat/test-config.R
git commit -m "feat: convert TempestConfig to S7 with citation-policy fields"
```

---

## Task 1H: Claim-level citation verification

**Files:**
- Create: `R/verify.R`
- Test: `tests/testthat/test-verify.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("verify_claims labels each claim and returns an audit tibble", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "supported claim", source_ids = s1))
  store$add_claim(tempest_claim(claim_text = "weak claim", source_ids = s1))

  judge <- fake_chat(structured = list(
    list(status = "supported", score = 0.92, rationale = "stated verbatim"),
    list(status = "unsupported", score = 0.10, rationale = "not in source")
  ))

  audit <- tempest_verify_claims(store, verifier = judge)
  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 2)
  expect_setequal(audit$verification_status, c("supported", "unsupported"))

  statuses <- vapply(store$list_claims(), function(c) c@verification_status, character(1))
  expect_true("supported" %in% statuses)
  expect_true("unsupported" %in% statuses)
})

test_that("verify_claims skips when policy is none/source_attributed", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(claim_text = "c", source_ids = store$list_sources()[[1]]$id))
  audit <- tempest_verify_claims(store, verifier = fake_chat(), policy = "source_attributed")
  expect_equal(nrow(audit), 0)
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-verify.R", reporter = "summary")'`
Expected: FAIL ("could not find function tempest_verify_claims").

- [ ] **Step 3: Implement the verifier**

```r
# R/verify.R
# Claim-level citation verification: does each cited source support the claim?

#' @keywords internal
tempest_type_verification <- function() {
  tempest_require("ellmer")
  ellmer::type_object(
    status = ellmer::type_enum(
      c("supported", "partially_supported", "unsupported", "contradicted", "unverifiable"),
      "Whether the cited sources support the claim."
    ),
    score = ellmer::type_number("Support strength in [0, 1].", required = FALSE),
    rationale = ellmer::type_string("Brief justification.", required = FALSE)
  )
}

#' @keywords internal
tempest_verify_one_claim <- function(claim, store, verifier) {
  excerpts <- purrr::map_chr(claim@source_ids, function(sid) {
    src <- store$get_source(sid)
    if (is.null(src)) return(paste0("[", sid, "]: (source missing)"))
    body <- src$content_text %||% src$snippet %||% ""
    paste0("[", sid, "] ", src$title %||% "", ": ", substr(body, 1, 1500))
  })
  prompt <- paste0(
    "Claim:\n", claim@claim_text, "\n\n",
    "Cited sources:\n", paste(excerpts, collapse = "\n\n"), "\n\n",
    "Does the cited evidence support the claim? Reply with a status, a score in [0,1], ",
    "and a short rationale."
  )
  verifier$chat_structured(prompt, type = tempest_type_verification())
}

#' Verify claim citations against their sources
#'
#' @param store A `SourceStore` holding claims and sources.
#' @param verifier A chat object (e.g. from `tempest_make_chat(config, "judge")`).
#' @param policy Citation policy; verification runs only for "claim_verified" or
#'   "strict". Defaults to "claim_verified".
#' @param verifier_model Optional model id recorded on each verified claim.
#' @return A `citation_audit` tibble (one row per verified claim).
#' @export
tempest_verify_claims <- function(store, verifier, policy = "claim_verified",
                                  verifier_model = NA_character_) {
  stopifnot(inherits(store, "SourceStore"))
  if (!policy %in% c("claim_verified", "strict")) {
    return(tibble::tibble(
      claim_id = character(), claim_text = character(),
      verification_status = character(), support_score = numeric(),
      rationale = character()
    ))
  }
  claims <- store$list_claims()
  rows <- purrr::map(claims, function(claim) {
    v <- tryCatch(
      tempest_verify_one_claim(claim, store, verifier),
      error = function(e) list(status = "unverifiable", score = NA_real_,
                               rationale = conditionMessage(e))
    )
    status <- v$status %||% "unverifiable"
    score <- v$score %||% NA_real_
    store$verify_claim(claim@claim_id, status = status, score = score,
                       verifier = verifier_model)
    tibble::tibble(
      claim_id = claim@claim_id,
      claim_text = claim@claim_text,
      verification_status = status,
      support_score = as.numeric(score),
      rationale = v$rationale %||% NA_character_
    )
  })
  audit <- if (length(rows) == 0) {
    tibble::tibble(
      claim_id = character(), claim_text = character(),
      verification_status = character(), support_score = numeric(),
      rationale = character()
    )
  } else {
    do.call(rbind, rows)
  }
  store$set_artifact("citation_audit", audit)
  audit
}
```

- [ ] **Step 4: Run to verify pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-verify.R", reporter = "summary")'`
Expected: PASS (2 tests).

- [ ] **Step 5: Document and commit**

```bash
R -q -e 'devtools::document()'
git add R/verify.R NAMESPACE man tests/testthat/test-verify.R
git commit -m "feat: add claim-level citation verification"
```

---

## Task 1I: `citation_policy` in report assembly + audit table

**Files:**
- Modify: `R/citations.R`
- Test: `tests/testthat/test-citations-policy.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("report renders verification badges under claim_verified", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "c", source_ids = s1,
                                verification_status = "supported", support_score = 0.9))
  body <- paste0("A verified sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store, citation_policy = "claim_verified")
  expect_match(md, "✓")  # check mark badge for supported
})

test_that("strict policy flags unsupported citations", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "c", source_ids = s1,
                                verification_status = "unsupported", support_score = 0.1))
  body <- paste0("A questionable sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store, citation_policy = "strict",
                          on_unsupported_claim = "flag")
  expect_match(md, "unsupported", ignore.case = TRUE)
})

test_that("default policy is unchanged source-attributed output", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  body <- paste0("Plain sentence [", s1, "].")
  md <- tempest_report_md("Title", body, store)
  expect_match(md, "## References")
  expect_no_match(md, "✓")
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-citations-policy.R", reporter = "summary")'`
Expected: FAIL (`tempest_report_md` has no `citation_policy` arg).

- [ ] **Step 3: Extend `tempest_report_md` and footnote rendering**

In `R/citations.R`, add a helper that maps a source id to the best verification status among claims citing it, and thread the policy through:

```r
#' @keywords internal
tempest_source_status <- function(store, source_id) {
  claims <- store$claims_for_source(source_id)
  if (length(claims) == 0) return(NA_character_)
  statuses <- vapply(claims, function(c) c@verification_status, character(1))
  # Worst-case wins so a weak citation is not masked by a strong one.
  order <- c("contradicted", "unsupported", "unverifiable", "partially_supported",
             "supported", "unverified")
  statuses[order(match(statuses, order))][1]
}

#' @keywords internal
tempest_status_badge <- function(status) {
  switch(status %||% "",
    supported = " ✓",
    partially_supported = " ⚠",
    unsupported = " ✗ unsupported",
    contradicted = " ✗ contradicted",
    unverifiable = " ?",
    ""
  )
}
```

Update `tempest_add_footnotes()` to accept `citation_policy` and append `tempest_status_badge(tempest_source_status(store, id))` to each footnote when `citation_policy %in% c("claim_verified", "strict")`. Update `tempest_report_md()` signature to:

```r
tempest_report_md <- function(title, body, store,
                              citation_policy = "source_attributed",
                              on_unsupported_claim = "flag") {
```

Pass `citation_policy` into `tempest_add_footnotes()`. When `citation_policy == "strict"` and a cited source's status is `"unsupported"` or `"contradicted"`, apply `on_unsupported_claim`: for `"flag"`, append `" [unsupported citation]"` after the inline marker; for `"drop"`, remove the `[^id]` marker; for `"keep_with_warning"`, leave as-is but ensure the footnote badge shows the status; `"revise"` is treated as `"flag"` in Phase 1 (a no-LLM rewrite is out of scope). Update the `@param` roxygen.

- [ ] **Step 4: Run to verify pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-citations-policy.R", reporter = "summary")'`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
R -q -e 'devtools::document()'
git add R/citations.R NAMESPACE man tests/testthat/test-citations-policy.R
git commit -m "feat: thread citation_policy through report assembly"
```

---

## Task 1J: dsprrr modules for claim extraction and verification

Make the two new LLM steps optimizable modules with ellmer fallback, matching the existing factory pattern.

**Files:**
- Modify: `R/dsprrr.R` (`tempest_make_dsprrr_modules`), `R/verify.R`, `R/storm-research.R`
- Test: `tests/testthat/test-dsprrr-integration.R` (extend)

- [ ] **Step 1: Write a failing test**

```r
test_that("module factory includes extract_claims and verify_claim_support", {
  skip_if_not_installed("dsprrr")
  cfg <- tempest_config()
  modules <- tempest_make_dsprrr_modules(cfg)
  expect_true(all(c("extract_claims", "verify_claim_support") %in% names(modules)))
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-dsprrr-integration.R", reporter = "summary")'`
Expected: FAIL (names absent).

- [ ] **Step 3: Add the two modules**

In `tempest_make_dsprrr_modules()` (`R/dsprrr.R`), inside the `modules <- list(...)` definition, add two entries using `tempest_type_fact_extract()` (already present) for extraction and `tempest_type_verification()` (from `R/verify.R`) for verification:

```r
        extract_claims = dsprrr::module(
          dsprrr::signature(
            inputs = list(dsprrr::input("answer_text", "string")),
            output_type = facts_type,
            instructions = paste(
              "Extract atomic factual claims from the answer.",
              "Only extract claims explicitly supported by [Sxxxxxxxxxxxx] citations.",
              "List the supporting source_id(s) for each claim. Do not invent facts.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
        verify_claim_support = dsprrr::module(
          dsprrr::signature(
            inputs = list(
              dsprrr::input("claim_text", "string"),
              dsprrr::input("source_excerpts", "string")
            ),
            output_type = tempest_type_verification(),
            instructions = paste(
              "Judge whether the cited source excerpts support the claim.",
              "Return a status, a support score in [0,1], and a short rationale.",
              sep = "\n"
            )
          ),
          type = "predict"
        ),
```

Wire them through: `tempest_extract_facts_from_answer()` already accepts a `module` arg — pass `modules[["extract_claims"]]` at its call sites in `tempest_run`/research (rename the local from the old `fact_extraction` key). In `tempest_verify_one_claim()`, accept an optional `module` and call `tempest_run_dsprrr_module(module, verifier, list(claim_text = claim@claim_text, source_excerpts = paste(excerpts, collapse = "\n\n")), "claim verification")` first, falling back to the direct `verifier$chat_structured(...)` when it returns `NULL`. Thread an optional `modules` arg into `tempest_verify_claims()`.

- [ ] **Step 4: Run to verify pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-dsprrr-integration.R", reporter = "summary")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/dsprrr.R R/verify.R R/storm-research.R R/storm.R tests/testthat/test-dsprrr-integration.R
git commit -m "feat: add extract_claims and verify_claim_support dsprrr modules"
```

---

## Task 1K: Run verification inside the pipeline + persist the audit

**Files:**
- Modify: `R/storm.R` (`tempest_run` polish stage), `R/run-persistence.R`
- Test: `tests/testthat/test-run-verification.R`

- [ ] **Step 1: Write a failing test**

```r
test_that("tempest_run verifies claims before polishing when policy requires it", {
  # Exercise the helper that tempest_run calls, not a full network run.
  store <- fake_store_with_sources(1)
  s1 <- store$list_sources()[[1]]$id
  store$add_claim(tempest_claim(claim_text = "c", source_ids = s1))
  judge <- fake_chat(structured = list(list(status = "supported", score = 0.9, rationale = "ok")))
  cfg <- tempest_config(citation_policy = "claim_verified")

  tempest_run_verification(store, cfg, verifier = judge)

  expect_equal(store$list_claims()[[1]]@verification_status, "supported")
  expect_false(is.null(store$get_artifact("citation_audit")))
})

test_that("verification is skipped under the default policy", {
  store <- fake_store_with_sources(1)
  store$add_claim(tempest_claim(claim_text = "c", source_ids = store$list_sources()[[1]]$id))
  cfg <- tempest_config()  # source_attributed
  tempest_run_verification(store, cfg, verifier = fake_chat())
  expect_equal(store$list_claims()[[1]]@verification_status, "unverified")
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-run-verification.R", reporter = "summary")'`
Expected: FAIL ("could not find function tempest_run_verification").

- [ ] **Step 3: Add the helper and call it from the pipeline**

In `R/storm.R`, add:

```r
#' @keywords internal
tempest_run_verification <- function(store, config, verifier = NULL, modules = NULL) {
  if (!config@citation_policy %in% c("claim_verified", "strict")) {
    return(invisible(NULL))
  }
  verifier <- verifier %||% tempest_make_chat(config, "judge")
  tempest_verify_claims(
    store,
    verifier = verifier,
    policy = config@citation_policy,
    verifier_model = config@models[["judge"]] %||% NA_character_,
    modules = modules
  )
  invisible(NULL)
}
```

Call `tempest_run_verification(store, config, modules = modules)` in `tempest_run()` immediately before the polish step (right before `tempest_write_section`/polish assembly that produces `report_md`). Pass `config@citation_policy` (and `config@on_unsupported_claim`) into the `tempest_report_md()` call that builds the polished article.

In `R/run-persistence.R`: add `citation_audit = file.path(run_dir, "citation_audit.json")` to `tempest_run_artifact_paths()`, and in `tempest_save_run_artifacts()` write it when present:

```r
  audit <- store$get_artifact("citation_audit")
  if (!is.null(audit)) {
    tempest_write_json(paths$citation_audit, audit)
  }
```

- [ ] **Step 4: Run to verify pass**

Run: `R -q -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-run-verification.R", reporter = "summary")'`
Expected: PASS (2 tests).

- [ ] **Step 5: Run the full suite**

Run: `R -q -e 'devtools::test()'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add R/storm.R R/run-persistence.R tests/testthat/test-run-verification.R
git commit -m "feat: verify claims in-pipeline and persist the citation audit"
```

---

## Task 1L: Docs, NEWS, and a green `R CMD check`

**Files:**
- Modify: `README.md`, `NEWS.md`, `R/*` roxygen as needed

- [ ] **Step 1: Update NEWS.md**

Add bullets under a new `# tempest (development version)` heading:

```markdown
# tempest (development version)

* Facts are now claims: a typed S7 evidence ledger (`tempest_claim`,
  `tempest_evidence_span`, `tempest_dispute`) replaces the plain-list fact API.
  `tempest_facts()` is now `tempest_claims()`.
* `tempest_verify_claims()` labels whether each cited source supports its claim
  (supported / partially_supported / unsupported / contradicted / unverifiable)
  and records a `citation_audit` artifact.
* `tempest_config()` gains `citation_policy` ("none", "source_attributed",
  "claim_verified", "strict"), `min_support_score`, and `on_unsupported_claim`.
  Verification runs only under "claim_verified" or "strict".
* `tempest_config()` and the evidence-ledger records are now S7 classes with
  validated properties; the stateful classes (`SourceStore`, `TempestSession`,
  `TempestRetriever`) remain R6.
* `storm.R` was split into per-stage files (`storm-research.R`, etc.).
```

- [ ] **Step 2: Update README**

In the README config table add the three new rows (`citation_policy`, `min_support_score`, `on_unsupported_claim`). Replace the `tempest_facts(result$store)` example with `tempest_claims(result$store)`. Add a short "Citation verification" subsection showing:

```r
cfg <- tempest_config(citation_policy = "claim_verified")
res <- tempest_run("History of jazz", config = cfg)
tempest_claims(res$store)        # claims with verification_status
res$store$get_artifact("citation_audit")
```

- [ ] **Step 3: Run document + check**

Run: `R -q -e 'devtools::document()'`
Run: `R -q -e 'devtools::check(args = "--no-manual", error_on = "warning")'`
Expected: 0 errors, 0 warnings. Resolve any NOTE about undefined globals (`@` props are fine) or missing `@param`.

- [ ] **Step 4: Commit**

```bash
git add README.md NEWS.md man NAMESPACE
git commit -m "docs: document the evidence ledger and citation verification"
```

---

## Self-review checklist (run before handing off)

- **Spec coverage:** 0.1 split (0B), 0.2 mock harness (0A), 0.3 validation (1B records + 1E unknown-source rejection), 1.1 records (1B), 1.2 ledger container (1D), 1.3 extraction (1E), 1.4 verification (1H), 1.5 citation_policy (1G config + 1I report), 1.6 config to S7 (1G), 1.7 persistence (1E claims.json + 1K audit), 1.8 tests+docs (throughout + 1L). All covered.
- **Type consistency:** `tempest_claim` properties (`claim_text`, `verification_status`, `support_score`, `source_ids`, `evidence_span_ids`) are used identically in 1B/1C/1D/1E/1H/1I. Store methods (`add_claim`, `get_claim`, `list_claims`, `claims_for_source`, `add_evidence_span`, `link_evidence`, `get_evidence_for_claim`, `verify_claim`, `add_dispute`, `list_disputes`) match between 1D and their callers. Config access is `@` everywhere after 1G.
- **Clean break:** no `tempest_fact`/`tempest_facts`/`add_fact` remain after 1F (grep returns nothing in `R/` and `tests/`).
```
