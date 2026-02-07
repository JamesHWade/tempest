# tempest Improvement Roadmap

This document provides a comprehensive analysis of the tempest package with prioritized improvements organized by urgency and category.

---

## Executive Summary

| Category | Critical | High | Medium | Low |
|----------|----------|------|--------|-----|
| R CMD Check Issues | 1 | 2 | 2 | - |
| Security | 2 | 1 | - | - |
| Error Handling | 1 | 2 | 1 | - |
| Testing | - | 1 | 2 | - |
| Documentation | - | 2 | 3 | 2 |
| Performance | - | 1 | 2 | - |
| Features | - | - | 4 | 6 |

**R CMD check Status:** 0 errors, 0 warnings, 1 note

---

## Critical Priority (Before Any Release)

### 1. ~~Package Name Conflict~~ (RESOLVED)
**Category:** R CMD Check | **File:** `DESCRIPTION`

~~The package name `stormr` conflicts with an existing CRAN package `StormR`. This will block CRAN submission.~~

**Resolution:** Package renamed to `tempest` to avoid CRAN conflict.

---

### 2. ~~Invalid URLs in DESCRIPTION~~ (RESOLVED)
**Category:** R CMD Check | **File:** `DESCRIPTION:48-49`

**Resolution:** URLs updated to `https://github.com/JamesHWade/tempest`

---

### 3. ~~Security: Unvalidated URL Input (SSRF Risk)~~ (RESOLVED)
**Category:** Security | **File:** `R/retriever.R`

**Resolution:** Added comprehensive SSRF protection in `storm_normalize_url()`:
- Blocks dangerous protocols (file, ftp, gopher, data, javascript, vbscript)
- Blocks local/internal addresses (localhost, 127.0.0.1, private IP ranges)
- Upgrades HTTP to HTTPS

---

### 4. ~~Bug: Missing "native" Provider in Search Switch~~ (RESOLVED)
**Category:** Bug | **File:** `R/retriever.R`

**Resolution:** Added fallback to Wikipedia when "native" provider is used for direct `search()` calls.

---

### 5. Missing Imports Declaration for ragnar
**Category:** R CMD Check | **File:** `DESCRIPTION`, `R/retriever.R`

```
WARNING: '::' or ':::' import not declared from: 'ragnar'
```

**Fix:** Move `ragnar` from Suggests to conditional usage with proper guards, or add to Imports with version requirement.

---

## High Priority (Before v0.2.0)

### 6. Unused Imports Warning
**Category:** R CMD Check | **File:** `DESCRIPTION`

```
WARNING: Namespaces in Imports field not imported from:
  'dplyr' 'jsonlite' 'R6' 'rlang' 'tidyr'
```

**Fix:** Either use `@importFrom` directives or move to Suggests if truly optional.

---

### 7. Missing Base R Imports
**Category:** R CMD Check | **File:** `R/storm.R`, `R/costorm.R`, `R/evals.R`, `R/utils.R`

```
NOTE: Undefined global functions: head, read.csv, runif, setNames
```

**Fix:** Add to `R/tempest-package.R`:
```r
#' @importFrom rlang %||%
#' @importFrom stats runif setNames
#' @importFrom utils head read.csv
NULL
```

---

### 8. Undocumented Function Arguments
**Category:** R CMD Check | **File:** `R/storm.R` (storm_run_async)

```
WARNING: Undocumented arguments in Rd file 'storm_run_async.Rd': '...'
```

**Fix:** Add `@param ...` documentation or `@inheritParams storm_run`.

---

### 9. Undocumented R6 Methods
**Category:** Documentation | **File:** `R/tools.R:349`

`ExpertSessionManager` class has undocumented methods and fields.

**Fix:** Add roxygen documentation for:
- Fields: `sessions`, `config`, `retriever`, `extractor`, `store`
- Methods: `initialize()`, `extract_facts()`, `generate_session_id()`, `get_or_create()`, `list_sessions()`

---

### 10. ~~Security: API Keys in Function Parameters~~ (RESOLVED)
**Category:** Security | **File:** `R/retriever.R`

**Resolution:** API keys are now retrieved inside function bodies, not as parameters. Clear error messages guide users to set environment variables.

---

### 11. ~~Silent Cache Failures~~ (RESOLVED)
**Category:** Error Handling | **File:** `R/cache.R`

**Resolution:** Cache read/write errors now emit warnings via `tempest_warn()` instead of failing silently.

---

### 12. Improve Test Coverage
**Category:** Testing | **Current Coverage:** 23.29%

| File | Coverage | Priority |
|------|----------|----------|
| `R/app.R` | 0.00% | Low (Shiny testing is complex) |
| `R/evals.R` | 0.00% | Medium |
| `R/citations.R` | 5.41% | High |
| `R/models.R` | 8.45% | High |
| `R/storm.R` | 10.00% | High |
| `R/retriever.R` | 13.94% | High |
| `R/costorm.R` | 23.30% | Medium |

**Strategy:**
1. Add unit tests with mocked LLM responses for core pipeline functions
2. Use `httptest2` for mocking HTTP requests
3. Target 60%+ coverage for v0.2.0

---

### 13. ~~Add R Version Dependency~~ (RESOLVED)
**Category:** R CMD Check | **File:** `DESCRIPTION`

**Resolution:** Added `Depends: R (>= 4.1.0)` to DESCRIPTION.

---

## Medium Priority (v0.3.0)

### 14. Memory: Unbounded Fact Accumulation
**Category:** Performance | **File:** `R/models.R:95-101`

Facts list grows without limit or deduplication in long sessions.

**Fix:** Add `max_facts` config option and deduplication by claim similarity.

---

### 15. Missing Structured Output Validation
**Category:** Error Handling | **File:** `R/storm.R:350-352, 487`

LLM structured outputs are used without validation.

**Fix:** Add schema validation after `chat_structured()` calls:
```r
validate_perspectives <- function(plan) {
  if (!is.list(plan$perspectives)) {
    tempest_abort("Invalid structured output: 'perspectives' must be a list")
  }
  # ... additional validation
}
```

---

### 16. Shiny App: No Timeout Protection
**Category:** Error Handling | **File:** `inst/shiny/app.R:201-227`

Async operations can hang indefinitely with no cancellation mechanism.

**Fix:** Add timeout wrapper with configurable limits.

---

### 17. Shiny App: Potential Race Conditions
**Category:** Concurrency | **File:** `inst/shiny/app.R:256-261`

R6 objects mutated in promise callbacks without synchronization.

**Fix:** Use reactive values instead of direct R6 mutation, or implement proper locking.

---

### 18. Add Package Vignettes
**Category:** Documentation

Planned vignettes:
- [ ] `vignettes/getting-started.Rmd` - Installation and first run
- [ ] `vignettes/storm-pipeline.Rmd` - Scripted STORM workflow
- [ ] `vignettes/costorm-interactive.Rmd` - Interactive Co-STORM guide
- [ ] `vignettes/rag-integration.Rmd` - Using ragnar for semantic search

---

### ~~19. Fix Default Model Name Typo~~ (RESOLVED)
**Category:** Bug | **File:** `R/config.R`

**Resolution:** Default model updated to `openai/gpt-4.1-mini`.

---

### 20. Add Security Documentation
**Category:** Documentation | **File:** `README.md`

Add "Security & Privacy Considerations" section covering:
- SSRF risks with user-provided URLs
- API cost implications (multiple LLM calls + searches per run)
- Data sent to third-party APIs (search queries, topics)
- Rate limiting recommendations

---

## Low Priority (Future)

### 21. Additional Search Providers
- [ ] Perplexity API
- [ ] Exa AI
- [ ] SerpAPI

### 22. Export Formats
- [ ] PDF export via Quarto
- [ ] Word document export
- [ ] Styled HTML export

### 23. Shiny App Enhancements
- [ ] Interactive mind map visualization (D3/visNetwork)
- [ ] Source preview panel
- [ ] Export buttons in UI
- [ ] Session save/restore

### 24. Streaming Support
- [ ] Stream expert responses in real-time
- [ ] Progressive mind map updates

### 25. Model Provider Testing
Document and test support for:
- [ ] Ollama local models
- [ ] Azure OpenAI
- [ ] AWS Bedrock
- [ ] Custom endpoints

### 26. Advanced Features (Long-term)
- [ ] Multi-language research support
- [ ] Image/diagram generation
- [ ] Citation style options (APA, MLA, Chicago)
- [ ] Fact verification/cross-checking
- [ ] Source credibility scoring

---

## Test Coverage by File

```
Overall: 23.29%

R/config.R      ████████████████████░░░  91.57%  ✓
R/prompts.R     ████████████████░░░░░░░  83.33%  ✓
R/tools.R       ███████░░░░░░░░░░░░░░░░  36.67%
R/cache.R       ███████░░░░░░░░░░░░░░░░  35.71%
R/utils.R       ██████░░░░░░░░░░░░░░░░░  30.43%
R/costorm.R     █████░░░░░░░░░░░░░░░░░░  23.30%
R/retriever.R   ███░░░░░░░░░░░░░░░░░░░░  13.94%
R/storm.R       ██░░░░░░░░░░░░░░░░░░░░░  10.00%
R/models.R      ██░░░░░░░░░░░░░░░░░░░░░   8.45%
R/citations.R   █░░░░░░░░░░░░░░░░░░░░░░   5.41%
R/app.R         ░░░░░░░░░░░░░░░░░░░░░░░   0.00%
R/evals.R       ░░░░░░░░░░░░░░░░░░░░░░░   0.00%
```

---

## R CMD Check Summary

| Type | Count | Status |
|------|-------|--------|
| ERROR | 0 | |
| WARNING | 0 | |
| NOTE | 1 | `:::` calls in mirai worker (expected for cross-process) |

---

## Contributing

Contributions welcome! To work on any item:

1. Open an issue to discuss the feature/fix
2. Fork the repository
3. Create a feature branch
4. Submit a pull request with tests

Please ensure `R CMD check` passes and test coverage doesn't decrease.

---

## Changelog

- **2026-02-07**: Close all 12 STORM/Co-STORM gaps: ToC-enriched perspectives, two-step outline, lead section, query decomposition, semantic fact retrieval, parallel research, discourse manager, dynamic expert roster, mind map expansion, unseen source surfacing, mind map reorganization, SimulatedUser evaluation. Optional dsprrr integration. Added `tempest_warn()` to all error-handling paths.
- **2025-12-29**: Comprehensive analysis with code review, R CMD check, and coverage assessment
