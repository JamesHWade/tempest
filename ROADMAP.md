# stormr Improvement Roadmap

This document provides a comprehensive analysis of the stormr package with prioritized improvements organized by urgency and category.

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

**Overall Test Coverage: 23.29%** (Target: 60%+)

**R CMD check Status:** 1 ERROR, 2 WARNINGs, 2 NOTEs

---

## Critical Priority (Before Any Release)

### 1. Package Name Conflict
**Category:** R CMD Check | **File:** `DESCRIPTION`

The package name `stormr` conflicts with an existing CRAN package `StormR`. This will block CRAN submission.

**Options:**
- Rename package (e.g., `rstorm`, `stormllm`, `stormrag`, `researchair`)
- Keep name for GitHub-only distribution (document non-CRAN status)

---

### 2. Invalid URLs in DESCRIPTION
**Category:** R CMD Check | **File:** `DESCRIPTION:48-49`

```
URL: https://example.com/stormr
BugReports: https://example.com/stormr/issues
```

**Fix:** Update to actual GitHub repository:
```
URL: https://github.com/JamesHWade/stormr
BugReports: https://github.com/JamesHWade/stormr/issues
```

---

### 3. Security: Unvalidated URL Input (SSRF Risk)
**Category:** Security | **File:** `R/retriever.R:4-10`

URLs are normalized but not validated. Malicious URLs could cause Server-Side Request Forgery attacks (accessing internal networks, localhost, etc.).

**Current:**
```r
storm_normalize_url <- function(url) {
  url <- stormr_trim(url)
  if (is.na(url) || url == "") return(NA_character_)
  url <- sub("^http://", "https://", url)
  url
}
```

**Fix:** Add protocol and host validation:
```r
storm_normalize_url <- function(url) {
  url <- stormr_trim(url)
  if (is.na(url) || url == "") return(NA_character_)

  # Block dangerous protocols

if (grepl("^(file|ftp|gopher|data):", url, ignore.case = TRUE)) {
    stormr_abort("URL protocol not allowed: {.url {url}}")
  }

  # Block local/internal addresses
  local_patterns <- "^https?://(localhost|127\\.0\\.0\\.1|0\\.0\\.0\\.0|\\[::\\]|192\\.168\\.|10\\.|172\\.(1[6-9]|2[0-9]|3[01])\\.)"
  if (grepl(local_patterns, url, ignore.case = TRUE)) {
    stormr_abort("Local network URLs not allowed: {.url {url}}")
  }

  url <- sub("^http://", "https://", url)
  url
}
```

---

### 4. Bug: Missing "native" Provider in Search Switch
**Category:** Bug | **File:** `R/retriever.R:231-246`

The default `search_provider = "native"` is not handled in the `StormRetriever$search()` method, causing errors when search is called directly.

**Fix:** Add native case with fallback:
```r
out <- if (identical(provider, "native")) {
  # Native provider handled via tool registration; fall back to Wikipedia for direct calls
  storm_wiki_search(query, limit = k)
} else {
  switch(provider, ...)
}
```

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

**Fix:** Add to `R/stormr-package.R`:
```r
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

### 10. Security: API Keys in Function Parameters
**Category:** Security | **File:** `R/retriever.R:452, 473, 494`

API keys are accepted as parameters with environment variable defaults, risking exposure in stack traces.

**Current:**
```r
storm_search_serper <- function(query, k = 8, api_key = Sys.getenv("SERPER_API_KEY", ""))
```

**Fix:** Move environment lookup inside function body:
```r
storm_search_serper <- function(query, k = 8) {
  api_key <- Sys.getenv("SERPER_API_KEY", "")
  if (identical(api_key, "")) {
    stormr_abort("SERPER_API_KEY environment variable is not set.")
  }
  ...
}
```

---

### 11. Silent Cache Failures
**Category:** Error Handling | **File:** `R/cache.R:24-28`

Cache read errors are silently swallowed, masking disk corruption or permission issues.

**Fix:** Add warning on failure:
```r
stormr_cache_get <- function(cache_dir, key) {
  p <- stormr_cache_path(cache_dir, key)
  if (!fs::file_exists(p)) return(NULL)
  tryCatch(
    readRDS(p),
    error = function(e) {
      cli::cli_warn("Cache read failed for {.file {key}}: {conditionMessage(e)}")
      NULL
    }
  )
}
```

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

### 13. Add R Version Dependency
**Category:** R CMD Check | **File:** `DESCRIPTION`

Package uses R 4.1+ syntax (`|>`, `\(x)`) but doesn't declare dependency.

**Fix:** Add to DESCRIPTION:
```
Depends: R (>= 4.1.0)
```

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
    stormr_abort("Invalid structured output: 'perspectives' must be a list")
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

### 19. Fix Default Model Name Typo
**Category:** Bug | **File:** `R/config.R`

Default model names may use invalid identifiers. Audit and update to current valid model names.

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
| ERROR | 1 | Package name conflict + invalid URLs |
| WARNING | 2 | Missing ragnar import, undocumented args |
| NOTE | 2 | Non-standard files, missing base R imports |

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

- **2025-12-29**: Comprehensive analysis with code review, R CMD check, and coverage assessment
