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
