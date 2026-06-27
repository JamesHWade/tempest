# Citations and report assembly

#' Return sources as a tibble
#' @param store A `SourceStore` or `TempestRetriever`.
#' @return A tibble of sources with columns: id, url, title, snippet, content_text, fetched_at.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_sources(result$store)
#' }
#' @export
tempest_sources <- function(store) {
  if (inherits(store, "TempestRetriever")) {
    store <- store$store
  }
  stopifnot(inherits(store, "SourceStore"))
  store$to_tibbles()$sources
}

#' Return claims as a tibble
#' @param store A `SourceStore` or `TempestRetriever`.
#' @return A tibble of claims with columns: claim_id, claim_text, claim_type,
#'   source_ids, confidence, verification_status, support_score, created_at.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_claims(result$store)
#' }
#' @export
tempest_claims <- function(store) {
  if (inherits(store, "TempestRetriever")) {
    store <- store$store
  }
  stopifnot(inherits(store, "SourceStore"))
  store$to_tibbles()$claims
}

#' @keywords internal
tempest_extract_citation_ids <- function(text) {
  ids <- unique(unlist(regmatches(
    text,
    gregexpr("\\[S[0-9a-f]{12}\\]", text, perl = TRUE)
  )))
  gsub("\\[|\\]", "", ids)
}

#' @keywords internal
tempest_normalize_min_support_score <- function(min_support_score) {
  score <- suppressWarnings(as.numeric(min_support_score %||% 0.7))
  if (length(score) != 1 || is.na(score) || score < 0 || score > 1) {
    tempest_abort("min_support_score must be in [0, 1].")
  }
  score
}

#' @keywords internal
tempest_apply_min_support_score <- function(status, score, min_support_score = 0.7) {
  score <- suppressWarnings(as.numeric(score))
  if (length(score) != 1 || is.na(score)) {
    return(status)
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  if (identical(status, "supported") && score < min_support_score) {
    return("unsupported")
  }
  status
}

#' @keywords internal
tempest_citation_matches <- function(text) {
  rx <- gregexpr("\\[(S[0-9a-f]{12})\\]", text, perl = TRUE)
  starts <- as.integer(rx[[1]])
  if (length(starts) == 1 && starts[[1]] == -1L) {
    return(data.frame(id = character(), start = integer(), end = integer()))
  }
  tokens <- regmatches(text, rx)[[1]]
  lens <- attr(rx[[1]], "match.length")
  data.frame(
    id = sub("^\\[(S[0-9a-f]{12})\\]$", "\\1", tokens),
    start = starts,
    end = starts + lens - 1L,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
tempest_citation_context <- function(text, start, end) {
  text_len <- nchar(text)
  before <- if (start > 1L) substr(text, 1L, start - 1L) else ""
  prev <- gregexpr("[.!?\\n]", before, perl = TRUE)[[1]]
  left <- if (length(prev) == 1 && prev[[1]] == -1L) 1L else max(prev) + 1L

  after <- if (end < text_len) substr(text, end + 1L, text_len) else ""
  next_boundary <- regexpr("[.!?\\n]", after, perl = TRUE)[[1]]
  right <- if (next_boundary == -1L) text_len else end + next_boundary

  trimws(substr(text, left, right))
}

#' @keywords internal
tempest_normalize_claim_match_text <- function(text) {
  text <- gsub("\\[(\\^?S[0-9a-f]{12})\\]", " ", text, perl = TRUE)
  text <- tolower(text)
  text <- gsub("[^[:alnum:]]+", " ", text, perl = TRUE)
  trimws(gsub("\\s+", " ", text))
}

#' @keywords internal
tempest_claims_for_citation_context <- function(claims, context = NULL) {
  if (length(claims) == 0 || is.null(context) || !nzchar(context)) {
    return(claims)
  }
  context_text <- tempest_normalize_claim_match_text(context)
  if (!nzchar(context_text)) {
    return(claims)
  }
  claim_texts <- vapply(
    claims,
    function(claim) tempest_normalize_claim_match_text(claim@claim_text),
    character(1)
  )
  matches <- vapply(claim_texts, function(claim_text) {
    nzchar(claim_text) && grepl(claim_text, context_text, fixed = TRUE)
  }, logical(1))
  matched <- claims[matches]
  if (length(matched) > 0) matched else claims
}

#' @keywords internal
tempest_source_status <- function(store, source_id, min_support_score = 0.7,
                                  context = NULL) {
  claims <- store$claims_for_source(source_id)
  if (length(claims) == 0) return(NA_character_)
  claims <- tempest_claims_for_citation_context(claims, context)
  statuses <- vapply(
    claims,
    function(c) {
      tempest_apply_min_support_score(
        c@verification_status,
        c@support_score,
        min_support_score = min_support_score
      )
    },
    character(1)
  )
  # Worst-case wins within the matched citation context so weak evidence is not
  # masked by strong evidence for the same sentence.
  worst_first <- c("contradicted", "unsupported", "unverifiable",
                   "partially_supported", "supported", "unverified")
  statuses[order(match(statuses, worst_first))][1]
}

#' @keywords internal
tempest_status_badge <- function(status) {
  if (length(status) != 1 || is.na(status)) return("")
  switch(status,
    supported = " \u2713",
    partially_supported = " \u26a0",
    unsupported = " \u2717 unsupported",
    contradicted = " \u2717 contradicted",
    unverifiable = " ?",
    ""
  )
}

#' @keywords internal
tempest_add_footnotes <- function(text, store, citation_policy = "source_attributed",
                                  on_unsupported_claim = "flag",
                                  min_support_score = 0.7) {
  stopifnot(inherits(store, "SourceStore"))
  if (identical(citation_policy, "none")) {
    return(list(text = text, footnotes = ""))
  }
  matches <- tempest_citation_matches(text)
  if (nrow(matches) == 0) {
    return(list(text = text, footnotes = ""))
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  verified <- citation_policy %in% c("claim_verified", "strict")

  pieces <- character()
  retained_ids <- character()
  cursor <- 1L
  for (i in seq_len(nrow(matches))) {
    id <- matches$id[[i]]
    start <- matches$start[[i]]
    end <- matches$end[[i]]
    marker <- paste0("[^", id, "]")
    replacement <- marker

    if (identical(citation_policy, "strict")) {
      context <- tempest_citation_context(text, start, end)
      status <- tempest_source_status(
        store,
        id,
        min_support_score = min_support_score,
        context = context
      )
      if (isTRUE(status %in% c("unsupported", "contradicted"))) {
        if (identical(on_unsupported_claim, "drop")) {
          replacement <- ""
        } else if (!identical(on_unsupported_claim, "keep_with_warning")) {
          # flag / revise -> annotate inline (revise has no LLM rewrite in Phase 1)
          replacement <- paste0(marker, " [unsupported citation]")
        }
      }
    }

    pieces <- c(pieces, substr(text, cursor, start - 1L), replacement)
    cursor <- end + 1L
    if (nzchar(replacement)) {
      retained_ids <- c(retained_ids, id)
    }
  }
  pieces <- c(pieces, substr(text, cursor, nchar(text)))
  text2 <- paste0(pieces, collapse = "")
  ids <- unique(retained_ids)

  notes <- purrr::map_chr(ids, function(id) {
    src <- store$get_source(id)
    if (is.null(src)) {
      return(glue::glue("[^{id}]: (missing source metadata)"))
    }
    title <- src$title %||% ""
    url <- src$url %||% ""
    fetched <- src$fetched_at %||% ""
    badge <- if (verified) {
      tempest_status_badge(
        tempest_source_status(store, id, min_support_score = min_support_score)
      )
    } else {
      ""
    }
    glue::glue("[^{id}]: {title}. {url} (retrieved {fetched}).{badge}")
  })
  list(text = text2, footnotes = paste(notes, collapse = "\n"))
}

#' Assemble a Markdown report with footnotes
#'
#' @param title Document title.
#' @param body Markdown body text that may include inline citations like `[Sxxxxxxxxxxxx]`.
#' @param store A `SourceStore` containing sources.
#' @param citation_policy One of "none", "source_attributed" (default),
#'   "claim_verified", "strict". "none" leaves inline citation ids unchanged
#'   and omits references. Under verified policies, footnotes show a
#'   verification badge; under "strict", unsupported/contradicted inline
#'   citations are handled per `on_unsupported_claim`.
#' @param on_unsupported_claim One of "flag" (default), "drop",
#'   "keep_with_warning", "revise". Only used under "strict".
#' @param min_support_score Minimum support score in `[0, 1]` for a claim to be
#'   considered supported.
#' @return Markdown with footnotes.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' md <- tempest_report_md(
#'   title = "History of Jazz",
#'   body = result$draft_md,
#'   store = result$store
#' )
#' }
#' @export
tempest_report_md <- function(title, body, store,
                              citation_policy = "source_attributed",
                              on_unsupported_claim = "flag",
                              min_support_score = 0.7) {
  if (inherits(store, "TempestRetriever")) {
    store <- store$store
  }
  stopifnot(inherits(store, "SourceStore"))

  res <- tempest_add_footnotes(body, store, citation_policy = citation_policy,
                               on_unsupported_claim = on_unsupported_claim,
                               min_support_score = min_support_score)
  if (nzchar(res$footnotes)) {
    return(paste0(
      "# ", title, "\n\n", res$text, "\n\n",
      "## References\n\n", res$footnotes, "\n"
    ))
  }
  paste0("# ", title, "\n\n", res$text, "\n")
}

#' Assemble a Markdown report from a Co-STORM session
#'
#' @param session A `TempestSession`.
#' @return Markdown with footnotes.
#' @examples
#' \dontrun{
#' session <- tempest_session("History of jazz", config = tempest_config())
#' session$step("Tell me about bebop.")
#' md <- tempest_session_report_md(session)
#' }
#' @export
tempest_session_report_md <- function(session) {
  stopifnot(inherits(session, "TempestSession"))
  title <- session$title %||% "Co-STORM Report"
  body <- session$artifacts$report %||% ""
  tempest_report_md(title = title, body = body, store = session$store)
}
