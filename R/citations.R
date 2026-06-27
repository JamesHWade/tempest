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
tempest_source_status <- function(store, source_id) {
  claims <- store$claims_for_source(source_id)
  if (length(claims) == 0) return(NA_character_)
  statuses <- vapply(claims, function(c) c@verification_status, character(1))
  # Worst-case wins so a weak citation is not masked by a strong one.
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
                                  on_unsupported_claim = "flag") {
  stopifnot(inherits(store, "SourceStore"))
  ids <- tempest_extract_citation_ids(text)
  if (length(ids) == 0) {
    return(list(text = text, footnotes = ""))
  }
  verified <- citation_policy %in% c("claim_verified", "strict")
  text2 <- gsub("\\[(S[0-9a-f]{12})\\]", "[^\\1]", text, perl = TRUE)

  # Strict mode: act on inline markers for unsupported/contradicted citations.
  if (identical(citation_policy, "strict")) {
    for (id in ids) {
      status <- tempest_source_status(store, id)
      if (isTRUE(status %in% c("unsupported", "contradicted"))) {
        pat <- paste0("\\[\\^", id, "\\]")
        if (identical(on_unsupported_claim, "drop")) {
          text2 <- gsub(pat, "", text2, perl = TRUE)
        } else if (!identical(on_unsupported_claim, "keep_with_warning")) {
          # flag / revise -> annotate inline (revise has no LLM rewrite in Phase 1)
          text2 <- gsub(pat, paste0("[^", id, "] [unsupported citation]"),
                        text2, perl = TRUE)
        }
      }
    }
  }

  notes <- purrr::map_chr(ids, function(id) {
    src <- store$get_source(id)
    if (is.null(src)) {
      return(glue::glue("[^{id}]: (missing source metadata)"))
    }
    title <- src$title %||% ""
    url <- src$url %||% ""
    fetched <- src$fetched_at %||% ""
    badge <- if (verified) tempest_status_badge(tempest_source_status(store, id)) else ""
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
#'   "claim_verified", "strict". Under verified policies, footnotes show a
#'   verification badge; under "strict", unsupported/contradicted inline
#'   citations are handled per `on_unsupported_claim`.
#' @param on_unsupported_claim One of "flag" (default), "drop",
#'   "keep_with_warning", "revise". Only used under "strict".
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
                              on_unsupported_claim = "flag") {
  if (inherits(store, "TempestRetriever")) {
    store <- store$store
  }
  stopifnot(inherits(store, "SourceStore"))

  res <- tempest_add_footnotes(body, store, citation_policy = citation_policy,
                               on_unsupported_claim = on_unsupported_claim)
  md <- paste0(
    "# ", title, "\n\n", res$text, "\n\n",
    "## References\n\n", res$footnotes, "\n"
  )
  md
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
