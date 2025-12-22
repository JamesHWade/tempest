# Citations and report assembly

#' Return sources as a tibble
#' @param store A `SourceStore` or `StormRetriever`.
#' @export
storm_sources <- function(store) {
  if (inherits(store, "StormRetriever")) store <- store$store
  stopifnot(inherits(store, "SourceStore"))
  store$to_tibbles()$sources
}

#' Return fact notes as a tibble
#' @param store A `SourceStore` or `StormRetriever`.
#' @export
storm_facts <- function(store) {
  if (inherits(store, "StormRetriever")) store <- store$store
  stopifnot(inherits(store, "SourceStore"))
  store$to_tibbles()$facts
}

#' @keywords internal
storm_extract_citation_ids <- function(text) {
  ids <- unique(unlist(regmatches(text, gregexpr("\\[S[0-9a-f]{12}\\]", text, perl = TRUE))))
  gsub("\\[|\\]", "", ids)
}

#' @keywords internal
storm_add_footnotes <- function(text, store) {
  stopifnot(inherits(store, "SourceStore"))
  ids <- storm_extract_citation_ids(text)
  if (length(ids) == 0) return(list(text = text, footnotes = ""))

  # Replace [Sxxxx] with [^Sxxxx]
  text2 <- gsub("\\[(S[0-9a-f]{12})\\]", "[^\\1]", text, perl = TRUE)

  notes <- purrr::map_chr(ids, function(id) {
    src <- store$get_source(id)
    if (is.null(src)) {
      return(glue::glue("[^{id}]: (missing source metadata)"))
    }
    title <- src$title %||% ""
    url <- src$url %||% ""
    fetched <- src$fetched_at %||% ""
    glue::glue("[^{id}]: {title}. {url} (retrieved {fetched}).")
  })

  list(text = text2, footnotes = paste(notes, collapse = "\n"))
}

#' Assemble a Markdown report with footnotes
#'
#' @param title Document title.
#' @param body Markdown body text that may include inline citations like `[Sxxxxxxxxxxxx]`.
#' @param store A `SourceStore` containing sources.
#' @return Markdown with footnotes.
#' @export
storm_report_md <- function(title, body, store) {
  if (inherits(store, "StormRetriever")) store <- store$store
  stopifnot(inherits(store, "SourceStore"))

  res <- storm_add_footnotes(body, store)
  md <- paste0(
    "# ", title, "\n\n",
    res$text, "\n\n",
    "## References\n\n",
    res$footnotes, "\n"
  )
  md
}

#' Assemble a Markdown report from a Co-STORM session
#'
#' @param session A `CoStormSession`.
#' @return Markdown with footnotes.
#' @export
costorm_report_md <- function(session) {
  stopifnot(inherits(session, "CoStormSession"))
  title <- session$title %||% "Co-STORM Report"
  body <- session$artifacts$report %||% ""
  storm_report_md(title = title, body = body, store = session$store)
}
