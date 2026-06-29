# Data models and stores

#' Create a deterministic source id from a URL
#' @param url URL
#' @return Source id like "Sxxxxxxxxxxxx"
#' @keywords internal
tempest_source_id <- function(url) {
  url <- tempest_trim(url)
  if (length(url) != 1 || is.na(url) || url == "") {
    tempest_abort("Invalid url for source id.")
  }
  paste0("S", substr(digest::digest(url, algo = "xxhash64"), 1, 12))
}

#' Create a Source object
#' @keywords internal
tempest_source <- function(
  url,
  title = NULL,
  snippet = NULL,
  content_text = NULL,
  fetched_at = NULL,
  content_hash = NULL,
  meta = list()
) {
  list(
    id = tempest_source_id(url),
    url = url,
    title = title %||% NA_character_,
    snippet = snippet %||% NA_character_,
    content_text = content_text %||% NA_character_,
    fetched_at = fetched_at %||% NA_character_,
    content_hash = content_hash %||% NA_character_,
    meta = meta %||% list()
  )
}

#' @keywords internal
tempest_source_scalar <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value) || length(value) == 0L) {
      next
    }
    if (is.list(value) && !is.data.frame(value)) {
      value <- unlist(value, use.names = FALSE)
    }
    value <- as.character(value[[1]])
    if (!is.na(value)) {
      value <- tempest_trim(value)
    }
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NA_character_
}

#' @keywords internal
tempest_source_context_text <- function(source) {
  meta <- source$meta %||% list()
  tempest_source_scalar(
    source$context_text,
    meta$context_text,
    meta$citation_context,
    meta$answer_context,
    source$content_text,
    source$snippet
  )
}

#' @keywords internal
tempest_source_snippet_text <- function(source, max_chars = 300L) {
  meta <- source$meta %||% list()
  text <- tempest_source_scalar(
    source$snippet,
    source$context_text,
    meta$context_text,
    meta$citation_context,
    meta$answer_context,
    source$content_text
  )
  if (is.na(text) || !nzchar(text)) {
    return(NA_character_)
  }
  text <- gsub("\\s+", " ", text, perl = TRUE)
  max_chars <- as.integer(max_chars %||% 300L)
  if (is.na(max_chars) || max_chars < 4L || nchar(text) <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1L, max_chars - 3L), "...")
}

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
      if (is.null(claim)) {
        tempest_abort("Unknown claim id: {.val {claim_id}}")
      }
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
      if (is.null(claim)) {
        return(list())
      }
      purrr::map(claim@evidence_span_ids, ~ self$evidence_spans[[.x]])
    },

    #' @description Update a claim's verification status.
    #' @param claim_id Claim id.
    #' @param status One of the verification status labels.
    #' @param score Support score in `[0, 1]` or NA.
    #' @param verifier Verifier model id.
    verify_claim = function(
      claim_id,
      status,
      score = NA_real_,
      verifier = NA_character_
    ) {
      claim <- self$get_claim(claim_id)
      if (is.null(claim)) {
        tempest_abort("Unknown claim id: {.val {claim_id}}")
      }
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
          id = character(),
          url = character(),
          title = character(),
          snippet = character(),
          content_text = character(),
          context_text = character(),
          fetched_at = character(),
          meta = list()
        )
      } else {
        tibble::tibble(
          id = purrr::map_chr(s, "id"),
          url = purrr::map_chr(s, "url"),
          title = purrr::map_chr(s, ~ .x$title %||% NA_character_),
          snippet = purrr::map_chr(s, tempest_source_snippet_text),
          content_text = purrr::map_chr(
            s,
            ~ .x$content_text %||% NA_character_
          ),
          context_text = purrr::map_chr(s, tempest_source_context_text),
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
