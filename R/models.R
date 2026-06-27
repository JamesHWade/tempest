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

#' Create a Fact note (atomic claim with citations)
#' @keywords internal
tempest_fact <- function(
  claim,
  source_ids,
  confidence = NA_character_,
  note = NA_character_,
  tags = character()
) {
  list(
    id = tempest_uuid("fact"),
    claim = claim,
    source_ids = unique(source_ids),
    confidence = confidence %||% NA_character_,
    note = note %||% NA_character_,
    tags = unique(tags),
    created_at = tempest_now_utc()
  )
}

#' SourceStore
#'
#' A lightweight in-memory store for sources, fact notes, and artifacts.
#' This store is designed for both script usage (STORM) and interactive usage (Co-STORM).
#'
#' @field sources Environment of source objects keyed by id.
#' @field facts List of fact objects.
#' @field artifacts Environment of arbitrary artifacts.
#'
#' @export
SourceStore <- R6::R6Class(
  "SourceStore",
  public = list(
    sources = NULL,
    facts = NULL,
    artifacts = NULL,

    #' @description
    #' Create a new SourceStore.
    initialize = function() {
      self$sources <- new.env(parent = emptyenv())
      self$facts <- list()
      self$artifacts <- new.env(parent = emptyenv())
      invisible(self)
    },

    #' @description
    #' Insert or update a source.
    #' @param source A source list with an `id` field.
    upsert_source = function(source) {
      stopifnot(is.list(source), !is.null(source$id))
      self$sources[[source$id]] <- source
      invisible(source$id)
    },

    #' @description
    #' Get a source by id.
    #' @param source_id The source id.
    #' @return The source list or NULL.
    get_source = function(source_id) {
      self$sources[[source_id]] %||% NULL
    },

    #' @description
    #' List all sources.
    #' @return A list of source objects.
    list_sources = function() {
      ids <- ls(self$sources, all.names = TRUE)
      purrr::map(ids, ~ self$sources[[.x]])
    },

    #' @description
    #' Add a fact to the store.
    #' @param fact A fact list with an `id` field.
    add_fact = function(fact) {
      stopifnot(is.list(fact), !is.null(fact$id))
      self$facts <- c(self$facts, list(fact))
      invisible(fact$id)
    },

    #' @description
    #' List all facts.
    #' @return A list of fact objects.
    list_facts = function() {
      self$facts
    },

    #' @description
    #' Store an artifact by name.
    #' @param name Artifact name.
    #' @param value Artifact value.
    set_artifact = function(name, value) {
      self$artifacts[[name]] <- value
      invisible(name)
    },

    #' @description
    #' Retrieve an artifact by name.
    #' @param name Artifact name.
    #' @return The artifact value or NULL.
    get_artifact = function(name) {
      self$artifacts[[name]] %||% NULL
    },

    #' @description
    #' Convert sources and facts to tibbles.
    #' @return A list with `sources` and `facts` tibbles.
    to_tibbles = function() {
      # Convenience for analysis/export
      s <- self$list_sources()
      if (length(s) == 0) {
        sources <- tibble::tibble(
          id = character(),
          url = character(),
          title = character(),
          snippet = character(),
          fetched_at = character()
        )
      } else {
        sources <- tibble::tibble(
          id = purrr::map_chr(s, "id"),
          url = purrr::map_chr(s, "url"),
          title = purrr::map_chr(s, "title"),
          snippet = purrr::map_chr(s, "snippet"),
          fetched_at = purrr::map_chr(s, "fetched_at")
        )
      }

      if (length(self$facts) == 0) {
        facts <- tibble::tibble(
          id = character(),
          claim = character(),
          source_ids = list(),
          confidence = character(),
          created_at = character()
        )
      } else {
        facts <- tibble::tibble(
          id = purrr::map_chr(self$facts, "id"),
          claim = purrr::map_chr(self$facts, "claim"),
          source_ids = purrr::map(self$facts, "source_ids"),
          confidence = purrr::map_chr(self$facts, "confidence"),
          created_at = purrr::map_chr(self$facts, "created_at")
        )
      }

      list(sources = sources, facts = facts)
    }
  )
)
