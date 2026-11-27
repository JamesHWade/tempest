# TempestRetriever class and constructor

TempestRetriever <- R6::R6Class(
  "TempestRetriever",
  public = list(
    initialize = function(
      config = tempest_config(),
      workspace = tempest_research_workspace()
    ) {
      if (!inherits(workspace, "ResearchWorkspace")) {
        tempest_research_workspace_abort(
          "{.arg workspace} must be a ResearchWorkspace."
        )
      }
      private$config_value <- config
      private$workspace_value <- workspace
      private$workspace_value$set_max_sources(config@max_sources)
      private$cache_counts <- new.env(parent = emptyenv())
      invisible(self)
    },

    search = function(query, k = NULL, provider = NULL, force = FALSE) {
      k <- k %||% self$config@max_search_results
      k <- tempest_config_count(k, "k")
      if (k > self$config@max_search_results) {
        tempest_config_abort(
          c(
            "Search result request exceeds the configured budget.",
            x = "Requested {k}; maximum is {self$config@max_search_results}."
          )
        )
      }
      provider <- provider %||% self$config@search_provider
      provider <- tempest_normalize_search_provider(provider)

      key <- tempest_search_cache_key(provider, query, k)
      otel_state <- tempest_otel_retrieval_state()
      execute <- function() {
        if (self$cache_enabled && !isTRUE(force)) {
          cached <- tempest_cache_lookup(
            self$cache_dir,
            key,
            max_age = self$cache_ttl
          )
          private$record_cache("search", cached$status)
          if (!is.null(cached$value)) {
            otel_state$cache_hit <- TRUE
            otel_state$result_count <-
              tempest_otel_retrieval_search_result_count(cached$value, k)
            otel_state$outcome <- "succeeded"
            return(cached$value)
          }
        } else {
          private$record_cache("search", "bypass")
        }

        # Handle "native" provider - falls back to Wikipedia for direct search() calls
        # since native provider web search is handled via tool registration
        effective_provider <- if (identical(provider, "native")) {
          otel_state$fallback_taken <- TRUE
          "wikipedia"
        } else {
          provider
        }

        out <- tryCatch(
          switch(
            effective_provider,
            wikipedia = tempest_wiki_search(query, limit = k),
            you = tempest_search_you(query, k = k),
            bing = tempest_search_bing(query, k = k),
            serper = tempest_search_serper(query, k = k),
            brave = tempest_search_brave(query, k = k),
            duckduckgo = tempest_search_duckduckgo(query, k = k),
            tavily = tempest_search_tavily(query, k = k),
            searxng = tempest_search_searxng(query, k = k),
            google = tempest_search_google(query, k = k),
            azure_ai_search = tempest_search_azure_ai_search(query, k = k),
            tempest_abort(c(
              "Unknown search provider: {.val {provider}}",
              i = "Available providers: {.val {tempest_search_provider_choices()}}"
            ))
          ),
          error = function(error) {
            tempest_rethrow_operation(error, class = "tempest_retriever_error")
          }
        )

        # Normalize URLs without aborting on missing/unsafe values, then drop
        # any rows we cannot use. A single bad or blocked URL must not discard
        # every other (valid) result for the query.
        out$url <- purrr::map_chr(out$url, function(u) {
          tryCatch(tempest_normalize_url(u), error = function(e) NA_character_)
        })
        out <- out[!is.na(out$url) & nzchar(out$url), , drop = FALSE]
        out$source_id <- purrr::map_chr(out$url, tempest_source_id)

        if (self$cache_enabled && tempest_cache_set(self$cache_dir, key, out)) {
          private$record_cache("search", "write")
        }
        otel_state$result_count <-
          tempest_otel_retrieval_search_result_count(out, k)
        otel_state$outcome <- "succeeded"
        out
      }
      tempest_otel_trace_retrieval(
        "retrieval.search",
        execute(),
        otel_state
      )
    },

    fetch = function(url, force = FALSE, perspective = NA_character_) {
      url <- tempest_normalize_url(url)
      if (is.na(url)) {
        tempest_abort(
          c(
            "Invalid URL.",
            i = "Provide a non-empty HTTP or HTTPS URL."
          ),
          class = c(
            "tempest_retriever_url_error",
            "tempest_retriever_error",
            "tempest_error"
          )
        )
      }

      key <- tempest_fetch_cache_key(url, self$config@user_agent)
      otel_state <- tempest_otel_retrieval_state()
      execute <- function() {
        if (self$cache_enabled && !isTRUE(force)) {
          cached <- tempest_cache_lookup(
            self$cache_dir,
            key,
            max_age = self$cache_ttl
          )
          private$record_cache("fetch", cached$status)
          if (!is.null(cached$value)) {
            cached_resource <- if (
              is.list(cached$value) &&
                !is.data.frame(cached$value) &&
                identical(
                  names(cached$value),
                  tempest_resource_record_fields()
                )
            ) {
              tryCatch(
                tempest_resource_from_data(cached$value),
                error = function(error) {
                  tempest_abort(
                    "Cached fetch resource data failed validation.",
                    class = c(
                      "tempest_retriever_cache_error",
                      "tempest_retriever_error",
                      "tempest_error"
                    ),
                    parent = error
                  )
                }
              )
            } else {
              cached$value
            }
            if (
              !tempest_is_exact_resource(cached_resource) ||
                !identical(cached_resource@resource_kind, "web") ||
                !identical(cached_resource@locator, url) ||
                !identical(
                  cached_resource@resource_id,
                  tempest_source_id(url)
                )
            ) {
              tempest_abort(
                paste0(
                  "Cached fetch values must be exact typed evidence ",
                  "resources for the requested URL."
                ),
                class = c(
                  "tempest_retriever_cache_error",
                  "tempest_retriever_error",
                  "tempest_error"
                )
              )
            }
            tryCatch(
              tempest_resource_data(cached_resource),
              error = function(error) {
                tempest_abort(
                  "Cached fetch resource failed live validation.",
                  class = c(
                    "tempest_retriever_cache_error",
                    "tempest_retriever_error",
                    "tempest_error"
                  ),
                  parent = error
                )
              }
            )
            otel_state$cache_hit <- TRUE
            tryCatch(
              self$workspace$upsert_retrieved_resource(cached_resource),
              error = function(error) {
                tempest_abort(
                  "Cached fetch resource could not be admitted.",
                  class = c(
                    "tempest_retriever_cache_error",
                    "tempest_retriever_error",
                    "tempest_error"
                  ),
                  parent = error
                )
              }
            )
            otel_state$result_count <- 1L
            otel_state$outcome <- "succeeded"
            return(cached_resource)
          }
        } else {
          private$record_cache("fetch", "bypass")
        }

        res <- tryCatch(
          tempest_fetch_url_text(url, user_agent = self$config@user_agent),
          error = function(error) {
            tempest_rethrow_operation(error, class = "tempest_retriever_error")
          }
        )
        fetched_at <- tempest_now_utc()

        if (!is.null(res$error)) {
          resource <- tempest_resource(
            resource_kind = "web",
            locator = url,
            title = url,
            media_type = "text/html",
            resource_id = tempest_source_id(url),
            retrieved_at = fetched_at,
            metadata = list(
              kind = res$kind %||% "html",
              error = res$error
            )
          )
          self$workspace$upsert_retrieved_resource(resource)
          otel_state$result_count <- 0L
          otel_state$outcome <- "failed"
          return(resource)
        }

        txt <- res$text %||% NA_character_
        content <- if (!is.na(txt) && nzchar(txt)) txt else NULL

        # Use title from result if available (ragnar extracts from headings)
        # Otherwise fall back to first line
        title <- res$title %||% NA_character_
        if (is.na(title) && !is.na(txt) && nzchar(txt)) {
          first <- unlist(strsplit(txt, "\n", fixed = TRUE))[1]
          title <- tempest_trim(first)
          if (nchar(title) > 120) title <- paste0(substr(title, 1, 117), "...")
        }
        if (is.na(title) || !nzchar(title)) {
          title <- url
        }

        snippet <- if (!is.na(txt) && nzchar(txt)) {
          substr(txt, 1, 300)
        } else {
          NA_character_
        }

        metadata <- list(kind = res$kind %||% "html")
        if (!is.na(snippet) && nzchar(snippet)) {
          metadata$snippet <- snippet
        }
        resource <- tempest_resource(
          resource_kind = "web",
          locator = url,
          title = title,
          media_type = "text/html",
          resource_id = tempest_source_id(url),
          content = content,
          retrieved_at = fetched_at,
          metadata = metadata
        )
        self$workspace$upsert_retrieved_resource(resource)
        if (
          self$cache_enabled &&
            tempest_cache_set(
              self$cache_dir,
              key,
              tempest_resource_record(resource)
            )
        ) {
          private$record_cache("fetch", "write")
        }

        # Ingest into ragnar store if available
        if (!is.null(self$ragnar_store) && !is.na(txt) && nzchar(txt)) {
          self$ingest_to_ragnar(
            source_id = resource@resource_id,
            url = url,
            title = title,
            text = txt,
            fetched_at = fetched_at,
            content_type = res$kind,
            perspective = perspective
          )
        }

        otel_state$result_count <- 1L
        otel_state$outcome <- "succeeded"
        resource
      }
      tempest_otel_trace_retrieval(
        "retrieval.fetch",
        execute(),
        otel_state
      )
    },

    ingest_to_ragnar = function(
      source_id,
      url,
      title,
      text,
      fetched_at,
      content_type,
      perspective = NA_character_
    ) {
      if (is.null(self$ragnar_store)) {
        return(invisible(NULL))
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")

      tryCatch(
        {
          doc <- ragnar::MarkdownDocument(text, origin = url)
          chunks <- ragnar::markdown_chunk(
            doc,
            target_size = 512,
            target_overlap = 0.25
          )

          n_chunks <- nrow(chunks)
          chunks$source_id <- rep(source_id, n_chunks)
          chunks$url <- rep(url, n_chunks)
          chunks$title <- rep(title %||% NA_character_, n_chunks)
          chunks$fetched_at <- rep(fetched_at, n_chunks)
          chunks$content_type <- rep(content_type %||% NA_character_, n_chunks)
          chunks$perspective <- rep(perspective %||% NA_character_, n_chunks)
          chunks$context <- paste0(
            "[Source: ",
            source_id,
            "] ",
            ifelse(!is.na(title), paste0(title, " - "), "")
          )

          tempest_ragnar_store_insert(self$ragnar_store, chunks)
          invisible(n_chunks)
        },
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
    },

    build_ragnar_index = function() {
      if (is.null(self$ragnar_store)) {
        return(invisible(NULL))
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")
      tryCatch(
        tempest_ragnar_store_build_index(self$ragnar_store),
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
      invisible(TRUE)
    },

    retrieve = function(query, k = 10, method = c("hybrid", "vss", "bm25")) {
      if (is.null(self$ragnar_store)) {
        tempest_abort(
          "No ragnar store configured. Use embed_fn in tempest_config()."
        )
      }
      tempest_require("ragnar", "RAG capabilities require the ragnar package.")
      method <- match.arg(method)
      k <- tempest_config_count(k, "k")

      tryCatch(
        tempest_ragnar_retrieve(
          self$ragnar_store,
          query,
          top_k = k,
          method = method
        ),
        error = function(error) {
          tempest_rethrow_operation(error, class = "tempest_retriever_error")
        }
      )
    },

    get_source_text = function(source_id) {
      src <- self$workspace$get_retrieved_source(source_id)
      if (is.null(src)) {
        return(NA_character_)
      }
      src$content_text %||% NA_character_
    },

    list_retrieved_sources = function() {
      self$workspace$list_retrieved_sources()
    },

    cache_stats = function(reset = FALSE) {
      out <- private$cache_stats_table()
      if (isTRUE(reset)) {
        private$cache_counts <- new.env(parent = emptyenv())
      }
      out
    }
  ),
  active = list(
    config = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field config} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value
    },
    workspace = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field workspace} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$workspace_value
    },
    ragnar_store = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field ragnar_store} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value@ragnar_store
    },
    cache_dir = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field cache_dir} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value@cache_dir
    },
    cache_enabled = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field cache_enabled} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      isTRUE(private$config_value@cache_enabled)
    },
    cache_ttl = function(value) {
      if (!missing(value)) {
        tempest_abort(
          "{.field cache_ttl} is fixed when the retriever is created.",
          class = c("tempest_retriever_identity_error", "tempest_error")
        )
      }
      private$config_value@cache_ttl
    }
  ),
  private = list(
    config_value = NULL,
    workspace_value = NULL,
    cache_counts = NULL,

    record_cache = function(kind, status) {
      key <- paste(kind, status, sep = "_")
      private$cache_counts[[key]] <- (private$cache_counts[[key]] %||% 0L) + 1L
      invisible(key)
    },

    cache_stats_table = function() {
      kinds <- c("search", "fetch")
      count <- function(kind, status) {
        private$cache_counts[[paste(kind, status, sep = "_")]] %||% 0L
      }
      tibble::tibble(
        kind = kinds,
        hits = unname(vapply(kinds, count, integer(1), status = "hit")),
        misses = unname(vapply(kinds, count, integer(1), status = "miss")),
        expired = unname(vapply(kinds, count, integer(1), status = "expired")),
        read_errors = unname(vapply(
          kinds,
          count,
          integer(1),
          status = "read_error"
        )),
        bypasses = unname(vapply(kinds, count, integer(1), status = "bypass")),
        writes = unname(vapply(kinds, count, integer(1), status = "write"))
      )
    }
  )
)

tempest_retriever <- function(
  config = tempest_config(),
  workspace = tempest_research_workspace()
) {
  TempestRetriever$new(config = config, workspace = workspace)
}
