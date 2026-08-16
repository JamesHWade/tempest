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
      if (is.null(value) || length(value) == 0L) {
        next
      }
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

#' @keywords internal
tempest_research_workspace_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c(
      "tempest_research_workspace_integrity_error",
      "tempest_research_workspace_error",
      "tempest_error"
    ),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

#' @keywords internal
tempest_research_sensitive_name <- function(value) {
  value <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", value)
  value <- tolower(gsub("[^A-Za-z0-9]+", "_", value))
  token_metric <- grepl(
    paste0(
      "(^|_)(max|min|input|output|total|prompt|completion)_tokens($|_)|",
      "(^|_)tokens?_(budget|count|limit|used|usage|remaining)($|_)"
    ),
    value
  )
  credential_terms <- c(
    "api_key",
    "apikey",
    "access_key",
    "accesskey",
    "access_token",
    "accesstoken",
    "refresh_token",
    "refreshtoken",
    "auth_token",
    "authtoken",
    "oauth_token",
    "bearer_token",
    "bearertoken",
    "id_token",
    "idtoken",
    "session_token",
    "sessiontoken",
    "password",
    "passwd",
    "secret",
    "secret_key",
    "client_secret",
    "clientsecret",
    "credential",
    "credentials",
    "private_key",
    "privatekey",
    "signing_key",
    "ssh_key",
    "authorization",
    "authentication",
    "auth",
    "auth_header",
    "authorization_header",
    "header",
    "headers",
    "http_header",
    "http_headers",
    "request_header",
    "request_headers",
    "cookie",
    "cookies",
    "set_cookie",
    "token"
  )
  value %in%
    credential_terms ||
    grepl(
      paste0(
        "(^|_)(api_key|access_key|access_token|refresh_token|auth_token|",
        "oauth_token|bearer_token|id_token|session_token|password|passwd|",
        "client_secret|private_key|signing_key|ssh_key|authorization|auth|",
        "headers?|cookies?|set_cookie|credentials?|secret|secret_key)($|_)"
      ),
      value
    ) ||
    (grepl("(^|_)token($|_)", value) && !token_metric)
}

#' @keywords internal
tempest_research_reference_list_names <- function(value, path, abort) {
  value_names <- names(value)
  if (is.null(value_names) || length(value_names) == 0L) {
    return(NULL)
  }
  if (anyNA(value_names)) {
    abort("Canonical lists cannot contain missing names at {.field {path}}.")
  }
  has_names <- nzchar(value_names)
  if (any(has_names) && any(!has_names)) {
    abort(
      "Canonical lists must be fully named or fully unnamed at {.field {path}}."
    )
  }
  if (!any(has_names)) {
    return(NULL)
  }
  if (anyDuplicated(value_names)) {
    duplicate <- value_names[duplicated(value_names)][[1]]
    abort(
      "Canonical lists cannot repeat {.field {duplicate}} at {.field {path}}."
    )
  }
  value_names
}

#' @keywords internal
tempest_research_reference_value <- function(
  value,
  path,
  abort,
  noun = "Research references",
  reject_sensitive = TRUE
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.function(value)) {
    abort("{noun} cannot contain functions at {.field {path}}.")
  }
  if (is.environment(value)) {
    kind <- if (inherits(value, "R6")) "R6 objects" else "environments"
    abort("{noun} cannot contain {kind} at {.field {path}}.")
  }
  if (inherits(value, "connection")) {
    abort("{noun} cannot contain connections at {.field {path}}.")
  }
  if (typeof(value) %in% c("externalptr", "weakref")) {
    abort("{noun} cannot contain external pointers at {.field {path}}.")
  }
  if (inherits(value, "S7_object")) {
    abort("{noun} cannot contain S7 objects at {.field {path}}.")
  }
  if (is.object(value)) {
    abort(
      "{noun} must use plain JSON-compatible values at {.field {path}}."
    )
  }
  if (is.list(value)) {
    value_names <- tempest_research_reference_list_names(value, path, abort)
    if (!is.null(value_names) && isTRUE(reject_sensitive)) {
      sensitive <- vapply(
        value_names,
        tempest_research_sensitive_name,
        logical(1)
      )
      if (any(sensitive)) {
        field <- value_names[sensitive][[1]]
        abort(c(
          "{noun} cannot contain credential-like fields.",
          x = "Sensitive field: {.field {paste0(path, '$', field)}}."
        ))
      }
    }
    if (is.null(value_names)) {
      names(value) <- NULL
      result <- lapply(
        seq_along(value),
        function(index) {
          tempest_research_reference_value(
            value[[index]],
            paste0(path, "[[", index, "]]"),
            abort = abort,
            noun = noun,
            reject_sensitive = reject_sensitive
          )
        }
      )
      names(result) <- NULL
      return(result)
    }
    value <- value[order(value_names)]
    value_names <- names(value)
    return(stats::setNames(
      lapply(
        value_names,
        function(name) {
          tempest_research_reference_value(
            value[[name]],
            paste0(path, "$", name),
            abort = abort,
            noun = noun,
            reject_sensitive = reject_sensitive
          )
        }
      ),
      value_names
    ))
  }
  if (!typeof(value) %in% c("logical", "integer", "double", "character")) {
    abort(
      "Unsupported research reference type {.val {typeof(value)}} at {.field {path}}."
    )
  }
  if (!is.null(names(value))) {
    abort("Named atomic vectors are not canonical at {.field {path}}.")
  }
  if (!is.null(attributes(value))) {
    abort("{noun} must use plain atomic values at {.field {path}}.")
  }
  if (anyNA(value) || (is.numeric(value) && any(!is.finite(value)))) {
    abort(
      "{noun} cannot contain missing or non-finite values at {.field {path}}."
    )
  }
  if (length(value) == 0L) {
    return(list())
  }
  if (length(value) > 1L) {
    return(lapply(
      seq_along(value),
      function(index) {
        tempest_research_reference_value(
          value[[index]],
          paste0(path, "[[", index, "]]"),
          abort = abort,
          noun = noun,
          reject_sensitive = reject_sensitive
        )
      }
    ))
  }
  if (
    is.double(value) &&
      value == trunc(value) &&
      value >= -.Machine$integer.max &&
      value <= .Machine$integer.max
  ) {
    return(as.integer(value))
  }
  if (is.character(value)) {
    return(enc2utf8(value))
  }
  if (is.logical(value)) {
    return(as.logical(value))
  }
  if (is.integer(value)) {
    return(as.integer(value))
  }
  as.double(value)
}

#' @keywords internal
tempest_research_workspace_snapshot_id <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(tempest_trim(value))
  ) {
    tempest_research_workspace_abort(
      "{.arg base_snapshot_id} must be `NULL` or a single non-empty string."
    )
  }
  tempest_trim(value)
}

#' @keywords internal
tempest_research_workspace_graft_snapshot <- function(
  value,
  base_snapshot_id = NULL
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!requireNamespace("graft", quietly = TRUE)) {
    tempest_research_workspace_abort(
      paste0(
        "{.arg graft_snapshot} requires the optional {.pkg graft} package ",
        "to be installed."
      )
    )
  }
  if (
    !inherits(value, "graft::GraftSnapshot") ||
      !inherits(value, "S7_object")
  ) {
    tempest_research_workspace_abort(
      "{.arg graft_snapshot} must be a real {.cls graft::GraftSnapshot}."
    )
  }
  tryCatch(
    S7::validate(value),
    error = function(error) {
      tempest_research_workspace_abort(
        "{.arg graft_snapshot} is not a valid immutable Graft snapshot.",
        parent = error
      )
    }
  )
  reference <- tryCatch(
    tempest_snapshot_reference(value),
    error = function(error) {
      tempest_research_workspace_abort(
        "{.arg graft_snapshot} does not expose the public Graft boundary.",
        parent = error
      )
    }
  )
  if (
    !is.null(base_snapshot_id) &&
      !identical(reference$snapshot_id, base_snapshot_id)
  ) {
    tempest_research_workspace_abort(
      paste0(
        "{.arg graft_snapshot} does not match ",
        "{.arg base_snapshot_id}."
      )
    )
  }
  value
}

#' @keywords internal
tempest_research_workspace_reference_value <- function(value, path) {
  tempest_research_reference_value(
    value,
    path,
    abort = tempest_research_workspace_abort,
    noun = "Accepted graft references"
  )
}

#' @keywords internal
tempest_research_workspace_reference_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    pretty = FALSE,
    force = TRUE
  ))
}

#' @keywords internal
tempest_research_workspace_validate_reference_ids <- function(
  value,
  path,
  base_snapshot_id
) {
  if (!is.list(value) || length(value) == 0L) {
    return(value)
  }
  value_names <- names(value)
  if (is.null(value_names)) {
    return(lapply(
      seq_along(value),
      \(index) {
        tempest_research_workspace_validate_reference_ids(
          value[[index]],
          paste0(path, "[[", index, "]]"),
          base_snapshot_id
        )
      }
    ))
  }
  normalized_names <- tolower(gsub(
    "[^A-Za-z0-9]+",
    "_",
    gsub("([a-z0-9])([A-Z])", "\\1_\\2", value_names)
  ))
  if (anyDuplicated(normalized_names)) {
    tempest_research_workspace_abort(
      "Accepted graft reference fields must remain unique after normalization at {.field {path}}."
    )
  }
  for (index in seq_along(value)) {
    field <- value_names[[index]]
    normalized <- normalized_names[[index]]
    child_path <- paste0(path, "$", field)
    child <- value[[field]]
    if (grepl("(^|_)id$", normalized)) {
      if (identical(normalized, "batch_id") && is.null(child)) {
        next
      }
      if (
        !is.character(child) ||
          length(child) != 1L ||
          is.na(child) ||
          !nzchar(tempest_trim(child))
      ) {
        tempest_research_workspace_abort(
          "Accepted graft reference ID {.field {child_path}} must be a single non-empty string."
        )
      }
      child <- tempest_trim(child)
      if (
        identical(normalized, "snapshot_id") &&
          !is.null(base_snapshot_id) &&
          !identical(child, base_snapshot_id)
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Accepted graft reference snapshot does not match ",
            "{.field base_snapshot_id} at {.field {path}}."
          )
        )
      }
      value[[field]] <- child
      next
    }
    value[field] <- list(tempest_research_workspace_validate_reference_ids(
      child,
      child_path,
      base_snapshot_id
    ))
  }
  value
}

#' @keywords internal
tempest_research_workspace_reference_record <- function(
  reference,
  index,
  base_snapshot_id
) {
  path <- paste0("accepted_graft_references[[", index, "]]")
  if (
    !is.list(reference) ||
      is.data.frame(reference) ||
      length(reference) == 0L
  ) {
    tempest_research_workspace_abort(
      "Accepted graft references must be non-empty named records at {.field {path}}."
    )
  }
  reference_names <- names(reference)
  if (
    is.null(reference_names) ||
      anyNA(reference_names) ||
      any(!nzchar(reference_names)) ||
      anyDuplicated(reference_names)
  ) {
    tempest_research_workspace_abort(
      "Accepted graft references must be non-empty named records at {.field {path}}."
    )
  }
  reference <- tempest_research_workspace_reference_value(reference, path)
  reference_names <- names(reference)
  normalized_names <- tolower(gsub(
    "[^A-Za-z0-9]+",
    "_",
    gsub("([a-z0-9])([A-Z])", "\\1_\\2", reference_names)
  ))
  if (anyDuplicated(normalized_names)) {
    tempest_research_workspace_abort(
      "Accepted graft reference fields must remain unique after normalization at {.field {path}}."
    )
  }
  id_fields <- grepl("(^|_)id$", normalized_names)
  if (!any(id_fields)) {
    tempest_research_workspace_abort(
      "Accepted graft references require at least one non-empty `*_id` field at {.field {path}}."
    )
  }
  reference <- tempest_research_workspace_validate_reference_ids(
    reference,
    path,
    base_snapshot_id
  )
  commit_order <- which(normalized_names == "commit_order")
  if (length(commit_order) == 1L) {
    value <- reference[[commit_order]]
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0 ||
        value != trunc(value) ||
        value >= 2^53
    ) {
      tempest_research_workspace_abort(
        "Accepted graft reference commit_order must be an exact non-negative whole number at {.field {path}}."
      )
    }
    reference[[commit_order]] <- as.double(value)
  }
  reference
}

#' @keywords internal
tempest_research_workspace_references <- function(
  value,
  base_snapshot_id = NULL
) {
  if (!is.list(value) || is.data.frame(value)) {
    tempest_research_workspace_abort(
      "{.arg accepted_graft_references} must be a list of references."
    )
  }
  if (length(value) == 0L) {
    return(list())
  }
  reference_names <- names(value)
  if (
    !is.null(reference_names) &&
      (anyNA(reference_names) || any(nzchar(reference_names)))
  ) {
    tempest_research_workspace_abort(
      "{.arg accepted_graft_references} must be an unnamed list of references."
    )
  }
  references <- lapply(
    seq_along(value),
    function(index) {
      reference <- tempest_research_workspace_reference_record(
        value[[index]],
        index,
        base_snapshot_id
      )
      reference
    }
  )
  keys <- vapply(
    references,
    tempest_research_workspace_reference_json,
    character(1)
  )
  index <- order(keys)
  references <- references[index]
  keys <- keys[index]
  references[!duplicated(keys)]
}

#' @keywords internal
tempest_validate_source <- function(source) {
  if (!is.list(source) || is.data.frame(source)) {
    tempest_research_workspace_abort(
      "{.arg source} must be a source record list."
    )
  }
  required <- c(
    "id",
    "url",
    "title",
    "snippet",
    "content_text",
    "fetched_at",
    "content_hash",
    "meta"
  )
  missing <- required[!required %in% names(source)]
  if (length(missing) > 0L) {
    tempest_research_workspace_abort(
      "{.arg source} is missing required field{?s}: {.field {missing}}."
    )
  }
  for (field in c("id", "url")) {
    value <- source[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      tempest_research_workspace_abort(
        "Source field {.field {field}} must be a non-empty string."
      )
    }
  }
  text_fields <- c(
    "title",
    "snippet",
    "content_text",
    "fetched_at",
    "content_hash"
  )
  if ("context_text" %in% names(source)) {
    text_fields <- c(text_fields, "context_text")
  }
  for (field in text_fields) {
    value <- source[[field]]
    if (is.null(value)) {
      source[[field]] <- NA_character_
      value <- source[[field]]
    }
    if (!is.character(value) || length(value) != 1L) {
      tempest_research_workspace_abort(
        "Source field {.field {field}} must be a single string or `NA`."
      )
    }
  }
  if (is.null(source$meta)) {
    source$meta <- list()
  }
  if (!is.list(source$meta) || is.data.frame(source$meta)) {
    tempest_research_workspace_abort(
      "Source field {.field meta} must be a list."
    )
  }
  expected_id <- tryCatch(
    tempest_source_id(source$url),
    error = function(error) {
      tempest_research_workspace_abort(
        "Source field {.field url} is invalid.",
        parent = error
      )
    }
  )
  if (!identical(source$id, expected_id)) {
    tempest_research_workspace_abort(
      c(
        "Source id does not match its URL.",
        x = "Expected {.val {expected_id}}, not {.val {source$id}}."
      )
    )
  }
  source
}

#' @keywords internal
tempest_research_workspace_copy <- function(value) {
  rlang::duplicate(value, shallow = FALSE)
}

#' @keywords internal
tempest_research_workspace_values <- function(values) {
  ids <- sort(ls(values, all.names = TRUE))
  stats::setNames(
    lapply(
      ids,
      \(id) tempest_research_workspace_copy(values[[id]])
    ),
    ids
  )
}

#' ResearchWorkspace (provisional scientific evidence ledger)
#'
#' A mutable, run-scoped workspace for retrieved resources, proposed claims,
#' evidence spans, disputes, and references to accepted graft knowledge. The
#' workspace never grants acceptance to proposed claims; acceptance remains an
#' explicit graft review and commit.
#'
#' @field retrieved_resources Read-only named-list snapshot of typed resources
#'   and built-in web-source records keyed by resource id.
#' @field proposed_claims Read-only named-list snapshot of provisional claim
#'   records keyed by claim id.
#' @field evidence_spans Read-only named-list snapshot of provisional
#'   evidence-span records.
#' @field disputes Read-only named-list snapshot of provisional dispute
#'   records.
#' @field accepted_graft_references Read-only list of opaque references to
#'   accepted graft knowledge used by the research run.
#' @field base_snapshot_id Read-only opaque identifier for the accepted
#'   knowledge snapshot on which this workspace is based.
#' @field graft_snapshot Optional read-only, path-free
#'   `graft::GraftSnapshot` used to reopen the accepted knowledge boundary.
#' @field citation_audit Latest claim-centered citation audit, when available.
#' @field max_sources Maximum number of unique resources admitted.
#'
#' @export
ResearchWorkspace <- R6::R6Class(
  "ResearchWorkspace",
  public = list(
    #' @description Create a new provisional research workspace.
    #' @param base_snapshot_id Optional opaque identifier for the pinned
    #'   accepted knowledge snapshot.
    #' @param graft_snapshot Optional real, path-free `graft::GraftSnapshot`.
    #' @param max_sources Maximum number of unique sources. New sources are
    #'   refused once the limit is reached.
    #' @param accepted_graft_references Unnamed list of canonical
    #'   JSON-compatible references to accepted graft records.
    initialize = function(
      base_snapshot_id = NULL,
      graft_snapshot = NULL,
      max_sources = Inf,
      accepted_graft_references = list()
    ) {
      private$resources_value <- new.env(parent = emptyenv())
      private$claims_value <- new.env(parent = emptyenv())
      private$evidence_spans_value <- new.env(parent = emptyenv())
      private$disputes_value <- new.env(parent = emptyenv())
      private$citation_audit_value <- NULL
      private$claims_by_source <- new.env(parent = emptyenv())
      base_snapshot_id <-
        tempest_research_workspace_snapshot_id(base_snapshot_id)
      graft_snapshot <- tempest_research_workspace_graft_snapshot(
        graft_snapshot,
        base_snapshot_id
      )
      if (is.null(base_snapshot_id) && !is.null(graft_snapshot)) {
        base_snapshot_id <- S7::prop(graft_snapshot, "snapshot_id")
      }
      private$base_snapshot_id_value <- base_snapshot_id
      private$graft_snapshot_value <- graft_snapshot
      private$accepted_graft_references_value <-
        tempest_research_workspace_references(
          accepted_graft_references,
          private$base_snapshot_id_value
        )
      if (!is.null(graft_snapshot)) {
        private$accepted_graft_references_value <-
          tempest_research_workspace_references(
            c(
              private$accepted_graft_references_value,
              list(tempest_snapshot_reference(graft_snapshot))
            ),
            private$base_snapshot_id_value
          )
      }
      self$set_max_sources(max_sources)
      invisible(self)
    },

    #' @description Set the maximum number of unique sources.
    #' @param max_sources A positive whole number or `Inf`.
    set_max_sources = function(max_sources) {
      if (
        !is.numeric(max_sources) ||
          length(max_sources) != 1L ||
          is.na(max_sources) ||
          max_sources <= 0 ||
          (!is.infinite(max_sources) &&
            (!is.finite(max_sources) ||
              max_sources != trunc(max_sources) ||
              max_sources > .Machine$integer.max))
      ) {
        tempest_research_workspace_abort(
          "{.arg max_sources} must be a positive whole number or Inf."
        )
      }
      if (
        is.finite(max_sources) &&
          length(self$list_retrieved_resources()) > max_sources
      ) {
        tempest_research_workspace_abort(
          "{.arg max_sources} cannot be lower than the current source count."
        )
      }
      private$max_sources_value <- if (is.infinite(max_sources)) {
        Inf
      } else {
        as.integer(max_sources)
      }
      invisible(self)
    },

    #' @description Insert or update a retrieved typed evidence resource.
    #' @param resource A resource created by [tempest_resource()] or an
    #'   internal built-in web-source record.
    upsert_retrieved_resource = function(resource) {
      if (!S7::S7_inherits(resource, TempestResource)) {
        resource <- tempest_source_as_resource(resource)
      }
      resource_id <- resource@resource_id
      previous <- private$resources_value[[resource_id]]
      is_new <- is.null(previous)
      if (
        is_new &&
          length(self$list_retrieved_resources()) >= self$max_sources
      ) {
        tempest_research_workspace_abort(
          c(
            "ResearchWorkspace resource limit reached.",
            i = "Increase {.arg max_sources} to admit more resources."
          )
        )
      }
      resource <- tempest_research_workspace_copy(resource)
      private$resources_value[[resource_id]] <- resource
      if (!identical(previous, resource)) {
        private$invalidate_citation_audit()
      }
      invisible(resource_id)
    },

    #' @description Get a retrieved typed evidence resource by id.
    #' @param resource_id Resource id.
    get_retrieved_resource = function(resource_id) {
      resource <- private$resources_value[[resource_id]] %||% NULL
      if (is.null(resource)) {
        return(NULL)
      }
      if (S7::S7_inherits(resource, TempestResource)) {
        return(tempest_research_workspace_copy(resource))
      }
      tempest_research_workspace_copy(tempest_source_as_resource(resource))
    },

    #' @description Get one retrieved resource as a built-in source view.
    #' @param resource_id Resource id.
    get_retrieved_source = function(resource_id) {
      resource <- self$get_retrieved_resource(resource_id)
      if (is.null(resource)) {
        return(NULL)
      }
      tempest_research_workspace_copy(tempest_resource_as_source(resource))
    },

    #' @description List all retrieved evidence as typed resources.
    list_retrieved_resources = function() {
      ids <- sort(ls(private$resources_value, all.names = TRUE))
      purrr::map(ids, self$get_retrieved_resource)
    },

    #' @description List retrieved resources as built-in source views.
    list_retrieved_sources = function() {
      ids <- sort(ls(private$resources_value, all.names = TRUE))
      purrr::map(ids, self$get_retrieved_source)
    },

    #' @description Add a proposed claim record to the workspace.
    #' @param claim A `tempest_claim` S7 record.
    add_proposed_claim = function(claim) {
      if (!S7::S7_inherits(claim, tempest_claim)) {
        tempest_research_workspace_abort(
          "{.arg claim} must be a {.cls tempest_claim} record."
        )
      }
      missing_sources <- setdiff(
        claim@source_ids,
        ls(private$resources_value, all.names = TRUE)
      )
      if (length(missing_sources) > 0L) {
        tempest_research_workspace_abort(
          "Claim cites unknown source id{?s}: {.val {missing_sources}}."
        )
      }
      missing_spans <- setdiff(
        claim@evidence_span_ids,
        ls(private$evidence_spans_value, all.names = TRUE)
      )
      if (length(missing_spans) > 0L) {
        tempest_research_workspace_abort(
          "Claim cites unknown evidence span id{?s}: {.val {missing_spans}}."
        )
      }
      mismatched_spans <- claim@evidence_span_ids[
        vapply(
          claim@evidence_span_ids,
          function(span_id) {
            !private$evidence_spans_value[[span_id]]@source_id %in%
              claim@source_ids
          },
          logical(1)
        )
      ]
      if (length(mismatched_spans) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Claim evidence span{?s} must come from a source cited by the ",
            "claim: {.val {mismatched_spans}}."
          )
        )
      }
      id <- claim@claim_id
      previous <- private$claims_value[[id]]
      if (!is.null(previous)) {
        for (sid in previous@source_ids) {
          private$claims_by_source[[sid]] <- setdiff(
            private$claims_by_source[[sid]] %||% character(),
            id
          )
        }
      }
      claim <- tempest_research_workspace_copy(claim)
      private$claims_value[[id]] <- claim
      for (sid in claim@source_ids) {
        existing <- private$claims_by_source[[sid]] %||% character()
        private$claims_by_source[[sid]] <- unique(c(existing, id))
      }
      if (!identical(previous, claim)) {
        private$invalidate_citation_audit()
      }
      invisible(id)
    },

    #' @description Get a proposed claim by id.
    #' @param claim_id The claim id.
    get_proposed_claim = function(claim_id) {
      claim <- private$claims_value[[claim_id]] %||% NULL
      tempest_research_workspace_copy(claim)
    },

    #' @description List all proposed claims.
    list_proposed_claims = function() {
      ids <- sort(ls(private$claims_value, all.names = TRUE))
      purrr::map(ids, self$get_proposed_claim)
    },

    #' @description Proposed claims that cite a retrieved resource.
    #' @param resource_id Resource id.
    proposed_claims_for_resource = function(resource_id) {
      ids <- sort(private$claims_by_source[[resource_id]] %||% character())
      purrr::map(ids, self$get_proposed_claim)
    },

    #' @description Add an evidence span.
    #' @param span A `tempest_evidence_span` S7 record.
    add_evidence_span = function(span) {
      if (!S7::S7_inherits(span, tempest_evidence_span)) {
        tempest_research_workspace_abort(
          "{.arg span} must be a {.cls tempest_evidence_span} record."
        )
      }
      if (is.null(self$get_retrieved_resource(span@source_id))) {
        tempest_research_workspace_abort(
          "Evidence span cites unknown source id: {.val {span@source_id}}."
        )
      }
      id <- span@evidence_span_id
      previous <- private$evidence_spans_value[[id]]
      if (
        !is.null(previous) && !identical(previous@source_id, span@source_id)
      ) {
        linked_claim_ids <- vapply(
          self$list_proposed_claims(),
          function(claim) {
            if (id %in% claim@evidence_span_ids) {
              claim@claim_id
            } else {
              NA_character_
            }
          },
          character(1)
        )
        linked_claim_ids <- linked_claim_ids[!is.na(linked_claim_ids)]
        if (length(linked_claim_ids) > 0L) {
          tempest_research_workspace_abort(
            paste0(
              "Cannot replace linked evidence span {.val {id}} with a ",
              "different source; it is cited by claim{?s}: ",
              "{.val {linked_claim_ids}}."
            )
          )
        }
      }
      span <- tempest_research_workspace_copy(span)
      private$evidence_spans_value[[id]] <- span
      if (!identical(previous, span)) {
        private$invalidate_citation_audit()
      }
      invisible(id)
    },

    #' @description Get an evidence span by id.
    #' @param span_id Evidence span id.
    get_evidence_span = function(span_id) {
      span <- private$evidence_spans_value[[span_id]] %||% NULL
      tempest_research_workspace_copy(span)
    },

    #' @description List all evidence spans in deterministic id order.
    list_evidence_spans = function() {
      ids <- sort(ls(private$evidence_spans_value, all.names = TRUE))
      purrr::map(ids, self$get_evidence_span)
    },

    #' @description Link an evidence span to a claim.
    #' @param claim_id Claim id.
    #' @param span_id Evidence span id.
    link_evidence_to_proposed_claim = function(claim_id, span_id) {
      claim <- self$get_proposed_claim(claim_id)
      if (is.null(claim)) {
        tempest_research_workspace_abort("Unknown claim id: {.val {claim_id}}.")
      }
      span <- private$evidence_spans_value[[span_id]]
      if (is.null(span)) {
        tempest_research_workspace_abort(
          "Unknown evidence span id: {.val {span_id}}."
        )
      }
      if (is.null(self$get_retrieved_resource(span@source_id))) {
        tempest_research_workspace_abort(
          "Evidence span cites unknown source id: {.val {span@source_id}}."
        )
      }
      if (!span@source_id %in% claim@source_ids) {
        tempest_research_workspace_abort(
          "Evidence span source is not cited by claim {.val {claim_id}}."
        )
      }
      evidence_span_ids <- unique(c(claim@evidence_span_ids, span_id))
      if (!identical(evidence_span_ids, claim@evidence_span_ids)) {
        private$claims_value[[claim_id]] <- S7::set_props(
          claim,
          evidence_span_ids = evidence_span_ids
        )
        private$invalidate_citation_audit()
      }
      invisible(claim_id)
    },

    #' @description Evidence spans linked to a claim.
    #' @param claim_id Claim id.
    get_evidence_for_proposed_claim = function(claim_id) {
      claim <- self$get_proposed_claim(claim_id)
      if (is.null(claim)) {
        return(list())
      }
      purrr::map(claim@evidence_span_ids, self$get_evidence_span)
    },

    #' @description Update a claim's verification status.
    #' @param claim_id Claim id.
    #' @param status One of the verification status labels.
    #' @param score Support score in `[0, 1]` or NA.
    #' @param verifier Verifier model id.
    verify_proposed_claim = function(
      claim_id,
      status,
      score = NA_real_,
      verifier = NA_character_
    ) {
      claim <- self$get_proposed_claim(claim_id)
      if (is.null(claim)) {
        tempest_abort("Unknown claim id: {.val {claim_id}}")
      }
      private$claims_value[[claim_id]] <- S7::set_props(
        claim,
        verification_status = status,
        support_score = score,
        verifier_model = verifier,
        verified_at = tempest_now_utc()
      )
      private$invalidate_citation_audit()
      invisible(claim_id)
    },

    #' @description Add a dispute.
    #' @param dispute A `tempest_dispute` S7 record.
    add_dispute = function(dispute) {
      if (!S7::S7_inherits(dispute, tempest_dispute)) {
        tempest_research_workspace_abort(
          "{.arg dispute} must be a {.cls tempest_dispute} record."
        )
      }
      missing_claims <- setdiff(
        dispute@claim_ids,
        ls(private$claims_value, all.names = TRUE)
      )
      if (length(missing_claims) > 0L) {
        tempest_research_workspace_abort(
          "Dispute cites unknown claim id{?s}: {.val {missing_claims}}."
        )
      }
      id <- dispute@dispute_id
      previous <- private$disputes_value[[id]]
      dispute <- tempest_research_workspace_copy(dispute)
      private$disputes_value[[id]] <- dispute
      if (!identical(previous, dispute)) {
        private$invalidate_citation_audit()
      }
      invisible(id)
    },

    #' @description List all disputes.
    list_disputes = function() {
      ids <- sort(ls(private$disputes_value, all.names = TRUE))
      purrr::map(
        ids,
        \(id) tempest_research_workspace_copy(private$disputes_value[[id]])
      )
    },

    #' @description Record a reference to accepted graft knowledge.
    #' @param reference Opaque canonical JSON-compatible graft reference.
    record_accepted_graft_reference = function(reference) {
      private$accepted_graft_references_value <-
        tempest_research_workspace_references(
          c(
            private$accepted_graft_references_value,
            list(reference)
          ),
          private$base_snapshot_id_value
        )
      invisible(tempest_research_workspace_copy(reference))
    },

    #' @description List accepted graft references deterministically.
    list_accepted_graft_references = function() {
      tempest_research_workspace_references(
        private$accepted_graft_references_value,
        private$base_snapshot_id_value
      )
    },

    #' @description Record the latest claim-centered citation audit.
    #' @param citation_audit A citation-audit data frame, or `NULL` to clear it.
    set_citation_audit = function(citation_audit) {
      if (is.null(citation_audit)) {
        private$citation_audit_value <- NULL
        return(invisible(NULL))
      }
      if (!is.data.frame(citation_audit)) {
        tempest_research_workspace_abort(
          "{.arg citation_audit} must be a data frame or `NULL`."
        )
      }
      required <- c(
        "claim_id",
        "claim_text",
        "verification_status",
        "support_score",
        "rationale"
      )
      fields <- names(citation_audit)
      if (
        is.null(fields) ||
          anyNA(fields) ||
          anyDuplicated(fields) ||
          !setequal(fields, required)
      ) {
        missing <- setdiff(required, fields %||% character())
        unexpected <- setdiff(fields %||% character(), required)
        tempest_research_workspace_abort(
          c(
            "{.arg citation_audit} must contain exactly the claim-audit fields.",
            x = if (length(missing) > 0L) {
              "Missing field{?s}: {.field {missing}}."
            },
            x = if (length(unexpected) > 0L) {
              "Unexpected field{?s}: {.field {unexpected}}."
            }
          )
        )
      }
      citation_audit <- citation_audit[required]
      for (field in c(
        "claim_id",
        "claim_text",
        "verification_status",
        "rationale"
      )) {
        if (!is.character(citation_audit[[field]])) {
          tempest_research_workspace_abort(
            "Citation-audit field {.field {field}} must be character."
          )
        }
      }
      if (!is.numeric(citation_audit$support_score)) {
        tempest_research_workspace_abort(
          "Citation-audit field {.field support_score} must be numeric."
        )
      }
      if (
        anyNA(citation_audit$claim_id) ||
          any(!nzchar(tempest_trim(citation_audit$claim_id))) ||
          anyDuplicated(citation_audit$claim_id)
      ) {
        tempest_research_workspace_abort(
          "Citation-audit claim IDs must be unique non-empty strings."
        )
      }
      if (
        anyNA(citation_audit$claim_text) ||
          any(!nzchar(tempest_trim(citation_audit$claim_text)))
      ) {
        tempest_research_workspace_abort(
          "Citation-audit claim text must contain non-empty strings."
        )
      }
      invalid_statuses <- setdiff(
        unique(citation_audit$verification_status),
        tempest_verification_statuses()
      )
      if (
        anyNA(citation_audit$verification_status) ||
          length(invalid_statuses) > 0L
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Citation-audit verification statuses must be one of: ",
            "{.val {tempest_verification_statuses()}}."
          )
        )
      }
      invalid_scores <- !is.na(citation_audit$support_score) &
        (!is.finite(citation_audit$support_score) |
          citation_audit$support_score < 0 |
          citation_audit$support_score > 1)
      if (any(invalid_scores)) {
        tempest_research_workspace_abort(
          "Citation-audit support scores must be `NA` or finite values in [0, 1]."
        )
      }
      citation_audit$support_score <- as.double(
        citation_audit$support_score
      )
      for (index in seq_len(nrow(citation_audit))) {
        claim_id <- citation_audit$claim_id[[index]]
        claim <- private$claims_value[[claim_id]]
        if (is.null(claim)) {
          tempest_research_workspace_abort(
            "Citation audit cites unknown claim id: {.val {claim_id}}."
          )
        }
        if (!identical(citation_audit$claim_text[[index]], claim@claim_text)) {
          tempest_research_workspace_abort(
            "Citation-audit text does not match claim {.val {claim_id}}."
          )
        }
        if (
          !identical(
            citation_audit$verification_status[[index]],
            claim@verification_status
          )
        ) {
          tempest_research_workspace_abort(
            "Citation-audit status does not match claim {.val {claim_id}}."
          )
        }
        if (
          !isTRUE(all.equal(
            citation_audit$support_score[[index]],
            claim@support_score,
            check.attributes = FALSE
          ))
        ) {
          tempest_research_workspace_abort(
            "Citation-audit support score does not match claim {.val {claim_id}}."
          )
        }
      }
      private$citation_audit_value <- rlang::duplicate(
        tibble::as_tibble(citation_audit),
        shallow = FALSE
      )
      invisible(self$citation_audit)
    },

    #' @description Convert sources, claims, and disputes to tibbles.
    to_tibbles = function() {
      s <- self$list_retrieved_sources()
      resources <- if (length(s) == 0) {
        tibble::tibble(
          id = character(),
          resource_kind = character(),
          locator = character(),
          url = character(),
          title = character(),
          media_type = character(),
          snippet = character(),
          content_text = character(),
          context_text = character(),
          fetched_at = character(),
          meta = list()
        )
      } else {
        tibble::tibble(
          id = purrr::map_chr(s, "id"),
          resource_kind = purrr::map_chr(
            s,
            function(source) {
              source$meta$resource_kind %||% "web"
            }
          ),
          locator = purrr::map_chr(
            s,
            function(source) {
              source$meta$locator %||% source$url %||% NA_character_
            }
          ),
          url = purrr::map_chr(s, "url"),
          title = purrr::map_chr(s, ~ .x$title %||% NA_character_),
          media_type = purrr::map_chr(
            s,
            function(source) {
              source$meta$media_type %||% "text/html"
            }
          ),
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
        retrieved_resources = resources,
        proposed_claims = tempest_claims_tibble(self$list_proposed_claims())
      )
    }
  ),
  active = list(
    retrieved_resources = function(value) {
      if (!missing(value)) {
        private$read_only_binding("retrieved_resources")
      }
      tempest_research_workspace_values(private$resources_value)
    },
    proposed_claims = function(value) {
      if (!missing(value)) {
        private$read_only_binding("proposed_claims")
      }
      tempest_research_workspace_values(private$claims_value)
    },
    evidence_spans = function(value) {
      if (!missing(value)) {
        private$read_only_binding("evidence_spans")
      }
      tempest_research_workspace_values(private$evidence_spans_value)
    },
    disputes = function(value) {
      if (!missing(value)) {
        private$read_only_binding("disputes")
      }
      tempest_research_workspace_values(private$disputes_value)
    },
    max_sources = function(value) {
      if (!missing(value)) {
        tempest_research_workspace_abort(
          paste0(
            "{.field max_sources} is read-only; use ",
            "{.fn set_max_sources}."
          )
        )
      }
      private$max_sources_value
    },
    base_snapshot_id = function(value) {
      if (!missing(value)) {
        tempest_research_workspace_abort(
          "{.field base_snapshot_id} is pinned when the workspace is created."
        )
      }
      private$base_snapshot_id_value
    },
    graft_snapshot = function(value) {
      if (!missing(value)) {
        tempest_research_workspace_abort(
          "{.field graft_snapshot} is pinned when the workspace is created."
        )
      }
      private$graft_snapshot_value
    },
    accepted_graft_references = function(value) {
      if (!missing(value)) {
        tempest_research_workspace_abort(
          paste0(
            "{.field accepted_graft_references} is read-only; use ",
            "{.fn record_accepted_graft_reference}."
          )
        )
      }
      self$list_accepted_graft_references()
    },
    citation_audit = function(value) {
      if (!missing(value)) {
        tempest_research_workspace_abort(
          paste0(
            "{.field citation_audit} is read-only; use ",
            "{.fn set_citation_audit}."
          )
        )
      }
      if (is.null(private$citation_audit_value)) {
        return(NULL)
      }
      rlang::duplicate(private$citation_audit_value, shallow = FALSE)
    }
  ),
  private = list(
    resources_value = NULL,
    claims_value = NULL,
    evidence_spans_value = NULL,
    disputes_value = NULL,
    max_sources_value = NULL,
    claims_by_source = NULL,
    base_snapshot_id_value = NULL,
    graft_snapshot_value = NULL,
    accepted_graft_references_value = NULL,
    citation_audit_value = NULL,
    invalidate_citation_audit = function() {
      private$citation_audit_value <- NULL
      invisible(NULL)
    },
    read_only_binding = function(field) {
      tempest_research_workspace_abort(
        paste0(
          "{.field {field}} is a read-only snapshot; use workspace ",
          "mutation methods."
        )
      )
    }
  )
)

#' Create a provisional research workspace
#'
#' `tempest_research_workspace()` creates the run-scoped ledger for material
#' gathered or proposed during scientific research. Accepted knowledge remains
#' in graft; this workspace retains opaque record references and, when pinned,
#' the path-free immutable Graft snapshot needed to reopen that boundary.
#'
#' @param base_snapshot_id Optional opaque identifier for the pinned accepted
#'   knowledge snapshot.
#' @param graft_snapshot Optional real, path-free `graft::GraftSnapshot`.
#' @param max_sources Maximum number of unique resources admitted.
#' @param accepted_graft_references Unnamed list of canonical JSON-compatible
#'   references to accepted graft records.
#'
#' @return A [ResearchWorkspace] object.
#' @export
tempest_research_workspace <- function(
  base_snapshot_id = NULL,
  graft_snapshot = NULL,
  max_sources = Inf,
  accepted_graft_references = list()
) {
  ResearchWorkspace$new(
    base_snapshot_id = base_snapshot_id,
    graft_snapshot = graft_snapshot,
    max_sources = max_sources,
    accepted_graft_references = accepted_graft_references
  )
}
