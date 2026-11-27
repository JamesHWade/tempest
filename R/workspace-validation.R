# Data models and stores

#' Create a deterministic source ID from a URL
#'
#' Use this when a host retriever constructs search results or fetched sources
#' for [tempest_run()] or [tempest_session()]. The same trimmed URL always
#' produces the same ID.
#'
#' @param url A single non-empty HTTP or HTTPS URL string.
#' @return A source ID such as `"Sxxxxxxxxxxxx"`.
#' @examples
#' tempest_source_id("https://example.org/study")
#' @export
tempest_source_id <- function(url) {
  if (!rlang::is_string(url) || is.na(url)) {
    tempest_abort(
      "{.arg url} must be a single non-empty HTTP or HTTPS URL.",
      class = "tempest_source_id_error"
    )
  }
  url <- tempest_trim(url)
  parsed <- tryCatch(curl::curl_parse_url(url), error = function(error) NULL)
  if (
    !nzchar(url) ||
      grepl("[[:space:]]", url) ||
      is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !nzchar(parsed$host %||% "")
  ) {
    tempest_abort(
      "{.arg url} must be a single non-empty HTTP or HTTPS URL.",
      class = "tempest_source_id_error"
    )
  }
  paste0("S", substr(digest::digest(url, algo = "xxhash64"), 1, 12))
}

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

tempest_research_sensitive_name_variants <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(character())
  }
  rendered <- tryCatch(
    commonmark::markdown_text(value, width = 0),
    error = function(...) value
  )
  variants <- unique(c(value, rendered))
  variants <- stringi::stri_trans_nfkc(variants)
  variants <- stringi::stri_replace_all_regex(
    variants,
    "\\p{Default_Ignorable_Code_Point}+",
    ""
  )
  variants <- stringi::stri_replace_all_regex(
    variants,
    "\\p{White_Space}+",
    "_"
  )
  variants <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", variants)
  unique(tolower(gsub("[^A-Za-z0-9]+", "_", variants)))
}

tempest_research_sensitive_name <- function(value) {
  value <- tempest_research_sensitive_name_variants(value)
  if (length(value) == 0L) {
    return(FALSE)
  }
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
  any(
    value %in%
      credential_terms |
      grepl(
        paste0(
          "(^|_)(api_key|access_key|access_token|refresh_token|auth_token|",
          "oauth_token|bearer_token|id_token|session_token|password|passwd|",
          "client_secret|private_key|signing_key|ssh_key|authorization|auth|",
          "headers?|cookies?|set_cookie|credentials?|secret|secret_key)($|_)"
        ),
        value
      ) |
      (grepl("(^|_)token($|_)", value) & !token_metric)
  )
}

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

tempest_research_workspace_reference_id_valid <- function(value) {
  tempest_opaque_identifier_valid(value)
}

tempest_research_workspace_copy <- function(value) {
  rlang::duplicate(value, shallow = FALSE)
}

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

tempest_research_workspace_environment_copy <- function(values) {
  copied <- new.env(parent = emptyenv())
  ids <- ls(values, all.names = TRUE)
  for (id in ids) {
    copied[[id]] <- tempest_research_workspace_copy(values[[id]])
  }
  copied
}

tempest_research_workspace_verification_owner_preflight <- function(
  workspace,
  restoring = FALSE
) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "Verification ownership requires a ResearchWorkspace."
    )
  }
  owner <- workspace$.__enclos_env__$private$verification_owner_token_value
  if (!is.null(owner)) {
    tempest_research_workspace_abort(
      "A ResearchWorkspace cannot be owned by more than one TempestSession."
    )
  }
  if (!isTRUE(restoring)) {
    claims <- workspace$list_proposed_claims()
    verified_summary <- any(vapply(
      claims,
      function(claim) {
        !identical(claim@verification_status, "unverified") ||
          !is.na(claim@verified_at) ||
          !is.na(claim@verifier_model)
      },
      logical(1)
    ))
    if (
      length(workspace$list_claim_supports()) > 0L ||
        verified_summary
    ) {
      tempest_research_workspace_abort(
        paste0(
          "A fresh TempestSession can adopt only an unverified workspace ",
          "without claim-support proof."
        )
      )
    }
  }
  invisible(workspace)
}

tempest_research_workspace_bind_verification_owner <- function(workspace) {
  tempest_research_workspace_verification_owner_preflight(
    workspace,
    restoring = TRUE
  )
  if (
    !identical(tempest_research_workspace_mutation_state(workspace), "open")
  ) {
    tempest_research_workspace_abort(
      "Verification ownership requires an open ResearchWorkspace."
    )
  }
  token <- new.env(parent = emptyenv())
  workspace$.__enclos_env__$private$verification_owner_token_value <- token
  token
}

tempest_research_workspace_assert_standalone_verification <- function(
  workspace
) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "Standalone verification requires a ResearchWorkspace."
    )
  }
  owner <- workspace$.__enclos_env__$private$verification_owner_token_value
  if (!is.null(owner)) {
    tempest_research_workspace_abort(
      paste0(
        "A session-owned ResearchWorkspace must be verified through its ",
        "TempestSession."
      )
    )
  }
  invisible(workspace)
}

tempest_research_workspace_mutation_state <- function(workspace) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "Workspace mutation state requires a ResearchWorkspace."
    )
  }
  state <- workspace$.__enclos_env__$private$mutation_state_value
  if (
    !rlang::is_string(state) ||
      !state %in%
        c(
          "open",
          "publication_locked",
          "sealed"
        )
  ) {
    tempest_research_workspace_abort(
      "ResearchWorkspace mutation state is invalid."
    )
  }
  state
}

tempest_research_workspace_publication_lock <- function(workspace, owner) {
  state <- tempest_research_workspace_mutation_state(workspace)
  private <- workspace$.__enclos_env__$private
  if (
    !identical(state, "open") ||
      !is.environment(owner) ||
      !identical(owner, private$verification_owner_token_value)
  ) {
    tempest_research_workspace_abort(
      "ResearchWorkspace publication requires its exact open-session owner."
    )
  }
  private$publication_owner_token_value <- owner
  private$mutation_state_value <- "publication_locked"
  invisible(workspace)
}

tempest_research_workspace_publication_release <- function(workspace, owner) {
  state <- tempest_research_workspace_mutation_state(workspace)
  private <- workspace$.__enclos_env__$private
  if (
    !identical(state, "publication_locked") ||
      !is.environment(owner) ||
      !identical(owner, private$publication_owner_token_value)
  ) {
    tempest_research_workspace_abort(
      "ResearchWorkspace publication lock cannot be released by this owner."
    )
  }
  private$publication_owner_token_value <- NULL
  private$mutation_state_value <- "open"
  invisible(workspace)
}

tempest_research_workspace_seal <- function(workspace, owner = NULL) {
  state <- tempest_research_workspace_mutation_state(workspace)
  private <- workspace$.__enclos_env__$private
  expected_owner <- if (identical(state, "publication_locked")) {
    private$publication_owner_token_value
  } else {
    private$verification_owner_token_value
  }
  if (
    identical(state, "sealed") ||
      !identical(state %in% c("open", "publication_locked"), TRUE) ||
      (!is.null(expected_owner) && !identical(owner, expected_owner)) ||
      (is.null(expected_owner) && !is.null(owner))
  ) {
    tempest_research_workspace_abort(
      "ResearchWorkspace cannot be sealed by this product owner."
    )
  }
  private$publication_owner_token_value <- NULL
  private$mutation_state_value <- "sealed"
  invisible(workspace)
}
