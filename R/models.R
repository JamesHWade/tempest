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

#' @keywords internal
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
tempest_research_workspace_reference_id_valid <- function(value) {
  tempest_opaque_identifier_valid(value)
}

tempest_research_workspace_snapshot_id <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (rlang::is_string(value) && !is.na(value)) {
    value <- tempest_trim(value)
  }
  if (!tempest_research_workspace_reference_id_valid(value)) {
    tempest_research_workspace_abort(
      paste0(
        "{.arg base_snapshot_id} must be `NULL` or a bounded ",
        "credential-free identifier."
      )
    )
  }
  value
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
      if (!tempest_research_workspace_reference_id_valid(child)) {
        tempest_research_workspace_abort(
          paste0(
            "Accepted graft reference ID {.field {child_path}} must be a ",
            "single non-empty string and bounded credential-free identifier."
          )
        )
      }
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

#' @keywords internal
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

#' ResearchWorkspace (provisional scientific evidence ledger)
#'
#' A mutable, run-scoped workspace for retrieved resources, proposed claims,
#' evidence spans, disputes, and references to accepted graft knowledge. The
#' workspace never grants acceptance to proposed claims; acceptance remains an
#' explicit graft review and commit.
#'
#' @field retrieved_resources Read-only named-list snapshot of typed resources
#'   keyed by resource id.
#' @field proposed_claims Read-only named-list snapshot of provisional claim
#'   records keyed by claim id.
#' @field evidence_spans Read-only named-list snapshot of provisional
#'   evidence-span records.
#' @field claim_supports Read-only named-list snapshot of explicit
#'   claim-by-evidence-span support assessments.
#' @field disputes Read-only named-list snapshot of provisional dispute
#'   records.
#' @field accepted_graft_references Read-only list of opaque references to
#'   accepted graft knowledge used by the research run.
#' @field base_snapshot_id Read-only opaque identifier for the accepted
#'   knowledge snapshot on which this workspace is based.
#' @field graft_snapshot Optional read-only, path-free
#'   `graft::GraftSnapshot` used to reopen the accepted knowledge boundary.
#' @field citation_audit Read-only pair-level projection of the authoritative
#'   claim-support assessments, when available.
#' @field max_sources Maximum number of unique retrieved resources admitted.
#'   Accepted Graft knowledge records inserted by [tempest_knowledge()] are
#'   bounded separately and do not count.
#'
#' @keywords internal
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
      private$mutation_state_value <- "open"
      private$publication_owner_token_value <- NULL
      private$resources_value <- new.env(parent = emptyenv())
      private$claims_value <- new.env(parent = emptyenv())
      private$evidence_spans_value <- new.env(parent = emptyenv())
      private$claim_supports_value <- new.env(parent = emptyenv())
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
      private$assert_mutation_open()
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
    #' @param resource A resource created by [tempest_resource()].
    upsert_retrieved_resource = function(resource) {
      private$assert_mutation_open()
      if (!tempest_is_exact_resource(resource)) {
        tempest_research_workspace_abort(
          "{.arg resource} must be an exact TempestResource record."
        )
      }
      resource <- private$validate_resource(resource)
      resource_id <- resource@resource_id
      previous <- private$resources_value[[resource_id]]
      is_new <- is.null(previous)
      # Accepted Graft records enter through tempest_knowledge()'s own bounded
      # allowlist and do not consume the retrieval-source budget.
      if (
        is_new &&
          !tempest_is_accepted_knowledge_resource(resource) &&
          private$retrieved_source_count() >= self$max_sources
      ) {
        tempest_research_workspace_abort(
          c(
            "ResearchWorkspace resource limit reached.",
            i = "Increase {.arg max_sources} to admit more resources."
          )
        )
      }
      if (
        !is.null(previous) &&
          (!identical(previous@resource_kind, resource@resource_kind) ||
            !identical(previous@locator, resource@locator))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "A retrieved resource cannot change kind or locator while ",
            "retaining the same resource id."
          )
        )
      }
      resource <- tempest_research_workspace_copy(resource)
      resources_value <- tempest_research_workspace_environment_copy(
        private$resources_value
      )
      resources_value[[resource_id]] <- resource
      for (span_id in ls(private$evidence_spans_value, all.names = TRUE)) {
        private$validate_evidence_span(
          private$evidence_spans_value[[span_id]],
          resources_value = resources_value
        )
      }
      for (claim_id in ls(private$claims_value, all.names = TRUE)) {
        private$validate_proposed_claim(
          private$claims_value[[claim_id]],
          evidence_spans_value = private$evidence_spans_value,
          resources_value = resources_value
        )
      }
      previous_resources <- private$resources_value
      previous_claims <- private$claims_value
      previous_claim_supports <- private$claim_supports_value
      previous_audit <- private$citation_audit_value
      if (!identical(previous, resource)) {
        private$assert_verification_mutable()
      }
      tryCatch(
        {
          private$resources_value <- resources_value
          if (!identical(previous, resource)) {
            private$invalidate_verification_state()
          }
        },
        error = function(error) {
          private$resources_value <- previous_resources
          private$claims_value <- previous_claims
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_audit
          stop(error)
        }
      )
      invisible(resource_id)
    },

    #' @description Get a retrieved typed evidence resource by id.
    #' @param resource_id Resource id.
    get_retrieved_resource = function(resource_id) {
      resource <- private$resources_value[[resource_id]] %||% NULL
      if (is.null(resource)) {
        return(NULL)
      }
      tempest_research_workspace_copy(resource)
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
      id <- self$add_proposed_claims(list(claim))[[1]]
      invisible(id)
    },

    #' @description Atomically add proposed claim records to the workspace.
    #' @param claims A list of `tempest_claim` S7 records.
    #' @param commit Optional zero-argument callback committed with the batch.
    add_proposed_claims = function(claims, commit = NULL) {
      private$assert_mutation_open()
      if (!is.list(claims) || is.data.frame(claims)) {
        tempest_research_workspace_abort(
          "{.arg claims} must be a list of {.cls tempest_claim} records."
        )
      }
      if (!is.null(commit) && !is.function(commit)) {
        tempest_research_workspace_abort(
          "{.arg commit} must be `NULL` or a function."
        )
      }
      claims <- lapply(claims, private$validate_proposed_claim)
      ids <- vapply(claims, \(claim) claim@claim_id, character(1))
      if (anyDuplicated(ids)) {
        duplicated_ids <- unique(ids[duplicated(ids)])
        tempest_research_workspace_abort(
          "{.arg claims} contains duplicate claim id{?s}: {.val {duplicated_ids}}."
        )
      }

      claims_value <- tempest_research_workspace_environment_copy(
        private$claims_value
      )
      claims_by_source <- tempest_research_workspace_environment_copy(
        private$claims_by_source
      )
      changed <- FALSE
      for (claim in claims) {
        id <- claim@claim_id
        previous <- claims_value[[id]]
        if (!is.null(previous)) {
          for (sid in previous@source_ids) {
            claims_by_source[[sid]] <- setdiff(
              claims_by_source[[sid]] %||% character(),
              id
            )
          }
        }
        claims_value[[id]] <- claim
        for (sid in claim@source_ids) {
          existing <- claims_by_source[[sid]] %||% character()
          claims_by_source[[sid]] <- unique(c(existing, id))
        }
        changed <- changed || !identical(previous, claim)
      }

      if (changed) {
        private$assert_verification_mutable()
        claims_value <- private$unverified_claim_state(claims_value)
        claim_supports_value <- new.env(parent = emptyenv())
        citation_audit <- NULL
      } else {
        claim_supports_value <-
          tempest_research_workspace_environment_copy(
            private$claim_supports_value
          )
        citation_audit <- private$citation_audit_value
      }
      private$commit_claim_state(
        claims_value,
        claims_by_source,
        claim_supports_value,
        citation_audit,
        commit = commit
      )
      invisible(ids)
    },

    #' @description Atomically add extracted evidence spans and claims.
    #' @param claims A list of `tempest_claim` S7 records.
    #' @param evidence_spans A list of `tempest_evidence_span` S7 records.
    #' @param commit Optional zero-argument callback committed with the batch.
    add_extracted_claim_batch = function(
      claims,
      evidence_spans = list(),
      commit = NULL
    ) {
      private$assert_mutation_open()
      if (
        !is.list(claims) ||
          is.data.frame(claims) ||
          !is.null(names(claims))
      ) {
        tempest_research_workspace_abort(
          "{.arg claims} must be an unnamed list of claim records."
        )
      }
      if (
        !is.list(evidence_spans) ||
          is.data.frame(evidence_spans) ||
          !is.null(names(evidence_spans))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg evidence_spans} must be an unnamed list of evidence-span ",
            "records."
          )
        )
      }
      if (!is.null(commit) && !is.function(commit)) {
        tempest_research_workspace_abort(
          "{.arg commit} must be `NULL` or a function."
        )
      }
      evidence_spans <- lapply(
        evidence_spans,
        private$validate_evidence_span
      )
      span_ids <- vapply(
        evidence_spans,
        \(span) span@evidence_span_id,
        character(1)
      )
      if (anyDuplicated(span_ids)) {
        duplicated_ids <- unique(span_ids[duplicated(span_ids)])
        tempest_research_workspace_abort(
          paste0(
            "{.arg evidence_spans} contains duplicate span id{?s}: ",
            "{.val {duplicated_ids}}."
          )
        )
      }

      evidence_spans_value <- tempest_research_workspace_environment_copy(
        private$evidence_spans_value
      )
      spans_changed <- FALSE
      for (span in evidence_spans) {
        span_id <- span@evidence_span_id
        previous <- evidence_spans_value[[span_id]]
        if (!is.null(previous) && !identical(previous, span)) {
          tempest_research_workspace_abort(
            paste0(
              "Extraction cannot replace evidence span {.val {span_id}} ",
              "with different content."
            )
          )
        }
        evidence_spans_value[[span_id]] <- span
        spans_changed <- spans_changed || is.null(previous)
      }

      claims <- lapply(
        claims,
        private$validate_proposed_claim,
        evidence_spans_value = evidence_spans_value
      )
      claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
      if (anyDuplicated(claim_ids)) {
        duplicated_ids <- unique(claim_ids[duplicated(claim_ids)])
        tempest_research_workspace_abort(
          "{.arg claims} contains duplicate claim id{?s}: {.val {duplicated_ids}}."
        )
      }

      claims_value <- tempest_research_workspace_environment_copy(
        private$claims_value
      )
      claims_by_source <- tempest_research_workspace_environment_copy(
        private$claims_by_source
      )
      claims_changed <- FALSE
      for (claim in claims) {
        claim_id <- claim@claim_id
        previous <- claims_value[[claim_id]]
        if (!is.null(previous)) {
          for (source_id in previous@source_ids) {
            claims_by_source[[source_id]] <- setdiff(
              claims_by_source[[source_id]] %||% character(),
              claim_id
            )
          }
        }
        claims_value[[claim_id]] <- claim
        for (source_id in claim@source_ids) {
          existing <- claims_by_source[[source_id]] %||% character()
          claims_by_source[[source_id]] <- unique(c(existing, claim_id))
        }
        claims_changed <- claims_changed || !identical(previous, claim)
      }
      referenced_span_ids <- unique(unlist(
        lapply(
          ls(claims_value, all.names = TRUE),
          \(claim_id) claims_value[[claim_id]]@evidence_span_ids
        ),
        use.names = FALSE
      ))
      unreferenced_span_ids <- setdiff(span_ids, referenced_span_ids)
      if (length(unreferenced_span_ids) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Extracted evidence span{?s} must be linked to a claim: ",
            "{.val {unreferenced_span_ids}}."
          )
        )
      }

      if (spans_changed || claims_changed) {
        private$assert_verification_mutable()
        claims_value <- private$unverified_claim_state(claims_value)
        claim_supports_value <- new.env(parent = emptyenv())
        citation_audit <- NULL
      } else {
        claim_supports_value <-
          tempest_research_workspace_environment_copy(
            private$claim_supports_value
          )
        citation_audit <- private$citation_audit_value
      }
      private$commit_extraction_state(
        claims_value,
        claims_by_source,
        evidence_spans_value,
        claim_supports_value,
        citation_audit,
        commit = commit
      )
      invisible(claim_ids)
    },

    #' @description Atomically replace exact claim-by-span support assessments.
    #' @param claim_supports An unnamed complete list of
    #'   `tempest_claim_support` records.
    #' @param min_support_score Bound support threshold used to derive claim
    #'   summaries.
    #' @param verifier Bounded verifier-model identifier or `NA`.
    #' @param verified_at Exact canonical verification-batch timestamp bound
    #'   into every corresponding verification-stage record.
    #' @param .verification_owner_token Internal process-local session
    #'   capability. Standalone workspaces require `NULL`.
    #' @param commit Optional zero-argument callback committed with the batch.
    verify_proposed_claims_batch = function(
      claim_supports,
      verified_at,
      min_support_score = 0.7,
      verifier = NA_character_,
      .verification_owner_token = NULL,
      commit = NULL
    ) {
      private$assert_verification_write(.verification_owner_token)
      if (
        !is.list(claim_supports) ||
          is.data.frame(claim_supports) ||
          !is.null(names(claim_supports))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg claim_supports} must be an unnamed list of exact ",
            "claim-support records."
          )
        )
      }
      if (!is.null(commit) && !is.function(commit)) {
        tempest_research_workspace_abort(
          "{.arg commit} must be `NULL` or a function."
        )
      }
      owner <- private$verification_owner_token_value
      if (
        (!is.null(owner) &&
          !identical(.verification_owner_token, owner)) ||
          (is.null(owner) && !is.null(.verification_owner_token))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Session-owned verification requires its exact process-local ",
            "ownership capability."
          )
        )
      }
      min_support_score <- tempest_normalize_min_support_score(
        min_support_score
      )
      if (!tempest_ledger_identifier_valid(verifier, optional = TRUE)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg verifier} must be `NA` or a bounded credential-free ",
            "identifier."
          )
        )
      }
      if (!tempest_ledger_timestamp_valid(verified_at)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg verified_at} must be one exact canonical UTC batch ",
            "timestamp."
          )
        )
      }
      claim_supports <- private$validate_claim_support_batch(
        claim_supports,
        min_support_score
      )
      claims_value <- private$verified_claim_state(
        private$claims_value,
        claim_supports,
        verifier,
        verified_at
      )
      claim_supports_value <- new.env(parent = emptyenv())
      for (support in claim_supports) {
        claim_supports_value[[support@claim_support_id]] <-
          tempest_research_workspace_copy(support)
      }
      citation_audit <- tempest_claim_supports_tibble(lapply(
        sort(vapply(
          claim_supports,
          \(support) support@claim_support_id,
          character(1)
        )),
        \(support_id) claim_supports_value[[support_id]]
      ))
      private$commit_verification_state(
        claims_value,
        claim_supports_value,
        citation_audit,
        commit = commit
      )
      invisible(self$citation_audit)
    },

    #' @description Get a claim-support assessment by id.
    #' @param claim_support_id Exact derived support identifier.
    get_claim_support = function(claim_support_id) {
      support <- private$claim_supports_value[[claim_support_id]] %||% NULL
      tempest_research_workspace_copy(support)
    },

    #' @description List claim-support assessments in deterministic id order.
    list_claim_supports = function() {
      ids <- sort(ls(private$claim_supports_value, all.names = TRUE))
      purrr::map(ids, self$get_claim_support)
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
      private$assert_mutation_open()
      span <- private$validate_evidence_span(span)
      id <- span@evidence_span_id
      previous <- private$evidence_spans_value[[id]]
      if (!is.null(previous) && !identical(previous, span)) {
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
              "Cannot replace linked evidence span {.val {id}} with ",
              "different content; it is cited by claim{?s}: ",
              "{.val {linked_claim_ids}}."
            )
          )
        }
      }
      if (!identical(previous, span)) {
        private$assert_verification_mutable()
        private$evidence_spans_value[[id]] <- span
        private$invalidate_verification_state()
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
      private$assert_mutation_open()
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
        supporting_quotes <- lapply(
          evidence_span_ids,
          function(evidence_span_id) {
            private$evidence_spans_value[[evidence_span_id]]@quote
          }
        )
        supporting_quotes <- Filter(
          \(quote) !is.na(quote),
          supporting_quotes
        )
        updated <- S7::set_props(
          claim,
          evidence_span_ids = evidence_span_ids,
          supporting_quotes = unname(supporting_quotes)
        )
        updated <- private$validate_proposed_claim(updated)
        private$assert_verification_mutable()
        private$claims_value[[claim_id]] <- updated
        private$invalidate_verification_state()
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

    #' @description Add a dispute.
    #' @param dispute A `tempest_dispute` S7 record.
    add_dispute = function(dispute) {
      private$assert_mutation_open()
      if (!S7::S7_inherits(dispute, tempest_dispute)) {
        tempest_research_workspace_abort(
          "{.arg dispute} must be a {.cls tempest_dispute} record."
        )
      }
      tryCatch(
        S7::validate(dispute),
        error = function(error) {
          tempest_research_workspace_abort(
            "The dispute failed live validation.",
            parent = error
          )
        }
      )
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
      if (!identical(previous, dispute)) {
        private$assert_verification_mutable()
        private$disputes_value[[id]] <- dispute
        private$invalidate_verification_state()
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
      private$assert_mutation_open()
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

    #' @description Validate all authoritative workspace cross-record links.
    validate_integrity = function() {
      resource_ids <- sort(ls(private$resources_value, all.names = TRUE))
      for (resource_id in resource_ids) {
        resource <- private$validate_resource(
          private$resources_value[[resource_id]]
        )
        if (!identical(resource@resource_id, resource_id)) {
          tempest_research_workspace_abort(
            "Retrieved-resource storage key does not match its resource id."
          )
        }
      }
      span_ids <- sort(ls(private$evidence_spans_value, all.names = TRUE))
      for (span_id in span_ids) {
        span <- private$validate_evidence_span(
          private$evidence_spans_value[[span_id]]
        )
        if (!identical(span@evidence_span_id, span_id)) {
          tempest_research_workspace_abort(
            "Evidence-span storage key does not match its span id."
          )
        }
      }
      claim_ids <- sort(ls(private$claims_value, all.names = TRUE))
      expected_by_source <- new.env(parent = emptyenv())
      for (claim_id in claim_ids) {
        claim <- private$validate_proposed_claim(
          private$claims_value[[claim_id]]
        )
        if (!identical(claim@claim_id, claim_id)) {
          tempest_research_workspace_abort(
            "Claim storage key does not match its claim id."
          )
        }
        for (source_id in claim@source_ids) {
          expected_by_source[[source_id]] <- sort(unique(c(
            expected_by_source[[source_id]] %||% character(),
            claim_id
          )))
        }
      }
      indexed_source_ids <- sort(ls(private$claims_by_source, all.names = TRUE))
      expected_source_ids <- sort(ls(expected_by_source, all.names = TRUE))
      if (!identical(indexed_source_ids, expected_source_ids)) {
        tempest_research_workspace_abort(
          "Claim-by-source index does not match authoritative claims."
        )
      }
      for (source_id in expected_source_ids) {
        if (
          !identical(
            sort(private$claims_by_source[[source_id]]),
            expected_by_source[[source_id]]
          )
        ) {
          tempest_research_workspace_abort(
            "Claim-by-source index contains inconsistent claim ids."
          )
        }
      }
      for (dispute in self$list_disputes()) {
        tryCatch(
          S7::validate(dispute),
          error = function(error) {
            tempest_research_workspace_abort(
              "A dispute failed live validation.",
              parent = error
            )
          }
        )
        missing_claims <- setdiff(dispute@claim_ids, claim_ids)
        if (length(missing_claims) > 0L) {
          tempest_research_workspace_abort(
            "A dispute cites claims absent from the workspace."
          )
        }
      }
      supports <- self$list_claim_supports()
      if (length(supports) > 0L) {
        supports <- private$validate_claim_support_batch(supports)
        supports <- supports[order(vapply(
          supports,
          \(support) support@claim_support_id,
          character(1)
        ))]
        expected_audit <- tempest_claim_supports_tibble(supports)
        if (!identical(private$citation_audit_value, expected_audit)) {
          tempest_research_workspace_abort(
            paste0(
              "Citation audit must exactly project the authoritative ",
              "claim-support records."
            )
          )
        }
        for (claim_id in claim_ids) {
          claim_supports <- Filter(
            \(support) identical(support@claim_id, claim_id),
            supports
          )
          expected <- private$aggregate_claim_support(claim_supports)
          claim <- private$claims_value[[claim_id]]
          if (
            !identical(claim@verification_status, expected$status) ||
              !isTRUE(all.equal(
                claim@support_score,
                expected$score,
                check.attributes = FALSE
              )) ||
              is.na(claim@verified_at)
          ) {
            tempest_research_workspace_abort(
              "Claim verification summary does not match its support records."
            )
          }
        }
      } else {
        if (
          !is.null(private$citation_audit_value) &&
            nrow(private$citation_audit_value) != 0L
        ) {
          tempest_research_workspace_abort(
            "Citation audit cannot exist without claim-support records."
          )
        }
        stale <- vapply(
          self$list_proposed_claims(),
          \(claim) !identical(claim@verification_status, "unverified"),
          logical(1)
        )
        if (any(stale)) {
          tempest_research_workspace_abort(
            "Verified claim summaries require exact claim-support records."
          )
        }
      }
      invisible(self)
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
        proposed_claims = tempest_claims_tibble(self$list_proposed_claims()),
        claim_supports = tempest_claim_supports_tibble(
          self$list_claim_supports()
        )
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
    claim_supports = function(value) {
      if (!missing(value)) {
        private$read_only_binding("claim_supports")
      }
      tempest_research_workspace_values(private$claim_supports_value)
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
        private$read_only_binding("citation_audit")
      }
      if (is.null(private$citation_audit_value)) {
        return(NULL)
      }
      rlang::duplicate(private$citation_audit_value, shallow = FALSE)
    }
  ),
  private = list(
    retrieved_source_count = function() {
      sum(
        !vapply(
          self$list_retrieved_resources(),
          tempest_is_accepted_knowledge_resource,
          logical(1)
        )
      )
    },
    mutation_state_value = "open",
    publication_owner_token_value = NULL,
    resources_value = NULL,
    claims_value = NULL,
    evidence_spans_value = NULL,
    claim_supports_value = NULL,
    disputes_value = NULL,
    max_sources_value = NULL,
    claims_by_source = NULL,
    base_snapshot_id_value = NULL,
    graft_snapshot_value = NULL,
    accepted_graft_references_value = NULL,
    citation_audit_value = NULL,
    verification_owner_token_value = NULL,
    assert_mutation_open = function() {
      if (!identical(private$mutation_state_value, "open")) {
        tempest_research_workspace_abort(
          paste0(
            "ResearchWorkspace is read-only during or after terminal ",
            "product publication."
          )
        )
      }
      invisible(NULL)
    },
    assert_verification_write = function(owner) {
      state <- private$mutation_state_value
      if (identical(state, "sealed")) {
        tempest_research_workspace_abort(
          "A sealed ResearchWorkspace cannot be verified again."
        )
      }
      if (
        identical(state, "publication_locked") &&
          !identical(owner, private$publication_owner_token_value)
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Only the exact publication owner may verify a locked ",
            "ResearchWorkspace."
          )
        )
      }
      if (!state %in% c("open", "publication_locked")) {
        tempest_research_workspace_abort(
          "ResearchWorkspace mutation state is invalid."
        )
      }
      invisible(NULL)
    },
    validate_resource = function(resource) {
      if (!tempest_is_exact_resource(resource)) {
        tempest_research_workspace_abort(
          "Retrieved resources must be exact TempestResource records."
        )
      }
      tryCatch(
        {
          S7::validate(resource)
          tempest_resource_data(resource, include_content = TRUE)
        },
        error = function(error) {
          tempest_research_workspace_abort(
            "A retrieved resource failed live validation.",
            parent = error
          )
        }
      )
      if (!is.null(resource@content)) {
        expected_hash <- tempest_product_content_hash(
          resource@content,
          resource@media_type
        )
        if (
          is.na(resource@content_hash) ||
            !identical(resource@content_hash, expected_hash)
        ) {
          tempest_research_workspace_abort(
            paste0(
              "Retrieved-resource content hash must exactly match its ",
              "captured inline content."
            )
          )
        }
      }
      tempest_research_workspace_copy(resource)
    },
    validate_proposed_claim = function(
      claim,
      evidence_spans_value = private$evidence_spans_value,
      resources_value = private$resources_value
    ) {
      if (!S7::S7_inherits(claim, tempest_claim)) {
        tempest_research_workspace_abort(
          "{.arg claims} must contain only {.cls tempest_claim} records."
        )
      }
      tryCatch(
        S7::validate(claim),
        error = function(error) {
          tempest_research_workspace_abort(
            "A proposed claim failed live validation.",
            parent = error
          )
        }
      )
      missing_sources <- setdiff(
        claim@source_ids,
        ls(resources_value, all.names = TRUE)
      )
      if (length(missing_sources) > 0L) {
        tempest_research_workspace_abort(
          "Claim cites unknown source id{?s}: {.val {missing_sources}}."
        )
      }
      missing_contradicting_sources <- setdiff(
        claim@contradicting_source_ids,
        ls(resources_value, all.names = TRUE)
      )
      if (length(missing_contradicting_sources) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Claim cites unknown contradicting source id{?s}: ",
            "{.val {missing_contradicting_sources}}."
          )
        )
      }
      missing_spans <- setdiff(
        claim@evidence_span_ids,
        ls(evidence_spans_value, all.names = TRUE)
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
            !evidence_spans_value[[span_id]]@source_id %in%
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
      supporting_quotes <- claim@supporting_quotes
      valid_quotes <- is.list(supporting_quotes) &&
        !is.data.frame(supporting_quotes) &&
        is.null(names(supporting_quotes)) &&
        all(vapply(supporting_quotes, rlang::is_string, logical(1)))
      if (!valid_quotes) {
        tempest_research_workspace_abort(
          "Claim supporting quotes must be a flat unnamed string array."
        )
      }
      expected_quotes <- lapply(
        claim@evidence_span_ids,
        function(span_id) evidence_spans_value[[span_id]]@quote
      )
      expected_quotes <- Filter(
        \(quote) !is.na(quote),
        expected_quotes
      )
      if (!identical(supporting_quotes, unname(expected_quotes))) {
        tempest_research_workspace_abort(
          paste0(
            "Claim supporting quotes must exactly match its referenced ",
            "quoted evidence spans in order."
          )
        )
      }
      tempest_research_workspace_copy(claim)
    },
    validate_evidence_span = function(
      span,
      resources_value = private$resources_value
    ) {
      if (!S7::S7_inherits(span, tempest_evidence_span)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg evidence_spans} must contain only ",
            "{.cls tempest_evidence_span} records."
          )
        )
      }
      tryCatch(
        S7::validate(span),
        error = function(error) {
          tempest_research_workspace_abort(
            "An evidence span failed live validation.",
            parent = error
          )
        }
      )
      resource <- resources_value[[span@source_id]]
      if (is.null(resource)) {
        tempest_research_workspace_abort(
          "Evidence span cites unknown source id: {.val {span@source_id}}."
        )
      }
      if (!is.na(span@quote)) {
        if (!nzchar(span@quote)) {
          tempest_research_workspace_abort(
            "Quoted evidence spans must contain a non-empty quote."
          )
        }
        source <- tempest_resource_as_source(resource)
        captured_text <- c(
          source$content_text %||% NA_character_,
          source$snippet %||% NA_character_,
          source$context_text %||% NA_character_
        )
        captured_text <- unique(captured_text[
          !is.na(captured_text) & nzchar(captured_text)
        ])
        if (length(captured_text) == 0L) {
          tempest_research_workspace_abort(
            paste0(
              "Evidence-span quote cannot be validated because source ",
              "{.val {span@source_id}} has no captured text."
            )
          )
        }
        has_offsets <- !is.na(span@start_offset)
        matched <- any(vapply(
          captured_text,
          function(text) {
            if (!has_offsets) {
              return(grepl(span@quote, text, fixed = TRUE))
            }
            span@end_offset <= nchar(text, type = "chars") &&
              identical(
                substr(text, span@start_offset + 1L, span@end_offset),
                span@quote
              )
          },
          logical(1)
        ))
        if (!matched) {
          tempest_research_workspace_abort(
            paste0(
              "Evidence-span quote and zero-based half-open offsets are not ",
              "captured by source {.val {span@source_id}}."
            )
          )
        }
      }
      tempest_research_workspace_copy(span)
    },
    validate_claim_support = function(
      support,
      claims_value = private$claims_value,
      evidence_spans_value = private$evidence_spans_value,
      resources_value = private$resources_value
    ) {
      if (!S7::S7_inherits(support, TempestClaimSupport)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg claim_supports} must contain only exact ",
            "{.cls tempest_claim_support} records."
          )
        )
      }
      tryCatch(
        S7::validate(support),
        error = function(error) {
          tempest_research_workspace_abort(
            "A claim-support assessment failed live validation."
          )
        }
      )
      claim <- claims_value[[support@claim_id]]
      if (is.null(claim)) {
        tempest_research_workspace_abort(
          "Claim support cites unknown claim id: {.val {support@claim_id}}."
        )
      }
      span <- evidence_spans_value[[support@evidence_span_id]]
      if (is.null(span)) {
        tempest_research_workspace_abort(
          paste0(
            "Claim support cites unknown evidence-span id: ",
            "{.val {support@evidence_span_id}}."
          )
        )
      }
      if (!support@evidence_span_id %in% claim@evidence_span_ids) {
        tempest_research_workspace_abort(
          "Claim support binds an evidence span not cited by its claim."
        )
      }
      if (
        !identical(support@source_id, span@source_id) ||
          !support@source_id %in% claim@source_ids ||
          is.null(resources_value[[support@source_id]])
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Claim-support source must exactly match the authoritative span ",
            "and cited claim source."
          )
        )
      }
      if (is.na(span@quote) || !nzchar(span@quote)) {
        tempest_research_workspace_abort(
          "Claim support requires a non-empty captured evidence-span quote."
        )
      }
      private$validate_evidence_span(
        span,
        resources_value = resources_value
      )
      tempest_research_workspace_copy(support)
    },
    validate_claim_support_batch = function(
      supports,
      min_support_score = NULL,
      claims_value = private$claims_value,
      evidence_spans_value = private$evidence_spans_value,
      resources_value = private$resources_value
    ) {
      supports <- lapply(
        supports,
        private$validate_claim_support,
        claims_value = claims_value,
        evidence_spans_value = evidence_spans_value,
        resources_value = resources_value
      )
      support_ids <- vapply(
        supports,
        \(support) support@claim_support_id,
        character(1)
      )
      if (anyDuplicated(support_ids)) {
        tempest_research_workspace_abort(
          "Claim-support batches cannot contain duplicate pair identities."
        )
      }
      claim_ids <- sort(ls(claims_value, all.names = TRUE))
      expected_ids <- character()
      for (claim_id in claim_ids) {
        claim <- claims_value[[claim_id]]
        if (length(claim@evidence_span_ids) == 0L) {
          tempest_research_workspace_abort(
            paste0(
              "Every verified claim requires at least one exact evidence ",
              "span: {.val {claim_id}}."
            )
          )
        }
        span_sources <- vapply(
          claim@evidence_span_ids,
          \(span_id) evidence_spans_value[[span_id]]@source_id,
          character(1)
        )
        if (!setequal(unique(span_sources), claim@source_ids)) {
          tempest_research_workspace_abort(
            paste0(
              "Every cited claim source requires at least one exact evidence ",
              "span: {.val {claim_id}}."
            )
          )
        }
        expected_ids <- c(
          expected_ids,
          vapply(
            claim@evidence_span_ids,
            \(span_id) tempest_claim_support_id(claim_id, span_id),
            character(1)
          )
        )
      }
      if (!setequal(support_ids, expected_ids)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg claim_supports} must exactly cover every current ",
            "claim-by-evidence-span pair."
          )
        )
      }
      if (!is.null(min_support_score)) {
        min_support_score <- tempest_normalize_min_support_score(
          min_support_score
        )
        below_threshold <- vapply(
          supports,
          function(support) {
            identical(support@verification_status, "supported") &&
              (is.na(support@support_score) ||
                support@support_score < min_support_score)
          },
          logical(1)
        )
        if (any(below_threshold)) {
          tempest_research_workspace_abort(
            paste0(
              "Supported claim-span assessments must satisfy the exact ",
              "verification threshold."
            )
          )
        }
      }
      supports[match(expected_ids, support_ids)]
    },
    aggregate_claim_support = function(supports) {
      tempest_claim_support_aggregate(supports)
    },
    verified_claim_state = function(
      claims_value,
      supports,
      verifier,
      verified_at
    ) {
      claims_value <- tempest_research_workspace_environment_copy(claims_value)
      claim_ids <- sort(ls(claims_value, all.names = TRUE))
      for (claim_id in claim_ids) {
        claim_supports <- Filter(
          \(support) identical(support@claim_id, claim_id),
          supports
        )
        aggregate <- private$aggregate_claim_support(claim_supports)
        claims_value[[claim_id]] <- S7::set_props(
          claims_value[[claim_id]],
          verification_status = aggregate$status,
          support_score = aggregate$score,
          verifier_model = verifier,
          verified_at = verified_at
        )
      }
      claims_value
    },
    unverified_claim_state = function(claims_value) {
      claims_value <- tempest_research_workspace_environment_copy(claims_value)
      for (claim_id in ls(claims_value, all.names = TRUE)) {
        claims_value[[claim_id]] <- S7::set_props(
          claims_value[[claim_id]],
          verification_status = "unverified",
          support_score = NA_real_,
          verified_at = NA_character_,
          verifier_model = NA_character_
        )
      }
      claims_value
    },
    commit_claim_state = function(
      claims_value,
      claims_by_source,
      claim_supports_value,
      citation_audit,
      commit = NULL
    ) {
      previous_claims <- private$claims_value
      previous_claims_by_source <- private$claims_by_source
      previous_claim_supports <- private$claim_supports_value
      previous_citation_audit <- private$citation_audit_value
      tryCatch(
        {
          private$claims_value <- claims_value
          private$claims_by_source <- claims_by_source
          private$claim_supports_value <- claim_supports_value
          private$citation_audit_value <- citation_audit
          if (!is.null(commit)) {
            commit()
          }
        },
        error = function(error) {
          private$claims_value <- previous_claims
          private$claims_by_source <- previous_claims_by_source
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_citation_audit
          stop(error)
        }
      )
      invisible(NULL)
    },
    commit_extraction_state = function(
      claims_value,
      claims_by_source,
      evidence_spans_value,
      claim_supports_value,
      citation_audit,
      commit = NULL
    ) {
      previous_claims <- private$claims_value
      previous_claims_by_source <- private$claims_by_source
      previous_evidence_spans <- private$evidence_spans_value
      previous_claim_supports <- private$claim_supports_value
      previous_citation_audit <- private$citation_audit_value
      tryCatch(
        {
          private$claims_value <- claims_value
          private$claims_by_source <- claims_by_source
          private$evidence_spans_value <- evidence_spans_value
          private$claim_supports_value <- claim_supports_value
          private$citation_audit_value <- citation_audit
          if (!is.null(commit)) {
            commit()
          }
        },
        error = function(error) {
          private$claims_value <- previous_claims
          private$claims_by_source <- previous_claims_by_source
          private$evidence_spans_value <- previous_evidence_spans
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_citation_audit
          stop(error)
        }
      )
      invisible(NULL)
    },
    commit_verification_state = function(
      claims_value,
      claim_supports_value,
      citation_audit,
      commit = NULL
    ) {
      previous_claims <- private$claims_value
      previous_claim_supports <- private$claim_supports_value
      previous_citation_audit <- private$citation_audit_value
      tryCatch(
        {
          private$claims_value <- claims_value
          private$claim_supports_value <- claim_supports_value
          private$citation_audit_value <- citation_audit
          if (!is.null(commit)) {
            commit()
          }
        },
        error = function(error) {
          private$claims_value <- previous_claims
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_citation_audit
          stop(error)
        }
      )
      invisible(NULL)
    },
    assert_verification_mutable = function() {
      if (length(ls(private$claim_supports_value, all.names = TRUE)) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Verified ResearchWorkspace evidence is sealed; current ",
            "claim-support proof cannot be invalidated or superseded."
          )
        )
      }
      invisible(NULL)
    },
    invalidate_verification_state = function() {
      private$assert_verification_mutable()
      private$claims_value <- private$unverified_claim_state(
        private$claims_value
      )
      private$claim_supports_value <- new.env(parent = emptyenv())
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
#' @keywords internal
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
