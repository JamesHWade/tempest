# Product-owned validation primitives

tempest_product_validation_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_product_validation_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_product_scalar <- function(
  value,
  arg,
  allow_na = FALSE,
  default = NULL
) {
  if (is.null(value) && !is.null(default)) {
    value <- default
  }
  valid <- is.character(value) && length(value) == 1L
  if (valid && is.na(value)) {
    valid <- isTRUE(allow_na)
  }
  if (valid && !is.na(value)) {
    value <- tempest_trim(value)
    valid <- nzchar(value)
  }
  if (!valid) {
    tempest_product_validation_abort(
      "{.arg {arg}} must be a single non-empty string."
    )
  }
  value
}

tempest_product_character <- function(value, arg) {
  value <- value %||% character()
  if (!is.character(value) || anyNA(value)) {
    tempest_product_validation_abort(
      "{.arg {arg}} must be a character vector without missing values."
    )
  }
  value <- unique(tempest_trim(value))
  if (any(!nzchar(value))) {
    tempest_product_validation_abort(
      "{.arg {arg}} cannot contain empty strings."
    )
  }
  value
}

tempest_product_character_array <- function(value, arg) {
  if (
    is.character(value) &&
      !is.object(value) &&
      is.null(names(value)) &&
      !anyNA(value) &&
      all(nzchar(value))
  ) {
    return(value)
  }
  valid_list <- is.list(value) &&
    !is.data.frame(value) &&
    !is.object(value) &&
    is.null(names(value)) &&
    all(vapply(
      value,
      \(element) {
        rlang::is_string(element) && !is.na(element) && nzchar(element)
      },
      logical(1)
    ))
  if (!isTRUE(valid_list)) {
    tempest_product_validation_abort(
      "{.arg {arg}} must be an exact unnamed array of non-empty strings."
    )
  }
  unname(vapply(value, identity, character(1)))
}

tempest_product_list <- function(value, arg) {
  value <- value %||% list()
  if (!is.list(value) || is.data.frame(value)) {
    tempest_product_validation_abort("{.arg {arg}} must be a list.")
  }
  value
}

tempest_product_canonical_list <- function(value, arg) {
  value <- tempest_product_list(value, arg)
  tryCatch(
    tempest_product_canonical_json(value),
    error = function(error) {
      tempest_product_validation_abort(
        "{.arg {arg}} must contain only canonical JSON-compatible values.",
        parent = error
      )
    }
  )
  value
}

tempest_product_path_is_safe <- function(path) {
  if (!rlang::is_string(path) || !nzchar(path)) {
    return(FALSE)
  }
  path <- gsub("\\\\", "/", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  !grepl("^(/|~|[A-Za-z]:)", path) &&
    length(parts) > 0L &&
    all(nzchar(parts)) &&
    !any(parts %in% c(".", ".."))
}

tempest_product_serializable_list <- function(value, arg) {
  value <- tempest_product_canonical_list(value, arg)
  sensitive <- c(
    tempest_contract_sensitive_names(value, arg),
    tempest_contract_sensitive_values(value, arg)
  )
  if (length(sensitive) > 0L) {
    tempest_product_validation_abort(
      "{.arg {arg}} cannot contain credential or secret material."
    )
  }
  value
}

tempest_product_flag <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    tempest_product_validation_abort("{.arg {arg}} must be `TRUE` or `FALSE`.")
  }
  value
}

tempest_product_version <- function(value, arg = "version") {
  value <- tempest_product_scalar(value, arg)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._+-]*$", value)) {
    tempest_product_validation_abort(
      "{.arg {arg}} must contain only letters, numbers, `.`, `_`, `+`, or `-`."
    )
  }
  value
}

tempest_product_prop_chr <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

tempest_product_prop_list <- function() {
  S7::new_property(S7::class_list, default = list())
}

tempest_product_knowledge_view <- function(
  program_set,
  knowledge_view,
  restoring = FALSE
) {
  required <- tempest_program_set_requires_knowledge_view(program_set)
  if (is.null(knowledge_view)) {
    if (required && !isTRUE(restoring)) {
      tempest_governed_procedure_abort(
        paste0(
          "A ProgramSet with governed procedures requires its exact pinned ",
          "{.arg knowledge_view}."
        )
      )
    }
    return(list(
      view = NULL,
      snapshot = NULL,
      reference = NULL,
      required = required
    ))
  }
  snapshot <- tryCatch(
    tempest_governed_procedure_view_snapshot(knowledge_view),
    error = function(error) {
      tempest_governed_procedure_abort(
        "{.arg knowledge_view} must be a valid pinned Graft view."
      )
    }
  )
  snapshot <- tempest_research_workspace_graft_snapshot(snapshot)
  reference <- tempest_snapshot_reference(snapshot)
  entries <- tempest_program_set_entries(program_set)
  governed <- Filter(
    Negate(is.null),
    lapply(entries, \(entry) entry$governed_procedure_ref)
  )
  snapshot_fields <- c(
    "store_id",
    "snapshot_id",
    "schema_build_digest",
    "commit_order"
  )
  mismatched <- names(governed)[
    !vapply(
      governed,
      \(procedure) {
        identical(procedure[snapshot_fields], reference[snapshot_fields])
      },
      logical(1)
    )
  ]
  if (length(mismatched) > 0L) {
    tempest_governed_procedure_abort(
      paste0(
        "Governed procedure references do not belong to the supplied pinned ",
        "view: {.val {mismatched}}."
      )
    )
  }
  list(
    view = knowledge_view,
    snapshot = snapshot,
    reference = reference,
    required = required
  )
}

tempest_product_workspace_validate <- function(
  workspace,
  knowledge,
  arg = "retriever"
) {
  if (is.null(knowledge$view)) {
    return(workspace)
  }
  snapshot <- workspace$graft_snapshot
  if (
    is.null(snapshot) ||
      !identical(workspace$base_snapshot_id, knowledge$reference$snapshot_id)
  ) {
    tempest_governed_procedure_abort(
      "{.arg {arg}} workspace does not use the supplied pinned knowledge view."
    )
  }
  workspace_reference <- tempest_snapshot_reference(snapshot)
  if (!identical(workspace_reference, knowledge$reference)) {
    tempest_governed_procedure_abort(
      "{.arg {arg}} workspace snapshot is not exactly the supplied pinned view."
    )
  }
  workspace
}

# Shared credential-safe validation retained by product records and errors.
tempest_contract_sensitive_names <- function(value, path) {
  if (!is.list(value) || length(value) == 0L) {
    return(character())
  }
  value_names <- names(value)
  found <- character()
  if (!is.null(value_names)) {
    sensitive <- vapply(
      value_names,
      tempest_research_sensitive_name,
      logical(1)
    )
    if (any(sensitive)) {
      found <- paste0(path, "$", value_names[sensitive])
    }
  }
  child_paths <- if (is.null(value_names)) {
    paste0(path, "[[", seq_along(value), "]]")
  } else {
    paste0(path, "$", value_names)
  }
  c(
    found,
    unlist(
      Map(tempest_contract_sensitive_names, value, child_paths),
      use.names = FALSE
    )
  )
}

tempest_contract_decode_numeric_entities <- function(value) {
  decode_one <- function(text) {
    matches <- gregexpr(
      "&#(?:[xX][0-9A-Fa-f]{1,6}|[0-9]{1,7});",
      text,
      perl = TRUE
    )[[1L]]
    if (identical(matches[[1L]], -1L)) {
      return(text)
    }
    references <- regmatches(text, list(matches))[[1L]]
    replacements <- vapply(
      references,
      function(reference) {
        encoded <- substring(reference, 3L, nchar(reference) - 1L)
        base <- 10L
        if (startsWith(encoded, "x") || startsWith(encoded, "X")) {
          encoded <- substring(encoded, 2L)
          base <- 16L
        }
        codepoint <- suppressWarnings(strtoi(encoded, base = base))
        if (is.na(codepoint) || codepoint < 32L || codepoint > 126L) {
          return(reference)
        }
        intToUtf8(codepoint)
      },
      character(1)
    )
    regmatches(text, list(matches)) <- list(replacements)
    text
  }
  unname(vapply(value, decode_one, character(1)))
}

tempest_contract_decode_named_entities <- function(value) {
  replacements <- c(
    amp = "&",
    apos = "'",
    ast = "*",
    bsol = "\\",
    colon = ":",
    comma = ",",
    commat = "@",
    dollar = "$",
    equals = "=",
    excl = "!",
    grave = "`",
    gt = ">",
    lcub = "{",
    lpar = "(",
    lsqb = "[",
    lt = "<",
    nbsp = " ",
    NewLine = "\n",
    num = "#",
    percnt = "%",
    period = ".",
    plus = "+",
    quest = "?",
    quot = '"',
    rcub = "}",
    rpar = ")",
    rsqb = "]",
    semi = ";",
    sol = "/",
    Tab = "\t",
    verbar = "|",
    lowbar = "_"
  )
  for (entity in names(replacements)) {
    value <- gsub(
      paste0("&", entity, ";"),
      replacements[[entity]],
      value,
      fixed = TRUE
    )
  }
  value
}

tempest_contract_decode_entities <- function(value) {
  value |>
    tempest_contract_decode_numeric_entities() |>
    tempest_contract_decode_named_entities()
}

tempest_contract_unescape_markdown <- function(value) {
  punctuation <- intToUtf8(
    c(33:47, 58:64, 91:96, 123:126),
    multiple = TRUE
  )
  for (mark in punctuation) {
    value <- gsub(paste0("\\", mark), mark, value, fixed = TRUE)
  }
  value
}

tempest_contract_rendered_markdown <- function(value) {
  unname(vapply(
    value,
    commonmark::markdown_text,
    character(1),
    width = 0
  ))
}

tempest_contract_url_decode <- function(value) {
  unname(vapply(
    value,
    function(text) {
      if (!grepl("%[0-9A-Fa-f]{2}", text, perl = TRUE)) {
        return(text)
      }
      suppressWarnings(tryCatch(
        utils::URLdecode(text),
        error = function(...) text
      ))
    },
    character(1)
  ))
}

tempest_contract_normalize_display_text <- function(value) {
  value <- stringi::stri_trans_nfkc(value)
  value <- stringi::stri_replace_all_regex(
    value,
    "\\p{Default_Ignorable_Code_Point}+",
    ""
  )
  stringi::stri_replace_all_regex(value, "\\p{White_Space}+", " ")
}

tempest_contract_sensitive_scalar <- function(value) {
  if (!is.character(value) || length(value) == 0L) {
    return(FALSE)
  }
  value <- value[!is.na(value)]
  if (length(value) == 0L) {
    return(FALSE)
  }
  markdown_unescaped <- tempest_contract_unescape_markdown(value)
  entity_unescaped <- tempest_contract_decode_entities(value)
  rendered <- tryCatch(
    tempest_contract_rendered_markdown(value),
    error = function(...) NULL
  )
  if (is.null(rendered)) {
    return(TRUE)
  }
  variants <- c(
    value,
    markdown_unescaped,
    entity_unescaped,
    rendered,
    tempest_contract_decode_entities(markdown_unescaped),
    tempest_contract_unescape_markdown(entity_unescaped),
    tempest_contract_url_decode(value),
    tempest_contract_url_decode(markdown_unescaped),
    tempest_contract_url_decode(entity_unescaped),
    tempest_contract_url_decode(rendered)
  )
  value <- unique(c(
    variants,
    tempest_contract_normalize_display_text(variants)
  ))
  patterns <- c(
    paste0(
      "(^|[^A-Za-z0-9])(?:sk-[A-Za-z0-9_]{20,}|",
      "sk-(?:proj|svcacct|ant-api[0-9]{2}|or-v1)-",
      "[A-Za-z0-9_-]{16,}|sk[_-](?:live|test)[_-]",
      "[A-Za-z0-9_-]{4,})"
    ),
    paste0(
      "(?i:(?:proxy-)?authorization)[[:space:]]*:[[:space:]]*",
      "[A-Za-z][A-Za-z0-9._-]*[[:space:]]+[^[:space:]]+"
    ),
    "(^|[[:space:]])(?i:bearer)[[:space:]]+[A-Za-z0-9._~+/-]{8,}",
    paste0(
      "[A-Za-z][A-Za-z0-9+.-]*://",
      "[^/@[:space:]]+@"
    ),
    paste0(
      "(?i:(?:set-)?cookie)[[:space:]]*:[[:space:]]*",
      "[^[:space:];=]+=[^[:space:];]+"
    ),
    paste0(
      "(?i:-----BEGIN[[:space:]]+",
      "(?:[A-Z0-9]+[[:space:]]+)*PRIVATE[[:space:]]+KEY-----)"
    ),
    "(^|[^A-Za-z0-9])xox[baprs]-[A-Za-z0-9-]{8,}",
    "(^|[^A-Za-z0-9])gh[pousr]_[A-Za-z0-9]{8,}",
    "(^|[^A-Za-z0-9])github_pat_[A-Za-z0-9_]{20,}",
    "(^|[^A-Za-z0-9])glpat-[A-Za-z0-9_-]{20,}",
    "(^|[^A-Za-z0-9])hf_[A-Za-z0-9]{30,}",
    "(^|[^A-Za-z0-9])npm_[A-Za-z0-9]{36}($|[^A-Za-z0-9])",
    paste0(
      "(^|[^A-Za-z0-9])SG\\.[A-Za-z0-9_-]{22}\\.",
      "[A-Za-z0-9_-]{43}($|[^A-Za-z0-9_-])"
    ),
    "(^|[^A-Za-z0-9])AIza[0-9A-Za-z_-]{35}($|[^0-9A-Za-z_-])",
    "(^|[^A-Za-z0-9])AKIA[0-9A-Z]{12,}",
    "(^|[^A-Za-z0-9])ASIA[0-9A-Z]{16}($|[^0-9A-Z])",
    "(^|[^A-Za-z0-9])SK[0-9A-Fa-f]{32}($|[^0-9A-Fa-f])",
    "(^|[^A-Za-z0-9])eyJ[A-Za-z0-9_-]{4,}\\.[A-Za-z0-9_-]{4,}\\.",
    paste0(
      "(?i:[?&#](?:api[-_]?key|key|access[-_]?token|refresh[-_]?token|",
      "security[-_]?token|token|credential|signature|sig|password|secret|",
      "client[-_]?secret|x[-_]amz[-_](?:security[-_]?token|credential|",
      "signature)|x[-_]goog[-_]signature)=)",
      "[^&#[:space:]]{8,}"
    ),
    paste0(
      "(?i:(api[-_ ]?key|access[-_ ]?token|refresh[-_ ]?token|password|",
      "client[-_ ]?secret|private[-_ ]?key|",
      "(?:aws[-_ ]?)?secret[-_ ]?(?:access[-_ ]?)?key))",
      "[[:space:]]*[:=]",
      "[[:space:]]*[^[:space:]]{4,}"
    )
  )
  any(vapply(
    patterns,
    \(pattern) any(grepl(pattern, value, perl = TRUE)),
    logical(1)
  ))
}

tempest_contract_sensitive_values <- function(value, path) {
  if (is.list(value)) {
    value_names <- names(value)
    child_paths <- if (is.null(value_names)) {
      paste0(path, "[[", seq_along(value), "]]")
    } else {
      paste0(path, "$", value_names)
    }
    return(unlist(
      Map(tempest_contract_sensitive_values, value, child_paths),
      use.names = FALSE
    ))
  }
  if (tempest_contract_sensitive_scalar(value)) path else character()
}

tempest_product_prop_chr <- function(default = NA_character_) {
  S7::new_property(S7::class_character, default = default)
}

tempest_product_prop_list <- function() {
  S7::new_property(S7::class_list, default = list())
}

TempestValidationResult <- S7::new_class(
  "tempest_validation_result",
  properties = list(
    validator_id = tempest_product_prop_chr(),
    status = prop_enum(c("passed", "failed", "warning"), "passed"),
    message = tempest_product_prop_chr(NA_character_),
    details = tempest_product_prop_list(),
    created_at = tempest_product_prop_chr()
  )
)

#' Create a Tempest validation result
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param validator_id Stable validator operation identifier.
#' @param status One of `"passed"`, `"failed"`, or `"warning"`.
#' @param message Optional human-readable result.
#' @param details Serializable diagnostic details.
#' @param created_at Optional creation timestamp.
#' @return A `tempest_validation_result` S7 object.
#' @export
tempest_validation_result <- function(
  validator_id,
  status = c("passed", "failed", "warning"),
  message = NA_character_,
  details = list(),
  created_at = NULL
) {
  validator_id <- tempest_product_scalar(validator_id, "validator_id")
  status <- match.arg(status)
  if (
    !is.character(message) ||
      length(message) != 1L ||
      (!is.na(message) && !nzchar(tempest_trim(message)))
  ) {
    tempest_product_validation_abort(
      "{.arg message} must be a non-empty string or `NA`."
    )
  }
  details <- tempest_product_canonical_list(details, "details")
  created_at <- tempest_product_scalar(
    created_at %||% tempest_now_utc(),
    "created_at"
  )
  TempestValidationResult(
    validator_id = validator_id,
    status = status,
    message = message,
    details = details,
    created_at = created_at
  )
}

tempest_validation_results <- function(results) {
  results <- results %||% list()
  if (
    !is.list(results) ||
      any(
        !vapply(
          results,
          function(result) {
            S7::S7_inherits(result, TempestValidationResult)
          },
          logical(1)
        )
      )
  ) {
    tempest_product_validation_abort(
      "{.arg validation_results} must contain only results from {.fn tempest_validation_result}."
    )
  }
  results
}
