# Shared product report citations, references, and canonical rendering

tempest_product_report_abort <- function(
  message,
  ...,
  class = character(),
  parent = NULL
) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_product_report_error", class),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_product_report_reference <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!rlang::is_string(value) || is.na(value)) {
    tempest_product_report_abort(
      "A product report must be one exact Markdown string."
    )
  }
  list(
    report_id = "report_md",
    sha256 = paste0("sha256:", tempest_product_record_hash(value))
  )
}

tempest_product_report_reference_validate <- function(reference, value) {
  expected <- tempest_product_report_reference(value)
  if (is.null(expected)) {
    if (!is.null(reference)) {
      tempest_product_report_abort(
        "A missing product report cannot carry a report reference."
      )
    }
    return(invisible(NULL))
  }
  if (
    !is.list(reference) ||
      is.data.frame(reference) ||
      !identical(names(reference), names(expected)) ||
      !identical(reference, expected)
  ) {
    tempest_product_report_abort(
      "The product report reference does not match its exact content digest."
    )
  }
  invisible(reference)
}

tempest_stage_execution_review_token <- function(value) {
  gsub(
    "[^A-Za-z0-9_.:@/+~-]",
    "_",
    tempest_product_scalar(value, "execution review token")
  )
}

tempest_stage_records_execution_review_lines <- function(records) {
  records <- tempest_stage_records_validate(records)
  records <- Filter(
    function(record) {
      isTRUE(record@fallback_taken) ||
        !identical(record@status, "succeeded") ||
        (record@execution_path %in%
          c("governed", "grounded") &&
          !identical(record@support_status, "verified"))
    },
    records
  )
  if (length(records) == 0L) {
    return(character())
  }
  vapply(
    records,
    function(record) {
      details <- c(
        paste0(
          "status `",
          tempest_stage_execution_review_token(record@status),
          "`"
        ),
        paste0(
          "path `",
          tempest_stage_execution_review_token(record@execution_path),
          "`"
        ),
        paste0(
          "support `",
          tempest_stage_execution_review_token(record@support_status),
          "`"
        ),
        paste0(
          "publication ",
          if (isTRUE(record@publication_allowed)) "allowed" else "blocked"
        )
      )
      if (isTRUE(record@fallback_taken)) {
        details <- c(
          details,
          paste0(
            "fallback `",
            tempest_stage_execution_review_token(
              record@fallback_implementation
            ),
            "`"
          )
        )
      }
      if (!is.na(record@failure_class)) {
        details <- c(
          details,
          paste0(
            "failure `",
            tempest_stage_execution_review_token(record@failure_class),
            "`"
          )
        )
      }
      paste0(
        "- `",
        tempest_stage_execution_review_token(record@stage),
        "` attempt `",
        tempest_stage_execution_review_token(record@attempt_id),
        "`: ",
        paste(details, collapse = "; "),
        "."
      )
    },
    character(1)
  )
}

tempest_stage_records_execution_review <- function(records) {
  lines <- tempest_stage_records_execution_review_lines(records)
  if (length(lines) == 0L) {
    return("")
  }
  paste(c("## Execution review", "", lines), collapse = "\n")
}

tempest_markdown_without_trusted_title <- function(content, trusted_title) {
  if (is.null(trusted_title)) {
    return(content)
  }
  trusted_title <- tempest_report_title_validate(trusted_title)
  prefix <- paste0(
    "# ",
    tempest_markdown_escape_plain_text(trusted_title, "report title"),
    "\n"
  )
  if (!startsWith(content, prefix)) {
    tempest_product_report_abort(
      "Canonical report content does not begin with its trusted title."
    )
  }
  substr(content, nchar(prefix) + 1L, nchar(content))
}

tempest_markdown_append_execution_review <- function(
  content,
  review,
  trusted_title = NULL
) {
  body <- tempest_markdown_without_trusted_title(content, trusted_title)
  if (tempest_markdown_has_heading(body, "Execution review")) {
    tempest_product_report_abort(
      "Provider-authored content cannot contain the reserved Execution review section."
    )
  }
  if (is.null(review) || identical(review, "")) {
    return(content)
  }
  review <- tempest_product_scalar(review, "context$execution_review")
  separator <- if (endsWith(content, "\n")) "\n" else "\n\n"
  paste0(content, separator, review, "\n")
}

tempest_stage_records_validate_execution_review <- function(
  content,
  records,
  trusted_title = NULL
) {
  if (is.null(content)) {
    return(invisible(content))
  }
  if (!rlang::is_string(content)) {
    tempest_stage_record_abort(
      "A final report must be one Markdown string before review validation."
    )
  }
  review <- tempest_stage_records_execution_review(records)
  body <- tempest_markdown_without_trusted_title(content, trusted_title)
  if (identical(review, "")) {
    if (tempest_markdown_has_heading(body, "Execution review")) {
      tempest_stage_record_abort(
        "A final report cannot contain an unbound execution review."
      )
    }
    return(invisible(content))
  }
  suffix <- paste0("\n\n", review, "\n")
  if (!endsWith(content, suffix)) {
    tempest_stage_record_abort(
      "A final report must end with its exact deterministic execution review."
    )
  }
  prefix_length <- nchar(content) - nchar(suffix)
  prefix <- if (prefix_length > 0L) {
    substr(content, 1L, prefix_length)
  } else {
    ""
  }
  prefix_body <- tempest_markdown_without_trusted_title(prefix, trusted_title)
  if (tempest_markdown_has_heading(prefix_body, "Execution review")) {
    tempest_stage_record_abort(
      "A final report must contain exactly one canonical execution review."
    )
  }
  invisible(content)
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
tempest_markdown_heading_text <- function(text) {
  if (!rlang::is_string(text) || is.na(text)) {
    tempest_product_report_abort(
      "Markdown heading extraction requires one string."
    )
  }
  values <- tryCatch(
    {
      document <- xml2::read_html(
        commonmark::markdown_html(text),
        options = c("RECOVER", "NOERROR", "NOWARNING", "NONET")
      )
      headings <- xml2::xml_find_all(
        document,
        ".//h1 | .//h2 | .//h3 | .//h4 | .//h5 | .//h6"
      )
      vapply(
        headings,
        \(heading) xml2::xml_text(heading, trim = TRUE),
        character(1)
      )
    },
    error = function(error) {
      tempest_product_report_abort(
        "Markdown headings could not be parsed safely.",
        parent = error
      )
    }
  )
  values <- tempest_contract_normalize_display_text(values)
  values <- stringi::stri_trim_both(values)
  values <- values[nzchar(values)]
  unique(values)
}

#' @keywords internal
tempest_markdown_has_heading <- function(text, labels) {
  heading_key <- function(value) {
    value <- tempest_contract_normalize_display_text(value)
    value <- stringi::stri_replace_all_regex(value, "\\p{White_Space}+", "")
    tolower(value)
  }
  headings <- heading_key(tempest_markdown_heading_text(text))
  labels <- heading_key(labels)
  any(headings %in% labels)
}

#' @keywords internal
tempest_normalize_min_support_score <- function(min_support_score) {
  score <- min_support_score %||% 0.7
  if (
    !is.numeric(score) ||
      is.object(score) ||
      !is.null(names(score)) ||
      length(score) != 1L ||
      is.na(score) ||
      !is.finite(score) ||
      score < 0 ||
      score > 1
  ) {
    tempest_abort("min_support_score must be in [0, 1].")
  }
  as.double(score)
}

#' @keywords internal
tempest_apply_min_support_score <- function(
  status,
  score,
  min_support_score = 0.7
) {
  score <- suppressWarnings(as.numeric(score))
  if (length(score) != 1L) {
    return(status)
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  if (identical(status, "supported")) {
    if (is.na(score) || !is.finite(score)) {
      return("unverifiable")
    }
    if (score < min_support_score) {
      return("unsupported")
    }
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
tempest_citation_shaped_tokens <- function(text) {
  matches <- gregexpr("\\[S[^]\\r\\n]*\\]", text, perl = TRUE)
  tokens <- regmatches(text, matches)
  unname(unlist(tokens, use.names = FALSE))
}

#' @keywords internal
tempest_citation_tokens_valid <- function(text) {
  tokens <- tempest_citation_shaped_tokens(text)
  length(tokens) == 0L ||
    all(grepl(
      "^\\[S[0-9a-f]{12}\\]$",
      tokens,
      perl = TRUE
    ))
}

#' @keywords internal
tempest_package_structural_headings <- function() {
  c(
    "Introduction",
    "Overview",
    "Background",
    "Methods",
    "Methodology",
    "Results",
    "Evidence",
    "Discussion",
    "Limitations",
    "Recommendations",
    "Conclusion",
    "Conclusions",
    "Summary"
  )
}

#' @keywords internal
tempest_markdown_structural_heading <- function(lines) {
  headings <- grepl("^[[:space:]]*#{1,6}[[:space:]]", lines)
  heading_text <- sub(
    "^[[:space:]]*#{1,6}[[:space:]]+",
    "",
    lines
  )
  headings &
    tolower(tempest_trim(heading_text)) %in%
      tolower(tempest_package_structural_headings())
}

#' @keywords internal
tempest_report_title_validate <- function(title) {
  if (
    !rlang::is_string(title) ||
      is.na(title) ||
      !nzchar(tempest_trim(title)) ||
      grepl("\r", title, fixed = TRUE) ||
      grepl("\n", title, fixed = TRUE)
  ) {
    tempest_product_report_abort(
      "Report title must be non-empty single-line plain text."
    )
  }
  title
}

#' @keywords internal
tempest_markdown_escape_plain_text <- function(value, field) {
  if (is.null(value) || (rlang::is_string(value) && is.na(value))) {
    return("")
  }
  if (
    !rlang::is_string(value) ||
      grepl("\r", value, fixed = TRUE) ||
      grepl("\n", value, fixed = TRUE)
  ) {
    tempest_product_report_abort(
      "Markdown metadata {.field {field}} must be one single-line string."
    )
  }
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value <- gsub("'", "&#39;", value, fixed = TRUE)
  gsub("([\\\\`*_{}\\[\\]()#+.!|~-])", "\\\\\\1", value, perl = TRUE)
}

#' @keywords internal
tempest_markdown_source_url <- function(value) {
  if (is.null(value) || (rlang::is_string(value) && is.na(value))) {
    return("")
  }
  invalid_bytes <- !rlang::is_string(value) ||
    grepl("[[:cntrl:][:space:]<>]", value, perl = TRUE)
  parsed <- if (invalid_bytes) {
    NULL
  } else {
    tryCatch(curl::curl_parse_url(value), error = function(error) NULL)
  }
  if (
    is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !nzchar(parsed$host %||% "") ||
      !identical(parsed$url, value)
  ) {
    tempest_product_report_abort(
      "Source locator cannot be rendered as one canonical safe HTTP URL."
    )
  }
  paste0("<", value, ">")
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
  matches <- vapply(
    claim_texts,
    function(claim_text) {
      nzchar(claim_text) && identical(claim_text, context_text)
    },
    logical(1)
  )
  matched <- claims[matches]
  matched
}

#' @keywords internal
tempest_citation_context_range <- function(text, start, end) {
  text_len <- nchar(text)
  before <- if (start > 1L) substr(text, 1L, start - 1L) else ""
  previous <- gregexpr("[.!?\\n]", before, perl = TRUE)[[1]]
  left <- if (length(previous) == 1L && previous[[1]] == -1L) {
    1L
  } else {
    max(previous) + 1L
  }
  after <- if (end < text_len) substr(text, end + 1L, text_len) else ""
  next_boundary <- regexpr("[.!?\\n]", after, perl = TRUE)[[1]]
  right <- if (next_boundary == -1L) text_len else end + next_boundary
  c(start = left, end = right)
}

#' @keywords internal
tempest_apply_strict_claim_action <- function(
  text,
  store,
  action,
  min_support_score
) {
  if (!action %in% c("drop", "revise")) {
    return(text)
  }
  matches <- tempest_citation_matches(text)
  ranges <- list()
  for (i in seq_len(nrow(matches))) {
    context <- tempest_citation_context(
      text,
      matches$start[[i]],
      matches$end[[i]]
    )
    status <- tempest_source_status(
      store,
      matches$id[[i]],
      min_support_score = min_support_score,
      context = context
    )
    if (isTRUE(status %in% c("unsupported", "contradicted"))) {
      range <- tempest_citation_context_range(
        text,
        matches$start[[i]],
        matches$end[[i]]
      )
      ranges[[paste(range, collapse = ":")]] <- range
    }
  }
  if (length(ranges) == 0L) {
    return(text)
  }
  ranges <- ranges[order(
    vapply(ranges, `[[`, integer(1), "start"),
    decreasing = TRUE
  )]
  replacement <- if (identical(action, "revise")) {
    " [Claim withheld pending revision.]"
  } else {
    ""
  }
  for (range in ranges) {
    prefix <- if (range[["start"]] > 1L) {
      substr(text, 1L, range[["start"]] - 1L)
    } else {
      ""
    }
    suffix <- if (range[["end"]] < nchar(text)) {
      substr(text, range[["end"]] + 1L, nchar(text))
    } else {
      ""
    }
    text <- paste0(prefix, replacement, suffix)
  }
  text
}

#' @keywords internal
tempest_source_status <- function(
  store,
  source_id,
  min_support_score = 0.7,
  context = NULL
) {
  claims <- store$proposed_claims_for_resource(source_id)
  if (length(claims) == 0) {
    return(NA_character_)
  }
  claims <- tempest_claims_for_citation_context(claims, context)
  statuses <- vapply(
    claims,
    function(c) {
      supports <- tempest_stage_claim_support_records(
        store,
        c,
        required = FALSE
      )
      summary <- if (is.null(supports)) {
        list(
          status = c@verification_status,
          score = c@support_score
        )
      } else {
        tempest_claim_support_aggregate(supports)
      }
      tempest_apply_min_support_score(
        summary$status,
        summary$score,
        min_support_score = min_support_score
      )
    },
    character(1)
  )
  # Worst-case wins within the matched citation context so weak evidence is not
  # masked by strong evidence for the same sentence.
  worst_first <- c(
    "contradicted",
    "unsupported",
    "unverifiable",
    "partially_supported",
    "supported",
    "unverified"
  )
  statuses[order(match(statuses, worst_first))][1]
}

#' @keywords internal
tempest_strict_publication_claims <- function(
  text,
  store,
  min_support_score = 0.7
) {
  if (!tempest_citation_tokens_valid(text)) {
    tempest_product_report_abort(
      "Strict publication contains a malformed source-citation token."
    )
  }
  assertions <- tempest_strict_publication_assertions(text)
  if (length(assertions) == 0L) {
    if (nzchar(tempest_trim(text))) {
      tempest_product_report_abort(
        "Strict publication found no publishable cited assertions."
      )
    }
    return(invisible(character()))
  }
  if (length(store$list_claim_supports()) == 0L) {
    tempest_product_report_abort(
      "Strict publication requires completed claim-by-span verification."
    )
  }

  required_claim_ids <- character()
  all_claims <- store$list_proposed_claims()
  for (assertion in assertions) {
    matches <- tempest_citation_matches(assertion)
    if (nrow(matches) == 0L) {
      tempest_product_report_abort(
        "Every strict-publication assertion must carry a bound citation."
      )
    }
    source_ids <- unique(matches$id)
    for (source_id in source_ids) {
      if (is.null(store$get_retrieved_source(source_id))) {
        tempest_product_report_abort(
          "Strict publication cites unknown source id {.val {source_id}}."
        )
      }
    }
    assertion_text <- tempest_normalize_claim_match_text(assertion)
    candidates <- Filter(
      function(claim) {
        identical(
          tempest_normalize_claim_match_text(claim@claim_text),
          assertion_text
        ) &&
          setequal(source_ids, claim@source_ids)
      },
      all_claims
    )
    if (length(candidates) != 1L) {
      tempest_product_report_abort(
        paste0(
          "Every strict-publication assertion must bind unambiguously to ",
          "one exact proposed claim and only its cited sources."
        )
      )
    }
    required_claim_ids <- c(
      required_claim_ids,
      candidates[[1]]@claim_id
    )
  }

  required_claim_ids <- unique(required_claim_ids)
  for (claim_id in required_claim_ids) {
    claim <- store$get_proposed_claim(claim_id)
    if (
      is.null(claim) ||
        identical(claim@verification_status, "unverified")
    ) {
      tempest_product_report_abort(
        "Strict publication requires completed verification for claim {.val {claim_id}}."
      )
    }
    if (!tempest_stage_claim_has_captured_evidence(claim, store)) {
      tempest_product_report_abort(
        paste0(
          "Strict publication claim {.val {claim_id}} has no exact captured ",
          "source evidence."
        )
      )
    }
    if (
      identical(claim@verification_status, "supported") &&
        (is.na(claim@support_score) || !is.finite(claim@support_score))
    ) {
      tempest_product_report_abort(
        paste0(
          "Strict publication cannot treat claim {.val {claim_id}} as ",
          "supported without a finite support score."
        )
      )
    }
    supports <- tryCatch(
      tempest_stage_claim_support_records(store, claim, required = TRUE),
      tempest_stage_error = function(error) {
        tempest_product_report_abort(
          paste0(
            "Strict publication claim {.val {claim_id}} requires its exact ",
            "complete claim-by-span support set."
          ),
          parent = error
        )
      }
    )
    summary <- tempest_claim_support_aggregate(supports)
    score_matches <- isTRUE(all.equal(
      summary$score,
      claim@support_score,
      check.attributes = FALSE
    ))
    if (
      !identical(summary$status, claim@verification_status) ||
        !score_matches
    ) {
      tempest_product_report_abort(
        paste0(
          "Strict publication claim {.val {claim_id}} disagrees with its ",
          "exact claim-by-span support set."
        )
      )
    }
    threshold_status <- tempest_apply_min_support_score(
      summary$status,
      summary$score,
      min_support_score = min_support_score
    )
    claim_threshold_status <- tempest_apply_min_support_score(
      claim@verification_status,
      claim@support_score,
      min_support_score = min_support_score
    )
    if (!identical(threshold_status, claim_threshold_status)) {
      tempest_product_report_abort(
        paste0(
          "Strict publication claim {.val {claim_id}} violates the exact ",
          "support-threshold semantics."
        )
      )
    }
  }

  invisible(required_claim_ids)
}

#' @keywords internal
tempest_strict_publication_assertions <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(tempest_trim(lines))]
  structural <- tempest_markdown_structural_heading(lines)
  references <- grepl(
    "^[[:space:]]*\\[\\^S[0-9a-f]{12}\\]:",
    lines
  )
  lines <- lines[!references & !structural]
  if (length(lines) == 0L) {
    return(character())
  }
  assertions <- unlist(
    lapply(lines, \(line) strsplit(line, "(?<=[.!?])\\s+", perl = TRUE)[[1]]),
    use.names = FALSE
  )
  assertions <- assertions[
    nzchar(tempest_normalize_claim_match_text(assertions))
  ]
  unname(assertions)
}

#' @keywords internal
tempest_report_body_validate_reserved_sections <- function(body) {
  if (!rlang::is_string(body) || is.na(body)) {
    tempest_product_report_abort("Report body must be one Markdown string.")
  }
  reserved <- tempest_markdown_has_heading(
    body,
    c("References", "Execution review")
  )
  if (reserved) {
    tempest_product_report_abort(
      "Report body cannot supply package-reserved Markdown sections."
    )
  }
  supplied_source_footnote <- grepl(
    "(^|\\n)[[:space:]]*\\[\\^[sS][^]\\r\\n]*\\][[:space:]]*:",
    body,
    perl = TRUE
  )
  if (supplied_source_footnote) {
    tempest_product_report_abort(
      "Report body cannot supply package-reserved source footnotes."
    )
  }
  invisible(body)
}

#' @keywords internal
tempest_status_badge <- function(status) {
  if (length(status) != 1 || is.na(status)) {
    return("")
  }
  switch(
    status,
    supported = " \u2713",
    partially_supported = " \u26a0",
    unsupported = " \u2717 unsupported",
    contradicted = " \u2717 contradicted",
    unverifiable = " ?",
    ""
  )
}

#' @keywords internal
tempest_add_footnotes <- function(
  text,
  store,
  citation_policy = "source_attributed",
  on_unsupported_claim = "flag",
  min_support_score = 0.7
) {
  if (!inherits(store, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg store} must be a ResearchWorkspace."
    )
  }
  if (identical(citation_policy, "none")) {
    return(list(text = text, footnotes = ""))
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  if (identical(citation_policy, "strict")) {
    tempest_strict_publication_claims(
      text,
      store,
      min_support_score = min_support_score
    )
    text <- tempest_apply_strict_claim_action(
      text,
      store,
      action = on_unsupported_claim,
      min_support_score = min_support_score
    )
  }
  matches <- tempest_citation_matches(text)
  if (nrow(matches) == 0) {
    return(list(text = text, footnotes = ""))
  }
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
        if (identical(on_unsupported_claim, "flag")) {
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
    src <- store$get_retrieved_source(id)
    if (is.null(src)) {
      return(glue::glue("[^{id}]: (missing source metadata)"))
    }
    title <- tempest_markdown_escape_plain_text(
      src$title %||% "",
      "source title"
    )
    url <- tempest_markdown_source_url(src$url %||% "")
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

#' @keywords internal
tempest_report_md_render <- function(
  title,
  body,
  workspace,
  citation_policy = "source_attributed",
  on_unsupported_claim = "flag",
  min_support_score = 0.7,
  include_references = TRUE
) {
  if (inherits(workspace, "TempestRetriever")) {
    workspace <- workspace$workspace
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace or TempestRetriever."
    )
  }
  include_references <- tempest_product_flag(
    include_references,
    "include_references"
  )
  if (
    !rlang::is_string(citation_policy) ||
      !citation_policy %in%
        c("none", "source_attributed", "claim_verified", "strict")
  ) {
    tempest_product_report_abort(
      "Citation policy is outside the closed report-rendering contract."
    )
  }
  if (
    !rlang::is_string(on_unsupported_claim) ||
      !on_unsupported_claim %in%
        c("flag", "drop", "revise", "keep_with_warning")
  ) {
    tempest_product_report_abort(
      "Unsupported-claim handling is outside the closed report contract."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  title <- tempest_report_title_validate(title)
  rendered_title <- tempest_markdown_escape_plain_text(title, "report title")
  tempest_report_body_validate_reserved_sections(body)
  if (
    !isTRUE(include_references) &&
      citation_policy %in% c("claim_verified", "strict")
  ) {
    required_claim_ids <- tempest_strict_publication_claims(
      body,
      workspace,
      min_support_score = min_support_score
    )
    fully_supported <- vapply(
      required_claim_ids,
      function(claim_id) {
        claim <- workspace$get_proposed_claim(claim_id)
        !is.null(claim) &&
          tempest_stage_claim_verified(
            claim,
            workspace,
            min_support_score
          )
      },
      logical(1)
    )
    if (any(!fully_supported)) {
      tempest_product_report_abort(
        paste0(
          "Verified citation policies can omit References only when every ",
          "cited assertion binds to fully supported evidence."
        )
      )
    }
  }

  res <- tempest_add_footnotes(
    body,
    workspace,
    citation_policy = citation_policy,
    on_unsupported_claim = on_unsupported_claim,
    min_support_score = min_support_score
  )
  rendered_text <- if (isTRUE(include_references)) {
    res$text
  } else {
    gsub(
      "\\[\\^(S[0-9a-f]{12})\\]",
      "[\\1]",
      res$text,
      perl = TRUE
    )
  }
  if (isTRUE(include_references) && nzchar(res$footnotes)) {
    return(paste0(
      "# ",
      rendered_title,
      "\n\n",
      rendered_text,
      "\n\n",
      "## References\n\n",
      res$footnotes,
      "\n"
    ))
  }
  paste0("# ", rendered_title, "\n\n", rendered_text, "\n")
}

#' Read the committed Markdown report from a Tempest product
#'
#' `tempest_report()` returns the exact authoritative Markdown already
#' committed by a completed [tempest_run()] product or a finalized
#' [tempest_session()]. It never generates, repairs, or republishes a report,
#' and it fails when the product is not published.
#'
#' @param x A completed [tempest_run()] product or a finalized
#'   `TempestSession`.
#' @return The exact committed Markdown report.
#' @examples
#' \dontrun{
#' result <- tempest_run("History of jazz", config = tempest_config())
#' tempest_report(result)
#'
#' session <- tempest_session("History of jazz", config = tempest_config())
#' session$step("Tell me about bebop.")
#' session$publish()
#' tempest_report(session)
#' }
#' @export
tempest_report <- function(x) {
  if (inherits(x, "TempestSession")) {
    return(tempest_session_report_read(x))
  }
  if (!tempest_is_product_result(x)) {
    tempest_product_report_abort(
      paste0(
        "{.arg x} must be a completed {.fn tempest_run} product or a ",
        "finalized {.cls TempestSession}."
      ),
      class = "tempest_input_error"
    )
  }
  if (!identical(x@status, "succeeded")) {
    tempest_product_report_abort(
      "The canonical product report is unavailable before publication."
    )
  }
  report_md <- x@report_md
  if (
    !rlang::is_string(report_md) ||
      is.na(report_md) ||
      !nzchar(report_md)
  ) {
    tempest_product_report_abort(
      "The canonical product report artifact has no Markdown content."
    )
  }
  reference <- x@manifest@deliverables$report_md %||% NULL
  tempest_product_report_reference_validate(
    reference[c("report_id", "sha256")],
    report_md
  )
  report_md
}

#' @keywords internal
tempest_final_report_validate <- function(
  report_md,
  workspace,
  title,
  citation_policy,
  on_unsupported_claim,
  min_support_score,
  stage_records = list()
) {
  if (!rlang::is_string(report_md) || is.na(report_md)) {
    tempest_product_report_abort("Final report must be one Markdown string.")
  }
  title <- tempest_report_title_validate(title)
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "{.arg workspace} must be a ResearchWorkspace."
    )
  }
  stage_records <- tempest_stage_records_validate(stage_records)
  tempest_stage_records_validate_execution_review(
    report_md,
    stage_records,
    trusted_title = title
  )
  review <- tempest_stage_records_execution_review(stage_records)
  report_without_review <- report_md
  if (nzchar(review)) {
    # Canonical report renderings already end in one newline. Remove only the
    # separator added before the review so the renderer-owned newline remains
    # part of the report being revalidated.
    suffix <- paste0("\n", review, "\n")
    report_without_review <- substr(
      report_md,
      1L,
      nchar(report_md) - nchar(suffix)
    )
  }

  prefix <- paste0(
    "# ",
    tempest_markdown_escape_plain_text(title, "report title"),
    "\n\n"
  )
  if (!startsWith(report_without_review, prefix)) {
    tempest_product_report_abort(
      "Final report title does not match its authoritative product title."
    )
  }
  rendered_body <- substr(
    report_without_review,
    nchar(prefix) + 1L,
    nchar(report_without_review)
  )
  references_marker <- "\n\n## References\n\n"
  reference_positions <- gregexpr(
    references_marker,
    rendered_body,
    fixed = TRUE
  )[[1]]
  reference_positions <- reference_positions[reference_positions != -1L]
  without_references <- if (endsWith(rendered_body, "\n")) {
    substr(rendered_body, 1L, nchar(rendered_body) - 1L)
  } else {
    rendered_body
  }
  candidates <- list(
    list(
      body = without_references,
      include_references = FALSE
    ),
    list(
      body = without_references,
      include_references = TRUE
    )
  )
  if (length(reference_positions) > 0L) {
    candidates <- c(
      candidates,
      lapply(reference_positions, function(position) {
        list(
          body = if (position > 1L) {
            substr(rendered_body, 1L, position - 1L)
          } else {
            ""
          },
          include_references = TRUE
        )
      })
    )
  }
  canonical <- vapply(
    candidates,
    function(candidate) {
      inline_body <- gsub(
        paste0(
          "\\[\\^(S[0-9a-f]{12})\\]",
          paste0(
            paste0(
              "(?: \u2713| \u26a0| \u2717 unsupported| ",
              "\u2717 contradicted| \\?)?"
            ),
            "(?: \\[unsupported citation\\])?"
          )
        ),
        "[\\1]",
        candidate$body,
        perl = TRUE
      )
      expected <- tryCatch(
        tempest_report_md_render(
          title = title,
          body = inline_body,
          workspace = workspace,
          citation_policy = citation_policy,
          on_unsupported_claim = on_unsupported_claim,
          min_support_score = min_support_score,
          include_references = candidate$include_references
        ),
        error = function(...) NULL
      )
      identical(report_without_review, expected)
    },
    logical(1)
  )
  if (!any(canonical)) {
    tempest_product_report_abort(
      "Final report is not the exact canonical rendering of its evidence."
    )
  }
  invisible(report_md)
}


#' @keywords internal
tempest_product_report_execution_review_candidates <- function(...) {
  histories <- list(...)
  candidates <- unlist(
    lapply(histories, function(records) {
      records <- tempest_stage_records_validate(records)
      vapply(
        seq.int(0L, length(records)),
        function(size) {
          tempest_stage_records_execution_review(records[seq_len(size)])
        },
        character(1)
      )
    }),
    use.names = FALSE
  )
  candidates <- unique(candidates[nzchar(candidates)])
  candidates[order(nchar(candidates), decreasing = TRUE)]
}

#' @keywords internal
tempest_product_report_without_execution_review <- function(
  value,
  records,
  prior_records = records,
  trusted_title = NULL
) {
  if (is.null(value)) {
    return(NULL)
  }
  value <- enc2utf8(value)
  body <- tempest_markdown_without_trusted_title(value, trusted_title)
  if (!tempest_markdown_has_heading(body, "Execution review")) {
    return(value)
  }
  candidates <- tempest_product_report_execution_review_candidates(
    prior_records,
    records
  )
  matches <- candidates[vapply(
    candidates,
    \(review) endsWith(value, paste0("\n\n", review, "\n")),
    logical(1)
  )]
  if (length(matches) == 0L) {
    tempest_stage_record_abort(
      paste0(
        "A durable report can remove only an exact package-owned terminal ",
        "Execution review."
      )
    )
  }
  suffix <- paste0("\n", matches[[1]], "\n")
  prefix_length <- nchar(value) - nchar(suffix)
  if (prefix_length == 0L) {
    return("")
  }
  substr(value, 1L, prefix_length)
}

#' @keywords internal
tempest_product_report_for_stage_records <- function(
  value,
  records,
  prior_records = records,
  trusted_title = NULL
) {
  if (is.null(value)) {
    return(NULL)
  }
  base <- tempest_product_report_without_execution_review(
    value,
    records,
    prior_records = prior_records,
    trusted_title = trusted_title
  )
  review <- tempest_stage_records_execution_review(records)
  tempest_markdown_append_execution_review(
    base,
    review,
    trusted_title = trusted_title
  )
}

#' @keywords internal
tempest_product_report_inline_citations <- function(value) {
  gsub(
    "\\[\\^(S[0-9a-f]{12})\\]",
    "[\\1]",
    value,
    perl = TRUE
  )
}

#' @keywords internal
tempest_product_report_validate_policy <- function(
  report_md,
  title,
  workspace,
  config,
  records
) {
  if (is.null(report_md)) {
    return(invisible(NULL))
  }
  tempest_final_report_validate(
    report_md = report_md,
    workspace = workspace,
    title = title,
    citation_policy = config@citation_policy,
    on_unsupported_claim = config@on_unsupported_claim,
    min_support_score = config@min_support_score,
    stage_records = records
  )
}
