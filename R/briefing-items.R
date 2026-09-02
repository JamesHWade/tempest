# Governed report synthesis records.

tempest_briefing_item_kinds <- function() {
  c("observation", "assessment", "review_action", "no_change")
}

tempest_briefing_item_confidences <- function() {
  c("low", "medium", "high")
}

# A briefing decides structurally whether a verified claim changes accepted
# knowledge: it compares the claim text with the Claim records that were
# pinned from the Graft snapshot at the start of the run. A claim that restates
# an accepted Claim is a duplicate and may only support a no-change item; any
# other verified claim is new and reads as an observation of what changed.
tempest_briefing_claim_dispositions <- function() {
  c("new", "duplicate")
}

tempest_workspace_accepted_claim_keys <- function(workspace) {
  if (!inherits(workspace, "ResearchWorkspace")) {
    return(character())
  }
  resources <- workspace$list_retrieved_resources()
  keys <- character()
  for (resource in resources) {
    if (!identical(resource@resource_kind, "graft.record")) {
      next
    }
    metadata <- resource@metadata
    if (!identical(metadata$graft_record_class, "Claim")) {
      next
    }
    text <- metadata$graft_statement_text
    if (!rlang::is_string(text)) {
      text <- tempest_knowledge_content_statement_text(resource@content)
    }
    if (rlang::is_string(text) && nzchar(tempest_trim(text))) {
      keys <- c(keys, tempest_claim_text_key(text))
    }
  }
  unique(keys)
}

tempest_knowledge_content_statement_text <- function(content) {
  if (!rlang::is_string(content)) {
    return(NULL)
  }
  lines <- strsplit(content, "\n", fixed = TRUE)[[1L]]
  line <- lines[startsWith(lines, "statement_text: ")]
  if (length(line) != 1L) {
    return(NULL)
  }
  sub("^statement_text: ", "", line)
}

# `accepted` is the vector from tempest_workspace_accepted_claim_keys(),
# computed once per briefing rather than once per item.
tempest_briefing_claim_disposition <- function(claim_text, accepted) {
  if (
    !rlang::is_string(claim_text) ||
      is.na(claim_text) ||
      !nzchar(tempest_trim(claim_text))
  ) {
    return("new")
  }
  if (tempest_claim_text_key(claim_text) %in% accepted) {
    "duplicate"
  } else {
    "new"
  }
}

tempest_briefing_item_disposition_valid <- function(
  kind,
  claim_text,
  accepted
) {
  if (!kind %in% c("observation", "no_change")) {
    return(TRUE)
  }
  disposition <- tempest_briefing_claim_disposition(claim_text, accepted)
  identical(
    disposition,
    if (identical(kind, "no_change")) "duplicate" else "new"
  )
}

tempest_briefing_item_confidence_prop <- function() {
  S7::new_property(
    S7::class_character,
    default = NA_character_,
    validator = function(value) {
      valid <- length(value) == 1L &&
        (is.na(value) || value %in% tempest_briefing_item_confidences())
      if (!valid) {
        paste0(
          "must be NA or one of: ",
          paste(tempest_briefing_item_confidences(), collapse = ", ")
        )
      }
    }
  )
}

tempest_briefing_item_content <- function(
  kind,
  text,
  claim_ids,
  confidence
) {
  list(
    kind = kind,
    text = text,
    claim_ids = unname(as.list(claim_ids)),
    confidence = if (is.na(confidence)) NULL else confidence
  )
}

tempest_briefing_item_id <- function(kind, text, claim_ids, confidence) {
  paste0(
    "B",
    substr(
      tempest_product_record_hash(tempest_briefing_item_content(
        kind,
        text,
        claim_ids,
        confidence
      )),
      1L,
      16L
    )
  )
}

#' Governed briefing item (S7)
#' @keywords internal
TempestBriefingItem <- S7::new_class(
  "TempestBriefingItem",
  package = "tempest",
  properties = list(
    item_id = prop_chr(),
    kind = prop_enum(tempest_briefing_item_kinds()),
    text = prop_chr(),
    claim_ids = prop_chr_vec(),
    confidence = tempest_briefing_item_confidence_prop()
  ),
  constructor = function(
    kind,
    text,
    claim_ids,
    confidence = NA_character_,
    item_id = NULL
  ) {
    text <- tempest_trim(text)
    claim_ids <- sort(unique(claim_ids))
    derived_id <- tempest_briefing_item_id(
      kind,
      text,
      claim_ids,
      confidence
    )
    S7::new_object(
      S7::S7_object(),
      item_id = item_id %||% derived_id,
      kind = kind,
      text = text,
      claim_ids = claim_ids,
      confidence = confidence
    )
  },
  validator = function(self) {
    if (!rlang::is_string(self@text) || is.na(self@text)) {
      return("text must be one non-missing string")
    }
    if (!nzchar(self@text)) {
      return("text must not be empty")
    }
    if (!isTRUE(validUTF8(self@text))) {
      return("text must be valid UTF-8")
    }
    if (nchar(self@text, type = "bytes") > 600L) {
      return("text must be at most 600 bytes")
    }
    if (
      grepl("\r", self@text, fixed = TRUE) ||
        grepl("\n", self@text, fixed = TRUE)
    ) {
      return("text must be one bounded, non-empty UTF-8 line")
    }
    if (
      grepl("\\[S[0-9a-f]{12}\\]", self@text, perl = TRUE) ||
        grepl("<!--|-->", self@text, perl = TRUE)
    ) {
      return("text cannot contain source citations or HTML comments")
    }
    if (
      length(self@claim_ids) == 0L ||
        !tempest_ledger_identifier_vector_valid(self@claim_ids) ||
        !identical(self@claim_ids, sort(self@claim_ids))
    ) {
      return("claim_ids must be a non-empty canonical identifier set")
    }
    expected_id <- tempest_briefing_item_id(
      self@kind,
      self@text,
      self@claim_ids,
      self@confidence
    )
    if (!identical(self@item_id, expected_id)) {
      return("item_id must match the exact briefing-item content")
    }
    if (self@kind %in% c("assessment", "no_change") && is.na(self@confidence)) {
      return("assessments and no-change items require confidence")
    }
    if (
      self@kind %in%
        c("observation", "review_action") &&
        !is.na(self@confidence)
    ) {
      return("observations and review actions cannot claim confidence")
    }
    NULL
  }
)

tempest_briefing_item_record_fields <- function() {
  c("item_id", "kind", "text", "claim_ids", "confidence")
}

tempest_briefing_item_to_list <- function(item) {
  stopifnot(S7::S7_inherits(item, TempestBriefingItem))
  S7::validate(item)
  list(
    item_id = item@item_id,
    kind = item@kind,
    text = item@text,
    claim_ids = unname(as.list(item@claim_ids)),
    confidence = if (is.na(item@confidence)) NULL else item@confidence
  )
}

tempest_briefing_item_claim_ids <- function(value) {
  if (is.character(value) && !is.object(value) && is.null(names(value))) {
    return(unname(value))
  }
  if (
    is.list(value) &&
      !is.data.frame(value) &&
      is.null(names(value)) &&
      all(vapply(value, rlang::is_string, logical(1)))
  ) {
    return(unname(as.character(unlist(value, use.names = FALSE))))
  }
  tempest_stage_output_abort(
    "Briefing-item claim_ids must be a plain string array."
  )
}

tempest_briefing_item_output_record <- function(value) {
  if (!is.list(value) || is.data.frame(value) || is.null(names(value))) {
    tempest_stage_output_abort("Each briefing item must be one named record.")
  }
  required <- c("kind", "text", "claim_ids")
  allowed <- c(required, "confidence")
  if (
    anyNA(names(value)) ||
      anyDuplicated(names(value)) ||
      !all(required %in% names(value)) ||
      any(!names(value) %in% allowed)
  ) {
    tempest_stage_output_abort(
      paste0(
        "Each briefing item must contain kind, text, claim_ids, and only ",
        "the optional confidence field."
      )
    )
  }
  value <- value[allowed[allowed %in% names(value)]]
  value$kind <- tempest_stage_scalar_character(value$kind, "kind")
  value$text <- tempest_stage_scalar_character(value$text, "text")
  value$claim_ids <- tempest_briefing_item_claim_ids(value$claim_ids)
  value$confidence <- value$confidence %||% NA_character_
  if (
    !(rlang::is_string(value$confidence) ||
      identical(value$confidence, NA_character_))
  ) {
    tempest_stage_output_abort(
      "Briefing-item confidence must be one string when supplied."
    )
  }
  if (value$kind %in% c("observation", "review_action")) {
    value$confidence <- NA_character_
  }
  value
}

tempest_briefing_item_authoritative_claims <- function(
  claim_ids,
  evidence,
  workspace,
  min_support_score
) {
  evidence_ids <- vapply(evidence, \(claim) claim@claim_id, character(1))
  if (any(!claim_ids %in% evidence_ids)) {
    tempest_stage_governance_abort(
      "Every briefing item must bind only claims supplied to this stage."
    )
  }
  claims <- lapply(claim_ids, workspace$get_proposed_claim)
  if (any(vapply(claims, is.null, logical(1)))) {
    tempest_stage_governance_abort(
      "A briefing item references a claim missing from the workspace."
    )
  }
  verified <- vapply(
    claims,
    tempest_stage_claim_verified,
    logical(1),
    workspace = workspace,
    min_support_score = min_support_score
  )
  if (!all(verified)) {
    tempest_stage_governance_abort(
      paste0(
        "Every briefing item must bind only threshold-verified claims with ",
        "exact support records."
      )
    )
  }
  claims
}

tempest_briefing_item_synthesis_text <- function(kind, claims) {
  claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
  claims <- claims[order(claim_ids)]
  claim_text <- paste(
    vapply(claims, \(claim) claim@claim_text, character(1)),
    collapse = " | "
  )
  switch(
    kind,
    assessment = paste0(
      "Assess the decision implications of: ",
      claim_text
    ),
    review_action = paste0("Review before deciding: ", claim_text),
    NULL
  )
}

tempest_briefing_item_validate_synthesis <- function(kind, text, claims) {
  if (!kind %in% c("assessment", "review_action")) {
    return(invisible(NULL))
  }
  expected <- tempest_briefing_item_synthesis_text(kind, claims)
  if (!identical(text, expected)) {
    tempest_stage_output_abort(
      paste0(
        "An assessment or review-action item must use its exact closed ",
        "prompt and copy only its bound threshold-verified claims."
      )
    )
  }
  invisible(NULL)
}

tempest_briefing_items_validate_counts <- function(
  items,
  limits = c(
    observation = 3L,
    assessment = 2L,
    review_action = 2L,
    no_change = 1L
  )
) {
  kinds <- vapply(items, \(item) item@kind, character(1))
  counts <- table(factor(kinds, levels = names(limits)))
  if (counts[["observation"]] == 0L && counts[["no_change"]] == 0L) {
    tempest_stage_output_abort(
      paste0(
        "Grounded writing requires at least one verified observation or ",
        "one verified no-change finding."
      )
    )
  }
  if (any(counts > limits)) {
    excess <- names(limits)[counts > limits]
    constraints <- paste0(excess, " <= ", limits[excess])
    tempest_stage_output_abort(
      paste0(
        "Grounded writing exceeds its item limits: ",
        paste(constraints, collapse = ", "),
        "."
      )
    )
  }
  ids <- vapply(items, \(item) item@item_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_stage_output_abort("Grounded writing cannot repeat an item.")
  }
  order_index <- match(kinds, names(limits))
  items[order(order_index, seq_along(items))]
}

tempest_briefing_items_from_output <- function(output, context) {
  output <- tempest_stage_exact_record(output, "output", "items")
  values <- output$items
  if (
    !is.list(values) ||
      is.data.frame(values) ||
      !is.null(names(values)) ||
      length(values) == 0L ||
      length(values) > 8L
  ) {
    tempest_stage_output_abort(
      "Grounded writing must return an unnamed array of one to eight items."
    )
  }
  evidence <- tempest_stage_evidence(context)
  workspace <- tempest_stage_workspace(context)
  min_support_score <- tempest_normalize_min_support_score(
    context$min_support_score %||% 0.7
  )
  accepted <- tempest_workspace_accepted_claim_keys(workspace)
  items <- lapply(values, function(value) {
    value <- tempest_briefing_item_output_record(value)
    if (
      !value$kind %in% tempest_briefing_item_kinds() ||
        !value$confidence %in%
          c(
            tempest_briefing_item_confidences(),
            NA_character_
          )
    ) {
      tempest_stage_output_abort(
        "A briefing item uses an unsupported kind or confidence."
      )
    }
    if (
      length(value$claim_ids) == 0L ||
        anyDuplicated(value$claim_ids) ||
        !tempest_ledger_identifier_vector_valid(value$claim_ids)
    ) {
      tempest_stage_output_abort(
        "A briefing item requires unique, bounded claim_ids."
      )
    }
    claims <- tempest_briefing_item_authoritative_claims(
      value$claim_ids,
      evidence,
      workspace,
      min_support_score
    )
    if (
      value$kind %in%
        c("observation", "no_change") &&
        (length(claims) != 1L || !identical(value$text, claims[[1]]@claim_text))
    ) {
      tempest_stage_output_abort(
        paste0(
          "An observation or no-change item must copy one threshold-verified ",
          "claim exactly and bind only that claim."
        )
      )
    }
    tempest_briefing_item_validate_synthesis(
      value$kind,
      value$text,
      claims
    )
    if (
      !tempest_briefing_item_disposition_valid(
        value$kind,
        claims[[1]]@claim_text,
        accepted
      )
    ) {
      tempest_stage_output_abort(
        paste0(
          "A no-change item must copy a verified claim that restates a Claim ",
          "accepted in the pinned Graft snapshot, and an observation must copy ",
          "a verified claim that is not yet accepted."
        )
      )
    }
    item <- tryCatch(
      TempestBriefingItem(
        kind = value$kind,
        text = value$text,
        claim_ids = value$claim_ids,
        confidence = value$confidence
      ),
      error = function(error) {
        tempest_stage_output_abort(
          "A briefing item violates the closed synthesis contract.",
          parent = error
        )
      }
    )
    item
  })
  tempest_briefing_items_validate_counts(items)
}

tempest_briefing_hex_encode <- function(value) {
  paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(value)))), collapse = "")
}

tempest_briefing_hex_decode <- function(value) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !nzchar(value) ||
      nchar(value) %% 2L != 0L ||
      !grepl("^[0-9a-f]+$", value)
  ) {
    tempest_product_report_abort("Briefing-item provenance is malformed.")
  }
  starts <- seq.int(1L, nchar(value), by = 2L)
  bytes <- as.raw(vapply(
    starts,
    \(start) strtoi(substr(value, start, start + 1L), base = 16L),
    integer(1)
  ))
  rawToChar(bytes)
}

tempest_briefing_item_provenance <- function(item) {
  json <- jsonlite::toJSON(
    tempest_briefing_item_to_list(item),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  paste0(
    "<!-- tempest-briefing-item:",
    tempest_briefing_hex_encode(json),
    " -->"
  )
}

tempest_briefing_item_evidence_text <- function(item) {
  ids <- paste0("`", item@claim_ids, "`", collapse = ", ")
  confidence <- if (is.na(item@confidence)) {
    ""
  } else {
    paste0("; confidence: ", item@confidence)
  }
  paste0("_(Evidence: ", ids, confidence, ".)_")
}

tempest_briefing_source_citations <- function(source_ids) {
  paste0("[", source_ids, "]", collapse = " ")
}

tempest_briefing_item_markdown <- function(item, workspace) {
  claims <- lapply(item@claim_ids, workspace$get_proposed_claim)
  if (any(vapply(claims, is.null, logical(1)))) {
    tempest_product_report_abort(
      "Briefing-item rendering requires its exact workspace claims."
    )
  }
  body <- switch(
    item@kind,
    observation = paste0(
      "- ",
      item@text,
      " ",
      tempest_briefing_source_citations(claims[[1]]@source_ids)
    ),
    assessment = paste0(
      "- **Assessment:** ",
      item@text,
      " ",
      tempest_briefing_item_evidence_text(item)
    ),
    review_action = paste0(
      "- **Review:** ",
      item@text,
      " ",
      tempest_briefing_item_evidence_text(item)
    ),
    no_change = paste0(
      "- **No material change:** ",
      item@text,
      " ",
      tempest_briefing_source_citations(claims[[1]]@source_ids),
      " ",
      tempest_briefing_item_evidence_text(item)
    )
  )
  paste(body, tempest_briefing_item_provenance(item))
}

tempest_briefing_items_markdown <- function(
  items,
  workspace,
  lead = FALSE
) {
  headings <- c(
    observation = "What changed",
    assessment = "Why it matters",
    review_action = "Review today",
    no_change = "No material change"
  )
  groups <- split(
    items,
    factor(
      vapply(items, \(item) item@kind, character(1)),
      levels = names(headings)
    ),
    drop = TRUE
  )
  parts <- unlist(
    lapply(names(groups), function(kind) {
      c(
        paste0("### ", headings[[kind]]),
        "",
        vapply(
          groups[[kind]],
          tempest_briefing_item_markdown,
          character(1),
          workspace = workspace
        )
      )
    }),
    use.names = FALSE
  )
  if (isTRUE(lead)) {
    parts <- c("## At a glance", "", parts)
  }
  paste(parts, collapse = "\n")
}

tempest_briefing_item_from_list <- function(value) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      !identical(names(value), tempest_briefing_item_record_fields())
  ) {
    tempest_product_report_abort(
      "Briefing-item provenance has unsupported fields."
    )
  }
  claim_ids <- tryCatch(
    tempest_briefing_item_claim_ids(value$claim_ids),
    error = function(error) {
      tempest_product_report_abort(
        "Briefing-item provenance has invalid claim ids.",
        parent = error
      )
    }
  )
  confidence <- value$confidence %||% NA_character_
  tryCatch(
    TempestBriefingItem(
      kind = value$kind,
      text = value$text,
      claim_ids = claim_ids,
      confidence = confidence,
      item_id = value$item_id
    ),
    error = function(error) {
      tempest_product_report_abort(
        "Briefing-item provenance does not match its content.",
        parent = error
      )
    }
  )
}

tempest_briefing_markdown_parse_line <- function(line) {
  pattern <- paste0(
    "^(.*) <!-- tempest-briefing-item:([0-9a-f]+) -->$"
  )
  match <- regexec(pattern, line, perl = TRUE)
  parts <- regmatches(line, match)[[1]]
  if (length(parts) == 0L) {
    return(NULL)
  }
  json <- tempest_briefing_hex_decode(parts[[3]])
  value <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(error) {
      tempest_product_report_abort(
        "Briefing-item provenance is not valid JSON.",
        parent = error
      )
    }
  )
  list(visible = parts[[2]], item = tempest_briefing_item_from_list(value))
}

tempest_briefing_items_from_markdown <- function(
  text,
  workspace,
  min_support_score = 0.7
) {
  if (!rlang::is_string(text) || is.na(text)) {
    tempest_product_report_abort(
      "Briefing-item validation requires one Markdown string."
    )
  }
  if (!inherits(workspace, "ResearchWorkspace")) {
    tempest_research_workspace_abort(
      "Briefing-item validation requires a ResearchWorkspace."
    )
  }
  min_support_score <- tempest_normalize_min_support_score(min_support_score)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  reserved <- grepl(
    "^[[:space:]]*-[[:space:]]*\\*\\*(Assessment|Review|No material change):\\*\\*",
    lines,
    perl = TRUE
  )
  parsed <- lapply(lines, tempest_briefing_markdown_parse_line)
  has_record <- !vapply(parsed, is.null, logical(1))
  if (any(reserved & !has_record)) {
    tempest_product_report_abort(
      "Labeled briefing synthesis requires exact item provenance."
    )
  }
  if (!any(has_record)) {
    return(list())
  }
  items <- lapply(parsed[has_record], `[[`, "item")
  parsed_lines <- parsed[has_record]
  accepted <- tempest_workspace_accepted_claim_keys(workspace)
  for (index in seq_along(items)) {
    item <- items[[index]]
    claims <- lapply(item@claim_ids, workspace$get_proposed_claim)
    if (any(vapply(claims, is.null, logical(1)))) {
      tempest_product_report_abort(
        "Briefing-item provenance references an unknown claim."
      )
    }
    verified <- vapply(
      claims,
      tempest_stage_claim_verified,
      logical(1),
      workspace = workspace,
      min_support_score = min_support_score
    )
    if (!all(verified)) {
      tempest_product_report_abort(
        "Briefing-item provenance requires threshold-verified claims."
      )
    }
    if (
      item@kind %in%
        c("observation", "no_change") &&
        (length(claims) != 1L || !identical(item@text, claims[[1]]@claim_text))
    ) {
      tempest_product_report_abort(
        paste0(
          "A briefing observation or no-change item no longer matches its ",
          "exact claim."
        )
      )
    }
    synthesis_error <- tryCatch(
      {
        tempest_briefing_item_validate_synthesis(
          item@kind,
          item@text,
          claims
        )
        NULL
      },
      error = identity
    )
    if (inherits(synthesis_error, "condition")) {
      tempest_product_report_abort(
        paste0(
          "A briefing assessment or review action no longer matches its ",
          "bound verified claims."
        ),
        parent = synthesis_error
      )
    }
    if (
      !tempest_briefing_item_disposition_valid(
        item@kind,
        claims[[1]]@claim_text,
        accepted
      )
    ) {
      tempest_product_report_abort(
        paste0(
          "A briefing item no longer matches its claim's disposition against ",
          "the pinned accepted Claims."
        )
      )
    }
    expected <- tempest_briefing_item_markdown(item, workspace)
    actual <- paste(
      parsed_lines[[index]]$visible,
      tempest_briefing_item_provenance(item)
    )
    if (!identical(actual, expected)) {
      tempest_product_report_abort(
        "Briefing-item Markdown does not match its exact provenance."
      )
    }
  }
  ids <- vapply(items, \(item) item@item_id, character(1))
  counts <- table(ids)
  if (any(counts > 2L)) {
    tempest_product_report_abort(
      paste0(
        "A report can repeat an item only once between At a glance and its ",
        "detailed section."
      )
    )
  }
  repeated <- names(counts)[counts == 2L]
  if (length(repeated) > 0L) {
    headings <- rep(NA_character_, length(lines))
    current_heading <- NA_character_
    for (index in seq_along(lines)) {
      if (grepl("^## [^#]", lines[[index]], perl = TRUE)) {
        current_heading <- sub("^## ", "", lines[[index]])
      }
      headings[[index]] <- current_heading
    }
    item_headings <- headings[has_record]
    for (item_id in repeated) {
      locations <- item_headings[ids == item_id]
      if (sum(locations == "At a glance", na.rm = TRUE) != 1L) {
        tempest_product_report_abort(
          paste0(
            "A repeated briefing item must appear once in At a glance and ",
            "once in a detailed section."
          )
        )
      }
    }
  }
  items
}

tempest_briefing_markdown_assertion_input <- function(
  text,
  workspace,
  min_support_score = 0.7
) {
  items <- tempest_briefing_items_from_markdown(
    text,
    workspace,
    min_support_score = min_support_score
  )
  if (length(items) == 0L) {
    return(list(text = text, atomic_lines = integer()))
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  parsed <- lapply(lines, tempest_briefing_markdown_parse_line)
  atomic_lines <- integer()
  for (index in seq_along(lines)) {
    record <- parsed[[index]]
    if (is.null(record)) {
      next
    }
    if (identical(record$item@kind, "observation")) {
      lines[[index]] <- record$visible
      atomic_lines <- c(atomic_lines, index)
    } else {
      lines[[index]] <- ""
    }
  }
  list(
    text = paste(lines, collapse = "\n"),
    atomic_lines = atomic_lines
  )
}

tempest_stage_evaluate_briefing_items <- function(output, context, stage) {
  items <- tempest_briefing_items_from_output(output, context)
  if (identical(stage, "lead_section")) {
    items <- tempest_briefing_items_validate_counts(
      items,
      limits = c(
        observation = 3L,
        assessment = 1L,
        review_action = 1L,
        no_change = 1L
      )
    )
  }
  list(
    output = tempest_briefing_items_markdown(
      items,
      tempest_stage_workspace(context),
      lead = identical(stage, "lead_section")
    ),
    support_status = "verified"
  )
}
