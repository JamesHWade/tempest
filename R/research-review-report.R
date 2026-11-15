# Scientific review remains separate from citation support and accepted knowledge.

#' Read scientific claim reviews
#'
#' Returns the exact advisory review records retained by a completed research
#' product. Each record binds a claim to captured source excerpts, an initial
#' reviewer, an independent adjudication, and a bounded evidence-search outcome.
#' `no_issue` means that the reviewers found no issue; it does not certify the
#' claim. Reviews do not replace [tempest_claim_supports()].
#'
#' @param x A completed [tempest_run()] product or published [tempest_session()].
#' @return A list of versioned plain records, in deterministic claim order.
#'   An empty list means no claims were reviewed.
#' @export
tempest_claim_reviews <- function(x) {
  context <- tempest_completed_product_context(
    research = x,
    abort = tempest_product_report_abort,
    boundary_class = "tempest_product_report_error",
    purpose = "Scientific review"
  )
  lapply(context$workspace$list_claim_reviews(), tempest_claim_review_record)
}

tempest_claim_review_markdown <- function(workspace) {
  reviews <- workspace$list_claim_reviews()
  if (!length(reviews)) {
    return("")
  }
  escape <- function(x) {
    tempest_markdown_escape_plain_text(
      gsub("[[:space:]]+", " ", x),
      "review text"
    )
  }
  lines <- unlist(
    lapply(reviews, function(value) {
      record <- tempest_claim_review_record(value)
      verdict <- switch(
        record$adjudication$verdict,
        no_issue = "No issue found",
        confirmed = "Concern confirmed by adjudicator",
        dismissed = "Concern dismissed by adjudicator",
        unresolved = "Unresolved"
      )
      evidence <- record$adjudication$evidence
      c(
        paste0("### ", escape(record$claim_id)),
        "",
        paste0("Claim: ", escape(record$claim_text)),
        "",
        paste0("Review outcome: **", verdict, "**."),
        "",
        if (nzchar(record$reviewer$finding)) {
          paste0("Initial finding: ", escape(record$reviewer$finding))
        },
        paste0("Adjudication: ", escape(record$adjudication$rationale)),
        if (length(evidence)) {
          vapply(
            evidence,
            function(item) {
              source <- Filter(
                function(source) identical(source$source_id, item$source_id),
                record$dossier
              )[[1L]]
              locator <- if (grepl("^https?://", source$locator)) {
                tempest_markdown_source_url(source$locator)
              } else {
                escape(source$locator)
              }
              paste0(
                "Evidence (",
                escape(item$source_id),
                "): \u201c",
                escape(item$quote),
                "\u201d Source: ",
                locator
              )
            },
            character(1)
          )
        },
        paste0(
          "Evidence search: ",
          escape(record$repair$status),
          ".",
          if (nzchar(record$repair$query)) {
            paste0(" Query: ", escape(record$repair$query))
          }
        ),
        paste0("Review identity: `", record$review_id, "`."),
        ""
      )
    }),
    use.names = FALSE
  )
  paste(
    c(
      "## Scientific review",
      "",
      paste0(
        "Advisory review of ",
        length(reviews),
        " selected claim(s). ",
        "Other claims were not reviewed. No issue found is not proof of correctness. ",
        "Citation support and accepted knowledge are unchanged."
      ),
      "",
      lines
    ),
    collapse = "\n"
  )
}

tempest_report_append_claim_reviews <- function(report, workspace) {
  review <- tempest_claim_review_markdown(workspace)
  if (!nzchar(review)) {
    return(report)
  }
  paste0(report, "\n", review, "\n")
}

tempest_report_strip_claim_reviews <- function(report, workspace) {
  review <- tempest_claim_review_markdown(workspace)
  if (!nzchar(review)) {
    if (tempest_markdown_has_heading(report, "Scientific review")) {
      tempest_product_report_abort(
        "Report contains scientific review without retained records."
      )
    }
    return(report)
  }
  suffix <- paste0("\n", review, "\n")
  if (!endsWith(report, suffix)) {
    tempest_product_report_abort(
      "Report scientific review differs from its retained records."
    )
  }
  substr(report, 1L, nchar(report) - nchar(suffix))
}
