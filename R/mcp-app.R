# A read-only MCP projection of one completed research product.

#' Review a completed research product in an MCP App
#'
#' `r lifecycle::badge("experimental")`
#'
#' Builds a research card with a report, claim-by-span evidence, and a bounded
#' execution review. The three read-only tools also work in MCP clients without
#' interactive pages. The card captures one exact completed product at creation;
#' reopening or inspecting it never runs research or changes accepted knowledge.
#'
#' The model receives a compact summary and can request a particular claim or
#' the report. The page receives the complete report and presentation tables.
#' Raw HTML and unsafe links in the report are suppressed when it is rendered.
#' Graft acceptance and withdrawal remain explicit host operations; this card
#' cannot record review decisions or authorize reuse.
#'
#' @param research A completed [tempest_run()] product or succeeded, quiescent
#'   `TempestSession`.
#' @param name A non-empty MCP App name prefix. The exact review identity is
#'   appended so a new product gets a distinct resource. Use distinct prefixes
#'   when hosting multiple cards for the same product together.
#' @return A `shinymcp::McpApp`. Preview with `shinymcp::preview_app()`, serve
#'   with `shinymcp::serve()`, or embed with `shinymcp::mcp_embed()`.
#' @seealso [tempest_trajectory_review()], [tempest_publish_artifact_research()]
#' @examples
#' \dontrun{
#' result <- tempest_run("Battery recycling")
#' card <- tempest_mcp_app(result)
#' shinymcp::preview_app(card)
#'
#' # A deterministic ecosystem demonstration, without credentials or network.
#' tempest_app(demo = TRUE)
#' }
#' @export
tempest_mcp_app <- function(research, name = "tempest-research") {
  tempest_require("shinymcp", "Research cards require shinymcp.")
  tempest_require("shiny", "Research cards use Shiny UI.")
  rlang::check_string(name, allow_empty = FALSE)

  # Read and validate the completed product once. In particular, do not retain
  # a live Co-STORM session in a tool closure.
  review <- tempest_trajectory_review_data(tempest_trajectory_review(research))
  report <- tempest_report(research)
  claims <- tempest_claims(research)
  supports <- tempest_claim_supports(research)
  sources <- tempest_sources(research)
  rm(research)
  summary <- list(
    review_id = review$review_id,
    research_run_id = review$product$research_run_id,
    mode = review$product$mode,
    report_reference = review$product$report_reference,
    claims = nrow(claims),
    evidence_pairs = nrow(supports),
    sources = nrow(sources),
    stages = review$stages$total,
    agent_runs = review$agent_runs$total,
    acceptance = "provisional; acceptance is a separate host decision"
  )
  claim_choices <- stats::setNames(claims$claim_id, claims$claim_text)
  stage_table <- tempest_mcp_stage_table(review$stages$items)
  report_html <- tempest_mcp_report_html(report)
  source_rows <- match(supports$source_id, sources$id)
  supports$source_title <- sources$title[source_rows]
  supports$source_locator <- sources$locator[source_rows]
  compact_claims <- claims[c("claim_id", "claim_text", "verification_status")]
  display_claims <- claims[c("claim_text", "verification_status")]
  names(display_claims) <- c("Claim", "Assessment")

  inspect <- function(claim_id) {
    rlang::check_string(claim_id, allow_empty = FALSE)
    if (!claim_id %in% claims$claim_id) {
      tempest_abort(
        "The claim does not belong to this research card.",
        class = c("tempest_mcp_claim_error", "tempest_error")
      )
    }
    claim <- claims[claims$claim_id == claim_id, , drop = FALSE]
    pairs <- supports[supports$claim_id == claim_id, , drop = FALSE]
    shinymcp::mcp_tool_result(
      claim = claim$claim_text[[1L]],
      evidence = shinymcp::mcp_result_html(
        tempest_mcp_evidence_html(pairs),
        text = paste(pairs$quote, collapse = "\n")
      ),
      text = paste0(
        claim$claim_text[[1L]],
        "\n",
        "Verification: ",
        claim$verification_status[[1L]],
        "; ",
        nrow(pairs),
        " exact evidence pair(s)."
      ),
      data = list(
        review_id = summary$review_id,
        claim_id = claim_id,
        claim_text = claim$claim_text[[1L]],
        verification_status = claim$verification_status[[1L]],
        evidence = pairs
      )
    )
  }
  readonly <- ellmer::tool_annotations(
    read_only_hint = TRUE,
    destructive_hint = FALSE,
    idempotent_hint = TRUE,
    open_world_hint = FALSE
  )
  overview_tool <- ellmer::tool(
    function() {
      shinymcp::mcp_tool_result(
        summary = paste(
          summary$claims,
          "claims;",
          summary$evidence_pairs,
          "evidence pairs;",
          summary$sources,
          "sources. Review before accepting for reuse."
        ),
        report = shinymcp::mcp_result_html(
          report_html,
          text = "Completed research report."
        ),
        claims = shinymcp::mcp_result_table(display_claims),
        stages = shinymcp::mcp_result_table(stage_table),
        text = paste(
          "Completed",
          summary$mode,
          "research:",
          summary$claims,
          "claims,",
          summary$sources,
          "sources. Use inspect_tempest_claim for exact evidence",
          "or read_tempest_report for the report. Acceptance remains a host decision."
        ),
        data = c(summary, list(claim_index = compact_claims))
      )
    },
    name = "review_tempest_research",
    description = "Open a completed Tempest research card and list its claims.",
    arguments = list(),
    annotations = readonly
  )
  claim_tool <- ellmer::tool(
    inspect,
    name = "inspect_tempest_claim",
    description = "Read one claim and its exact evidence quotes and support assessments.",
    arguments = list(
      claim_id = ellmer::type_string("An exact claim_id from this card.")
    ),
    annotations = readonly
  )
  report_tool <- ellmer::tool(
    function() {
      shinymcp::mcp_tool_result(
        text = report,
        data = list(review_id = summary$review_id, report_md = report)
      )
    },
    name = "read_tempest_report",
    description = "Read the exact completed Markdown report; never reruns research.",
    arguments = list(),
    annotations = readonly
  )
  ui <- shiny::fluidPage(
    shiny::tags$style(shiny::HTML(paste(
      ".tempest-card {max-width:1100px;margin:auto;padding:1rem;}",
      ".tempest-card table {font-size:1.4rem;overflow-wrap:anywhere;}",
      ".tempest-card pre {white-space:pre-wrap;overflow-wrap:anywhere;}",
      ".tempest-card .tab-content {padding-top:1rem;}",
      ".tempest-evidence {border:1px solid #ddd;border-radius:8px;padding:1.5rem;margin-top:1rem;}",
      ".tempest-evidence blockquote {border-color:#196c72;background:#f6f7f9;padding:1rem;font-size:1.6rem;}",
      ".tempest-evidence .source-locator {overflow-wrap:anywhere;}"
    ))),
    shiny::div(
      class = "tempest-card",
      shiny::h2("Research review"),
      shinymcp::mcp_text("summary"),
      shiny::tabsetPanel(
        id = "review_section",
        selected = "Report",
        shiny::tabPanel("Report", shinymcp::mcp_html("report")),
        shiny::tabPanel(
          "Evidence",
          shinymcp::mcp_table("claims"),
          if (nrow(claims)) {
            shiny::selectInput("claim_id", "Inspect a claim", claim_choices)
          },
          shinymcp::mcp_text("claim"),
          shinymcp::mcp_html("evidence")
        ),
        shiny::tabPanel(
          "Execution",
          shiny::p(
            "Structured stages use dsprrr. Open-ended expert work uses deputy."
          ),
          shinymcp::mcp_table("stages"),
          shiny::tags$details(
            shiny::tags$summary("Exact review identity"),
            shiny::tags$code(summary$review_id)
          )
        )
      ),
      shiny::tags$script(shiny::HTML(
        "window.addEventListener('load', function() {
          var tab = document.querySelector('#review_section a[data-value=\"Report\"]');
          if (tab) tab.click();
        }, {once: true});"
      ))
    )
  )
  tools <- list(overview_tool, report_tool)
  if (nrow(claims)) {
    tools <- c(tools, list(claim_tool))
  }
  shinymcp::mcp_app(
    ui = ui,
    tools = tools,
    name = paste0(
      name,
      "-",
      digest::digest(summary$review_id, algo = "sha256", serialize = FALSE)
    ),
    title = "Tempest research review",
    description = "Inspect a completed report, its exact claim evidence, and research stages.",
    tool_outputs = list(
      review_tempest_research = c("summary", "report", "claims", "stages"),
      read_tempest_report = character(),
      inspect_tempest_claim = if (nrow(claims)) c("claim", "evidence") else NULL
    )[vapply(tools, \(tool) tool@name, character(1))]
  )
}

tempest_mcp_evidence_html <- function(pairs) {
  if (!nrow(pairs)) {
    return(as.character(htmltools::tags$p(
      "No exact evidence pairs are recorded for this claim."
    )))
  }
  cards <- lapply(seq_len(nrow(pairs)), function(index) {
    pair <- pairs[index, , drop = FALSE]
    identity <- pair[c(
      "claim_support_id",
      "claim_id",
      "evidence_span_id",
      "source_id",
      "start_offset",
      "end_offset",
      "page",
      "section_heading"
    )]
    htmltools::tags$article(
      class = "tempest-evidence",
      htmltools::tags$h4(pair$source_title[[1L]]),
      htmltools::tags$blockquote(pair$quote[[1L]]),
      htmltools::tags$p(
        htmltools::tags$strong("Support assessment: "),
        pair$verification_status[[1L]],
        paste0(" \u00b7 score ", pair$support_score[[1L]])
      ),
      htmltools::tags$p(pair$rationale[[1L]]),
      htmltools::tags$p(
        class = "source-locator",
        htmltools::tags$strong("Source: "),
        pair$source_locator[[1L]]
      ),
      htmltools::tags$details(
        htmltools::tags$summary("Evidence identity and location"),
        htmltools::tags$pre(as.character(jsonlite::toJSON(
          identity,
          dataframe = "rows",
          auto_unbox = TRUE,
          pretty = TRUE,
          na = "null"
        )))
      )
    )
  })
  as.character(htmltools::tagList(cards))
}

tempest_mcp_stage_table <- function(stages) {
  tibble::tibble(
    Stage = vapply(stages, `[[`, character(1), "stage"),
    Status = vapply(stages, `[[`, character(1), "status"),
    Path = vapply(stages, `[[`, character(1), "execution_path"),
    Support = vapply(stages, `[[`, character(1), "support_status"),
    Fallback = vapply(stages, `[[`, logical(1), "fallback_taken")
  )
}

tempest_mcp_report_html <- function(report) {
  report <- gsub("<!-- tempest-briefing-item:[[:xdigit:]]+ -->", "", report)
  html <- commonmark::markdown_html(
    report,
    extensions = TRUE,
    footnotes = TRUE
  )
  document <- xml2::read_html(html)
  xml2::xml_remove(xml2::xml_find_all(
    document,
    paste0(
      "//script|//style|//iframe|//object|//embed|//svg|//math|//template|",
      "//form|//input|//button|//textarea|//select|//img|//audio|//video|",
      "//source|//link|//meta|//base|//comment()"
    )
  ))
  allowed_elements <- c(
    "p",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "blockquote",
    "pre",
    "code",
    "em",
    "strong",
    "del",
    "a",
    "ul",
    "ol",
    "li",
    "hr",
    "br",
    "table",
    "thead",
    "tbody",
    "tr",
    "th",
    "td",
    "section",
    "sup"
  )
  elements <- xml2::xml_find_all(document, "//body//*")
  for (element in rev(elements)) {
    if (!xml2::xml_name(element) %in% allowed_elements) {
      for (child in xml2::xml_contents(element)) {
        xml2::xml_add_sibling(element, child, .where = "before")
      }
      xml2::xml_remove(element)
    }
  }
  for (element in xml2::xml_find_all(document, "//body//*")) {
    allowed_attributes <- switch(
      xml2::xml_name(element),
      a = c("id", "href", "title"),
      ol = c("id", "start"),
      "id"
    )
    attributes <- xml2::xml_attrs(element)
    xml2::xml_attrs(element) <- attributes[
      names(attributes) %in% allowed_attributes
    ]
  }
  links <- xml2::xml_find_all(document, "//a[@href]")
  href <- xml2::xml_attr(links, "href")
  safe <- grepl("^(https?://|#)", href, ignore.case = TRUE) &
    !grepl("[[:cntrl:]]", href)
  for (link in links[!safe]) {
    attributes <- xml2::xml_attrs(link)
    xml2::xml_attrs(link) <- attributes[names(attributes) != "href"]
  }
  paste(
    as.character(xml2::xml_contents(xml2::xml_find_first(document, "//body"))),
    collapse = "\n"
  )
}
