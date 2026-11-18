tempest_researcher_evidence_ui <- function(report) {
  bundle <- jsonlite::read_json(file.path(report@path, "report.json"))
  tags <- htmltools::tags
  sources <- tempest_enriched_render_items(bundle$sources)
  source_item <- function(source) {
    title <- tempest_enriched_render_text(source$title)
    if (!nzchar(title)) {
      title <- tempest_enriched_render_text(source$id, "Source")
    }
    url <- tempest_enriched_render_safe_url(source$url) %||%
      tempest_enriched_render_safe_url(source$locator)
    tags$li(
      if (is.null(url)) {
        title
      } else {
        tags$a(
          title,
          href = url,
          target = "_blank",
          rel = "noopener noreferrer"
        )
      }
    )
  }
  outcomes <- function(tasks, label, empty) {
    tasks <- tempest_enriched_render_items(tasks)
    tags$details(
      tags$summary(sprintf("%s (%d)", label, length(tasks))),
      if (!length(tasks)) {
        tags$p(empty)
      } else {
        tags$ul(lapply(tasks, function(task) {
          status <- switch(
            tempest_enriched_render_text(task$status),
            succeeded = "Completed",
            skipped = "Skipped",
            failed = "Failed",
            "Status unavailable"
          )
          tags$li(tags$strong(status), " \u00b7 ", task$title %||% task$id)
        }))
      }
    )
  }
  tags$section(
    class = "researcher-evidence-navigator",
    `aria-label` = "Evidence navigator",
    tags$h2(tempest_enriched_render_text(bundle$title, "Saved report")),
    tags$p(
      if (identical(report@status, "partial")) {
        "Partial report"
      } else {
        "Complete report"
      },
      " \u00b7 ",
      length(sources),
      if (length(sources) == 1L) " recorded source" else " recorded sources"
    ),
    tags$details(
      tags$summary(sprintf("Recorded sources (%d)", length(sources))),
      if (length(sources)) {
        tags$ol(lapply(sources, source_item))
      } else {
        tags$p("No sources recorded.")
      }
    ),
    outcomes(bundle$analyses, "Analysis outcomes", "No analyses recorded."),
    outcomes(
      bundle$illustrations,
      "Illustration outcomes",
      "No illustrations recorded."
    )
  )
}
