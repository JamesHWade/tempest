# Run with tempest::tempest_app(demo = TRUE).
library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Tempest · Research that remembers",
  theme = bs_theme(
    version = 5,
    bg = "#f6f7f9",
    fg = "#202c38",
    primary = "#196c72"
  ),
  sidebar = sidebar(
    width = 330,
    tags$p(
      class = "text-muted",
      "Synthetic evidence · scripted responses · no API keys"
    ),
    h4("1. Research"),
    p(
      "The first pilot note reports 82% recovery. A corrected note reports 74%."
    ),
    actionButton(
      "correction",
      "Research the corrected note",
      class = "btn-outline-primary"
    ),
    h4("2. Review and retain"),
    p("Read the claim and its exact source quote in the research card."),
    checkboxInput(
      "reviewed",
      "I reviewed this candidate and its evidence",
      FALSE
    ),
    textAreaInput(
      "reason",
      "Review reason",
      "Reviewed the synthetic pilot source and its exact evidence quote.",
      rows = 3
    ),
    actionButton("accept", "Accept reviewed research", class = "btn-primary"),
    h4("3. Reuse"),
    actionButton(
      "reopen",
      "Reopen in a fresh R process",
      class = "btn-outline-primary"
    ),
    actionButton(
      "withdraw",
      "Withdraw retained research",
      class = "btn-outline-secondary"
    ),
    actionButton("refresh", "Refresh review basis", class = "btn-link"),
    tags$small(
      "Acceptance is a host action. Models can inspect the card; they cannot accept or withdraw research."
    )
  ),
  tags$style(HTML(paste(
    ".bslib-sidebar-layout h4 {margin-top:1.5rem;font-size:1.05rem;}",
    ".bslib-sidebar-layout .btn {white-space:normal;margin-bottom:.5rem;}",
    ".demo-status {border-left:4px solid #196c72;padding:1rem;background:#fff;}",
    ".demo-status pre {white-space:pre-wrap;}"
  ))),
  tags$script(HTML(
    "$(function() { Shiny.addCustomMessageHandler('tempest-demo-busy', function(busy) {
      ['correction', 'reviewed', 'accept', 'reopen', 'withdraw', 'refresh'].forEach(function(id) {
        document.getElementById(id).disabled = busy;
      });
    }); });"
  )),
  div(
    class = "demo-status",
    role = "status",
    `aria-live` = "polite",
    textOutput("status")
  ),
  navset_card_tab(
    nav_panel("Research card", uiOutput("research_card")),
    nav_panel(
      "Retained evidence",
      h4("Current decision"),
      textOutput("decision"),
      h4("Latest reviewed report (historical after withdrawal)"),
      verbatimTextOutput("retained_report"),
      h4("Decision history"),
      tableOutput("history"),
      h4("Fresh-process proof"),
      verbatimTextOutput("restart")
    ),
    nav_panel(
      "How the packages work together",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Package"),
          tags$th("Role in this demonstration")
        )),
        tags$tbody(
          tags$tr(
            tags$td("deputy"),
            tags$td(
              "Runs the expert with bounded tools and records its execution identity."
            )
          ),
          tags$tr(
            tags$td("dsprrr"),
            tags$td(
              "Executes typed extraction, verification, outline, and writing programs."
            )
          ),
          tags$tr(
            tags$td("shinymcp"),
            tags$td(
              "Shows one exact research result as an interactive card and read-only tools."
            )
          ),
          tags$tr(
            tags$td("graft"),
            tags$td(
              "Retains exact report and evidence bytes, acceptance decisions, and withdrawals."
            )
          ),
          tags$tr(
            tags$td("tempest"),
            tags$td(
              "Owns the research workflow and validates evidence before publication and reuse."
            )
          )
        )
      ),
      p(
        "The replay exercises the actual package code. Its scores and responses are scripted, so it demonstrates composition and evidence integrity, not model quality."
      ),
      tags$details(
        tags$summary("Demo files and verified package versions"),
        verbatimTextOutput("versions")
      )
    )
  )
)

server <- function(input, output, session) {
  directory <- getOption(
    "tempest.demo_dir",
    file.path(tempdir(), "tempest-ecosystem-demo")
  )
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  directory <- normalizePath(directory, mustWork = TRUE)
  evidence_path <- file.path(directory, "evidence")
  store <- graft::graft_store(
    evidence_path,
    create = !dir.exists(evidence_path)
  )
  latest <- function() {
    events <- graft::graft_history(store, "synthetic-pilot")
    if (length(events)) tail(events, 1L)[[1L]] else NULL
  }
  worker <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "ellmerverse", "worker.R", package = "tempest"),
    worker
  )
  package_path <- if (
    requireNamespace("pkgload", quietly = TRUE) &&
      pkgload::is_dev_package("tempest")
  ) {
    pkgload::pkg_path()
  } else {
    NULL
  }
  process <- NULL
  task <- ExtendedTask$new(function(operation, ...) {
    process <<- callr::r_bg(
      worker$tempest_demo_worker,
      args = c(
        list(
          operation = operation,
          directory = directory,
          package_path = package_path
        ),
        list(...)
      ),
      libpath = .libPaths(),
      wd = directory
    )
    worker$tempest_demo_process_promise(process)
  })
  session$onSessionEnded(function() {
    if (!is.null(process) && process$is_alive()) process$kill()
  })
  candidate <- reactiveVal(NULL)
  card <- reactiveVal(NULL)
  review_id <- reactiveVal(NULL)
  basis <- reactiveVal(latest())
  status <- reactiveVal(
    "Preparing the initial research in a background process. The screen remains interactive."
  )
  restart <- reactiveVal("No fresh-process reuse attempted yet.")
  retained_report <- reactiveVal(
    "No report has been reviewed and accepted yet."
  )
  history <- reactiveVal(data.frame())
  reviewed_id <- reactiveVal(NULL)
  observeEvent(input$reviewed, {
    reviewed_id(if (isTRUE(input$reviewed)) review_id() else NULL)
  })
  attempt <- function(operation) {
    tryCatch(operation(), error = function(error) {
      while (inherits(error$parent, "condition")) {
        error <- error$parent
      }
      status(paste("Action refused:", conditionMessage(error)))
    })
  }
  reset_review <- function() {
    reviewed_id(NULL)
    updateCheckboxInput(session, "reviewed", value = FALSE)
  }
  begin <- function(operation, message, ...) {
    if (identical(task$status(), "running")) {
      stop("Wait for the current background task to finish.")
    }
    status(message)
    task$invoke(operation, ...)
  }
  observeEvent(task$status(), {
    session$sendCustomMessage(
      "tempest-demo-busy",
      identical(task$status(), "running")
    )
    if (identical(task$status(), "success")) {
      result <- task$result()
      retained_report(result$state$retained_report)
      history(result$state$history)
      if (result$operation %in% c("initial", "correction")) {
        candidate(result$candidate)
        card(result$card)
        review_id(result$review_id)
        basis(result$head)
        reset_review()
        status(
          if (identical(result$operation, "initial")) {
            "The initial 82% research is ready. Inspect its evidence, then record an explicit review."
          } else {
            "The corrected note reports 74%. It is a new candidate; accepted research has not changed."
          }
        )
      } else if (result$operation %in% c("accept", "withdraw")) {
        basis(result$value)
        reset_review()
        if (identical(result$operation, "withdraw")) {
          restart(
            "Acceptance withdrawn. Reopen again to verify that fresh reuse is refused."
          )
        }
        status(
          if (identical(result$operation, "accept")) {
            "Reviewed research accepted. The exact report and evidence are retained for a new session."
          } else {
            "Acceptance withdrawn. Historical reports remain readable; fresh reuse is now refused."
          }
        )
      } else if (identical(result$operation, "refresh")) {
        basis(result$state$head)
        reset_review()
        status(
          "Review basis refreshed. Review the candidate again before accepting."
        )
      } else {
        proof <- result$value
        restart(paste(
          "A fresh R process rechecked current acceptance and eligibility, admitted exact evidence",
          "into a new Co-STORM session, and verified unchanged sources after save/resume.",
          paste("Evidence records:", proof$records),
          paste("Selection:", proof$selection_id),
          sep = "\n"
        ))
        status(
          "Fresh-process reuse succeeded with the exact accepted evidence."
        )
      }
    } else if (identical(task$status(), "error")) {
      attempt(function() task$result())
    }
  })
  session$onFlushed(function() task$invoke("initial"), once = TRUE)
  observeEvent(
    input$correction,
    attempt(function() {
      begin(
        "correction",
        "Researching the corrected note in a background process."
      )
    })
  )
  observeEvent(input$refresh, {
    attempt(function() begin("refresh", "Refreshing the review basis."))
  })
  observeEvent(
    input$accept,
    attempt(function() {
      if (
        is.null(candidate()) ||
          !isTRUE(input$reviewed) ||
          !identical(reviewed_id(), review_id())
      ) {
        stop(
          "Review the exact current candidate and its source evidence first."
        )
      }
      reason <- trimws(input$reason)
      if (!nzchar(reason)) {
        stop("Supply a review reason.")
      }
      previous <- basis()
      begin(
        "accept",
        "Retaining reviewed research in a background process.",
        candidate = candidate(),
        review_id = review_id(),
        reason = reason,
        expected = if (is.null(previous)) NULL else previous@id
      )
    })
  )
  observeEvent(
    input$withdraw,
    attempt(function() {
      previous <- basis()
      if (is.null(previous) || !identical(previous@action, "accept")) {
        stop("There is no accepted research in this review basis to withdraw.")
      }
      begin("withdraw", "Withdrawing acceptance.", expected = previous@id)
    })
  )
  observeEvent(
    input$reopen,
    attempt(function() {
      events <- graft::graft_history(store, "synthetic-pilot")
      accepted <- Filter(
        function(event) identical(event@action, "accept"),
        events
      )
      if (!length(accepted)) {
        stop("Accept reviewed research before attempting reuse.")
      }
      begin(
        "reopen",
        "Rechecking acceptance and reopening exact evidence in a fresh R process.",
        decision_id = tail(accepted, 1L)[[1L]]@id
      )
    })
  )
  output$research_card <- renderUI({
    req(card())
    shinymcp::mcp_embed(card(), tool = "review_tempest_research")
  })
  output$status <- renderText(status())
  output$restart <- renderText(restart())
  output$decision <- renderText({
    event <- basis()
    if (is.null(event)) {
      "No accepted research."
    } else {
      paste(event@action, "·", event@reason)
    }
  })
  output$retained_report <- renderText({
    gsub("<!-- tempest-briefing-item:[[:xdigit:]]+ -->", "", retained_report())
  })
  output$history <- renderTable(history())
  output$versions <- renderText({
    packages <- c("tempest", "ellmer", "deputy", "dsprrr", "graft", "shinymcp")
    paste(
      c(
        paste(
          packages,
          vapply(
            packages,
            function(package) as.character(packageVersion(package)),
            character(1)
          )
        ),
        paste("Demo directory:", directory)
      ),
      collapse = "\n"
    )
  })
}

shinyApp(ui, server)
