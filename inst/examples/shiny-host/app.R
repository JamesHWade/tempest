library(shiny)
library(bslib)
library(tempest)

ui <- page_sidebar(
  title = "Embedded Tempest STORM",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    textAreaInput(
      "topic",
      "Research topic",
      value = "Evidence for recycling grid-scale batteries",
      rows = 4
    ),
    actionButton("run", "Run STORM", class = "btn-primary w-100")
  ),
  card(
    card_header("Evidence-backed report"),
    card_body(
      uiOutput("status"),
      tags$pre(
        style = "white-space: pre-wrap;",
        textOutput("report")
      )
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  error_message <- reactiveVal(NULL)

  observeEvent(input$run, {
    error_message(NULL)
    result(NULL)
    tryCatch(
      {
        value <- tempest_run(
          input$topic,
          config = tempest_config(),
          verbose = FALSE
        )
        result(value)
      },
      error = function(error) {
        error_message(conditionMessage(error))
      }
    )
  })

  output$status <- renderUI({
    if (!is.null(error_message())) {
      return(div(class = "text-danger", error_message()))
    }
    if (is.null(result())) {
      return(p("Choose a focused topic, then run the scripted STORM product."))
    }
    p(class = "text-success", "Research completed.")
  })

  output$report <- renderText({
    value <- result()
    if (is.null(value)) {
      return("")
    }
    value$report_md
  })
}

shinyApp(ui, server)
