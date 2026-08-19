library(shiny)
library(bslib)
library(tempest)

ui <- page_fillable(
  theme = bs_theme(version = 5),
  tags$header(
    class = "container-fluid pt-3",
    tags$h1(class = "h3", "Embedded Tempest STORM")
  ),
  tempest_shiny_ui("research", panels = "storm")
)

server <- function(input, output, session) {
  tempest_shiny_server(
    "research",
    config = tempest_config(),
    panels = "storm"
  )
}

shinyApp(ui, server)
