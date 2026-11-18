tempest_researcher_example_ui <- function() {
  # Plant-level summaries from the published example's output-1.csv.
  bars <- function(region, means) {
    shiny::div(
      class = "tempest-example-group",
      shiny::tags$strong(region),
      lapply(seq_along(means), function(i) {
        shiny::div(
          class = "tempest-example-row",
          shiny::span(c("Nonchilled", "Chilled")[[i]]),
          shiny::div(
            class = "tempest-example-track",
            shiny::span(
              class = c("control", "chilled")[[i]],
              style = paste0("width:", 100 * means[[i]] / 40, "%")
            )
          ),
          shiny::span(sprintf("%.2f", means[[i]]))
        )
      })
    )
  }

  shiny::tags$article(
    class = "tempest-example",
    shiny::tags$style(htmltools::HTML(
      "
      .tempest-example {max-width:48rem;margin:auto;}
      .tempest-example .sample-label {font:600 .84rem var(--researcher-sans);color:#376b62;}
      .tempest-example figure {margin:1.5rem 0;background:#f3f7f4;padding:clamp(.85rem,3vw,1.6rem);border-radius:.6rem;}
      .tempest-example figcaption,.tempest-example-group {font-family:var(--researcher-sans);}
      .tempest-example figcaption {font-size:.88rem;}
      .tempest-example-group {margin-top:1rem;font-size:.85rem;}
      .tempest-example-row {display:grid;grid-template-columns:5.2rem minmax(0,1fr) 3rem;align-items:center;gap:.5rem;font-variant-numeric:tabular-nums;}
      .tempest-example-row span:last-child {text-align:right;}
      .tempest-example-track span {display:block;height:.65rem;background:#347c71;border-radius:0 .18rem .18rem 0;}
      .tempest-example-track .chilled {background:#556998;}
    "
    )),
    shiny::p(class = "sample-label", "Example report"),
    shiny::h2("Chilling and the uptake gap"),
    shiny::p(paste(
      "After chilling and recovery, the difference in average carbon uptake",
      "was larger among the Mississippi plants than among the Quebec plants."
    )),
    shiny::tags$figure(
      shiny::tags$figcaption(
        "Mean carbon uptake across seven concentrations (\u00b5mol CO\u2082/m\u00b2/s)"
      ),
      bars("Mississippi", c(25.95, 15.81)),
      bars("Quebec", c(35.33, 31.75))
    ),
    shiny::p(
      "Each group contained three plants. The full report shows the individual measurements, R analysis, and limits of the comparison."
    ),
    shiny::tags$a(
      href = "https://jameshwade.github.io/tempest/live-research/index.html",
      target = "_blank",
      rel = "noopener noreferrer",
      "Read the worked example"
    )
  )
}
