test_that("example compares observed means at the plant level", {
  skip_if_not_installed("shiny")
  html <- as.character(tempest_researcher_example_ui())
  plant <- stats::aggregate(
    uptake ~ Plant + Treatment + Type,
    datasets::CO2,
    mean
  )
  groups <- stats::aggregate(uptake ~ Treatment + Type, plant, mean)
  for (value in groups$uptake) {
    expect_match(html, sprintf("%.2f", value), fixed = TRUE)
  }
  expect_match(html, "three plants", fixed = TRUE)
  expect_match(html, "live-research/index.html", fixed = TRUE)
})
