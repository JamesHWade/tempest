# A supplied plan bypasses model-based planning and makes this demo repeatable.

tempest_co2_enrichment_plan <- function(
  research = NULL,
  example_dir,
  source_ids = NULL,
  max_analyses = 2L,
  max_illustrations = 1L
) {
  if (is.null(source_ids)) {
    source_ids <- as.character(tempest::tempest_sources(research)$id)
  }
  source_ids <- as.character(source_ids)
  if (!length(source_ids) || anyNA(source_ids) || anyDuplicated(source_ids)) {
    stop("The completed fixture product did not retain unique source IDs.")
  }
  if (max_analyses < 2L || max_illustrations < 1L) {
    stop("This worked plan needs budgets of two analyses and one illustration.")
  }
  read_code <- function(name) {
    path <- file.path(example_dir, "analysis", name)
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  list(
    title = "Cold treatment, contrasting responses",
    summary = paste(
      "Authored offline research replay; no live model or search evaluation.",
      "The R analyses below are actually executed. This reproducible study of CO2 uptake in barnyard",
      "grass (Echinochloa crus-galli) follows the original study's matched",
      "plant design. The R help summarizes chilling as overnight; the article",
      "specifies 14 hours at 7 C followed by 10 hours of recovery at 20 C.",
      "The help and Table 1 also differ on units. Estimates are descriptive",
      "for these three pairs per origin."
    ),
    analyses = list(
      list(
        id = "high-co2-uptake",
        title = "Paired plant contrast at the highest CO2 concentration",
        question = paste(
          "At the highest concentration in datasets::CO2, using the original",
          "study's pre-specified plant pairs, what are the",
          "chilled-minus-nonchilled uptake contrasts within each origin, their",
          "equal-origin average, and the origin-by-treatment interaction?"
        ),
        code = read_code("high-co2.R"),
        source_ids = source_ids,
        assumptions = c(
          "The built-in datasets::CO2 object is the exact local analysis input.",
          "The analysis maps tabled control and chilled plant numbers to the R plant IDs and writes each matched pair to CSV.",
          "Potvin et al. Table 1 labels the concentration values µL/L; the R help says mL/L, and the methods text rounds the lowest table value from 95 to 100.",
          "The paired bootstrap resamples three pre-specified pair differences within each origin; intervals are exploratory, not randomization-based causal intervals."
        )
      ),
      list(
        id = "response-curves",
        title = "Repeated plant trajectories and integrated response",
        question = paste(
          "How do individual and mean uptake curves vary by origin and",
          "treatment, and how does the plant-level integrated-curve contrast",
          "and its origin interaction look across the observed recorded-concentration range?"
        ),
        code = read_code("response-curves.R"),
        source_ids = source_ids,
        assumptions = c(
          "Each plant has one uptake value at each of the seven measured concentrations.",
          "The original Table 1 labels concentration µL/L; the R help's mL/L label differs and is flagged in the report.",
          "The trapezoidal area integrates uptake over the observed concentration axis; it is an index, not total carbon fixed.",
          "The bootstrap resamples three matched plant-pair differences within each origin and does not treat concentration rows as independent plants.",
          "The curves and interaction contrasts are descriptive; no repeated-measures inferential model is fit."
        )
      )
    ),
    illustrations = list(list(
      id = "greenhouse-concept",
      title = "Conceptual greenhouse scene",
      prompt = paste(
        "A gentle editorial illustration of several Echinochloa crus-galli",
        "barnyard grass plants in pots beside a cool blue greenhouse chamber,",
        "warm natural greenhouse light, scientifically plausible grass leaves",
        "and seed heads, landscape composition, no text, labels, charts, numbers,",
        "or implied measurements. This is conceptual artwork, not experimental evidence."
      ),
      alt = paste(
        "Potted barnyard grass plants in a sunlit greenhouse beside a cool",
        "blue chamber; conceptual illustration, not a depiction of measured results."
      ),
      caption = paste(
        "AI-generated conceptual illustration of barnyard grass plants near a",
        "cool chamber. It depicts no observed setup or result and is not evidence."
      )
    ))
  )
}
