# Paired plant comparison at the highest concentration in datasets::CO2.
# Run by tempest_r_executor() from a task directory with base R only.

data <- as.data.frame(datasets::CO2)
data$Plant <- factor(data$Plant)
data$Type <- factor(data$Type, levels = c("Quebec", "Mississippi"))
data$Treatment <- factor(data$Treatment, levels = c("nonchilled", "chilled"))

highest_concentration <- max(data$conc)
high <- data[data$conc == highest_concentration, ]
if (anyDuplicated(high$Plant) || nrow(high) != nlevels(data$Plant)) {
  stop("Expected one highest-concentration measurement per plant.")
}

types <- levels(data$Type)
treatments <- levels(data$Treatment)
cell_n <- table(high$Type, high$Treatment)
if (any(cell_n != 3L)) {
  stop("The documented balanced 3-plant cells changed; review this analysis.")
}

cell_means <- aggregate(
  uptake ~ Type + Treatment,
  data = high,
  FUN = mean
)
cell_means$n_plants <- as.integer(cell_n[cbind(
  match(cell_means$Type, rownames(cell_n)),
  match(cell_means$Treatment, colnames(cell_n))
)])
write.csv(cell_means, "high-co2-cell-means.csv", row.names = FALSE)

# Table 1 pairs the treatment groups by table number; source-notes.md records
# the crosswalk from those numbers to the R `Plant` identifiers.
pair_map <- data.frame(
  Type = c(rep("Quebec", 3L), rep("Mississippi", 3L)),
  pair = c("Q1", "Q2", "Q3", "M1", "M2", "M3"),
  nonchilled_plant = c("Qn1", "Qn2", "Qn3", "Mn1", "Mn2", "Mn3"),
  chilled_plant = c("Qc3", "Qc1", "Qc2", "Mc3", "Mc2", "Mc1"),
  stringsAsFactors = FALSE
)
uptake_by_plant <- stats::setNames(high$uptake, as.character(high$Plant))
pair_data <- pair_map
pair_data$nonchilled_uptake <- unname(uptake_by_plant[
  pair_data$nonchilled_plant
])
pair_data$chilled_uptake <- unname(uptake_by_plant[pair_data$chilled_plant])
if (anyNA(pair_data$nonchilled_uptake) || anyNA(pair_data$chilled_uptake)) {
  stop("The plant IDs do not match the original study's pairing table.")
}
pair_data$difference <- pair_data$chilled_uptake - pair_data$nonchilled_uptake
write.csv(pair_data, "high-co2-paired-plants.csv", row.names = FALSE)

values <- split(pair_data$difference, pair_data$Type)
contrast_by_type <- vapply(
  types,
  function(type) mean(values[[type]]),
  numeric(1)
)
pooled_equal_origin <- mean(contrast_by_type)
interaction <- unname(
  contrast_by_type[["Mississippi"]] - contrast_by_type[["Quebec"]]
)

set.seed(20261114L)
n_boot <- 20000L
bootstrap <- matrix(NA_real_, nrow = n_boot, ncol = 4L)
colnames(bootstrap) <- c(
  "Quebec",
  "Mississippi",
  "interaction",
  "equal-origin mean"
)
for (iteration in seq_len(n_boot)) {
  draw <- vapply(
    types,
    function(type) {
      mean(sample(values[[type]], replace = TRUE))
    },
    numeric(1)
  )
  bootstrap[iteration, "Quebec"] <- draw[["Quebec"]]
  bootstrap[iteration, "Mississippi"] <- draw[["Mississippi"]]
  bootstrap[iteration, "interaction"] <- draw[["Mississippi"]] -
    draw[["Quebec"]]
  bootstrap[iteration, "equal-origin mean"] <- mean(draw)
}

percentile_interval <- function(x) {
  unname(stats::quantile(x, probs = c(0.025, 0.975), type = 6))
}
contrasts <- data.frame(
  contrast = c(
    "Quebec paired: chilled - nonchilled",
    "Mississippi paired: chilled - nonchilled",
    "Equal-origin mean paired contrast",
    "Interaction: Mississippi paired effect - Quebec paired effect"
  ),
  estimate = c(
    contrast_by_type[["Quebec"]],
    contrast_by_type[["Mississippi"]],
    pooled_equal_origin,
    interaction
  ),
  lower_95 = c(
    percentile_interval(bootstrap[, "Quebec"])[[1L]],
    percentile_interval(bootstrap[, "Mississippi"])[[1L]],
    percentile_interval(bootstrap[, "equal-origin mean"])[[1L]],
    percentile_interval(bootstrap[, "interaction"])[[1L]]
  ),
  upper_95 = c(
    percentile_interval(bootstrap[, "Quebec"])[[2L]],
    percentile_interval(bootstrap[, "Mississippi"])[[2L]],
    percentile_interval(bootstrap[, "equal-origin mean"])[[2L]],
    percentile_interval(bootstrap[, "interaction"])[[2L]]
  ),
  stringsAsFactors = FALSE
)
write.csv(contrasts, "high-co2-pair-bootstrap.csv", row.names = FALSE)

fmt <- function(x) formatC(x, digits = 2, format = "f")
cat(sprintf(
  paste0(
    "Highest concentration: %.0f µL/L using the original Table 1 label. One row per plant; ",
    "6 a priori plant pairs (3 per origin) follow Potvin et al. Table 1.\n"
  ),
  highest_concentration
))
for (type in types) {
  interval <- percentile_interval(bootstrap[, type])
  cat(sprintf(
    "%s: mean paired chilled-minus-nonchilled uptake = %s umol/m^2 sec; pair bootstrap 95%% percentile interval [%s, %s].\n",
    type,
    fmt(contrast_by_type[[type]]),
    fmt(interval[[1L]]),
    fmt(interval[[2L]])
  ))
}
interval <- percentile_interval(bootstrap[, "equal-origin mean"])
cat(sprintf(
  "Equal-origin average paired contrast = %s umol/m^2 sec; pair bootstrap 95%% percentile interval [%s, %s].\n",
  fmt(pooled_equal_origin),
  fmt(interval[[1L]]),
  fmt(interval[[2L]])
))
interval <- percentile_interval(bootstrap[, "interaction"])
cat(sprintf(
  "Origin-by-treatment interaction contrast (Mississippi minus Quebec paired effect) = %s umol/m^2 sec; pair bootstrap 95%% percentile interval [%s, %s].\n",
  fmt(interaction),
  fmt(interval[[1L]]),
  fmt(interval[[2L]])
))
cat(paste(
  "Bootstrap resampling was stratified by origin and resampled the three",
  "pre-specified plant-pair differences within each origin (20,000 draws,",
  "percentile interval, seed 20261114). Pairing follows the original paper's",
  "Table 1 and was mapped to the R plant IDs in the saved CSV. Only three",
  "pairs per origin make these intervals exploratory and sensitive to each",
  "pair. Repeated concentrations are not treated as independent replicates.",
  "This is a descriptive contrast; the small dataset does not establish a",
  "general causal effect.\n"
))
