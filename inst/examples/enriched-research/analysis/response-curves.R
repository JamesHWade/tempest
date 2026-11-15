# Plant response curves and plant-level integrated-curve contrasts for CO2.
# Run by tempest_r_executor() from a task directory with base R only.

source_data <- as.data.frame(datasets::CO2)
write.csv(source_data, "co2-input.csv", row.names = FALSE)
data <- source_data
data$Plant <- factor(data$Plant)
data$Type <- factor(data$Type, levels = c("Quebec", "Mississippi"))
data$Treatment <- factor(data$Treatment, levels = c("nonchilled", "chilled"))
data <- data[order(data$Type, data$Treatment, data$Plant, data$conc), ]

plants <- split(data, data$Plant)
plant_auc <- do.call(
  rbind,
  lapply(plants, function(plant) {
    plant <- plant[order(plant$conc), ]
    if (anyDuplicated(plant$conc) || nrow(plant) < 2L) {
      stop("Each plant must have one observation at each concentration.")
    }
    data.frame(
      Plant = as.character(plant$Plant[[1L]]),
      Type = as.character(plant$Type[[1L]]),
      Treatment = as.character(plant$Treatment[[1L]]),
      concentrations = nrow(plant),
      auc = sum(
        diff(plant$conc) *
          (head(plant$uptake, -1L) + tail(plant$uptake, -1L)) /
          2
      ),
      stringsAsFactors = FALSE
    )
  })
)
write.csv(plant_auc, "plant-integrated-uptake.csv", row.names = FALSE)

curve_means <- aggregate(
  uptake ~ Type + Treatment + conc,
  data = data,
  FUN = mean
)
curve_means$n_plants <- 3L
write.csv(curve_means, "response-curve-means.csv", row.names = FALSE)

types <- levels(data$Type)
treatments <- levels(data$Treatment)
auc_cell_means <- aggregate(
  auc ~ Type + Treatment,
  data = plant_auc,
  FUN = mean
)
auc_cell_counts <- table(plant_auc$Type, plant_auc$Treatment)
auc_cell_means$n_plants <- as.integer(auc_cell_counts[cbind(
  match(auc_cell_means$Type, rownames(auc_cell_counts)),
  match(auc_cell_means$Treatment, colnames(auc_cell_counts))
)])
write.csv(auc_cell_means, "integrated-uptake-cell-means.csv", row.names = FALSE)

# Table 1 pairs the treatment groups by table number; source-notes.md records
# the crosswalk from those numbers to the R `Plant` identifiers.
pair_map <- data.frame(
  Type = c(rep("Quebec", 3L), rep("Mississippi", 3L)),
  pair = c("Q1", "Q2", "Q3", "M1", "M2", "M3"),
  nonchilled_plant = c("Qn1", "Qn2", "Qn3", "Mn1", "Mn2", "Mn3"),
  chilled_plant = c("Qc3", "Qc1", "Qc2", "Mc3", "Mc2", "Mc1"),
  stringsAsFactors = FALSE
)
auc_by_plant <- stats::setNames(plant_auc$auc, plant_auc$Plant)
pair_auc <- pair_map
pair_auc$nonchilled_auc <- unname(auc_by_plant[pair_auc$nonchilled_plant])
pair_auc$chilled_auc <- unname(auc_by_plant[pair_auc$chilled_plant])
if (anyNA(pair_auc$nonchilled_auc) || anyNA(pair_auc$chilled_auc)) {
  stop("The plant IDs do not match the original study's pairing table.")
}
pair_auc$difference <- pair_auc$chilled_auc - pair_auc$nonchilled_auc
write.csv(pair_auc, "integrated-uptake-paired-plants.csv", row.names = FALSE)

values <- split(pair_auc$difference, pair_auc$Type)
contrast_by_type <- vapply(
  types,
  function(type) mean(values[[type]]),
  numeric(1)
)
interaction <- unname(
  contrast_by_type[["Mississippi"]] - contrast_by_type[["Quebec"]]
)

set.seed(20261114L)
n_boot <- 20000L
bootstrap <- matrix(NA_real_, nrow = n_boot, ncol = 3L)
colnames(bootstrap) <- c("Quebec", "Mississippi", "interaction")
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
}
percentile_interval <- function(x) {
  unname(stats::quantile(x, probs = c(0.025, 0.975), type = 6))
}
auc_contrasts <- data.frame(
  contrast = c(
    "Quebec paired: chilled - nonchilled",
    "Mississippi paired: chilled - nonchilled",
    "Interaction: Mississippi paired effect - Quebec paired effect"
  ),
  estimate = c(
    contrast_by_type[["Quebec"]],
    contrast_by_type[["Mississippi"]],
    interaction
  ),
  lower_95 = c(
    percentile_interval(bootstrap[, "Quebec"])[[1L]],
    percentile_interval(bootstrap[, "Mississippi"])[[1L]],
    percentile_interval(bootstrap[, "interaction"])[[1L]]
  ),
  upper_95 = c(
    percentile_interval(bootstrap[, "Quebec"])[[2L]],
    percentile_interval(bootstrap[, "Mississippi"])[[2L]],
    percentile_interval(bootstrap[, "interaction"])[[2L]]
  ),
  stringsAsFactors = FALSE
)
write.csv(auc_contrasts, "integrated-uptake-contrasts.csv", row.names = FALSE)

colors <- c(nonchilled = "#1864AB", chilled = "#D9485F")
png("response-curves.png", width = 1400, height = 850, res = 150)
par(mfrow = c(1L, 2L), mar = c(4.4, 4.5, 3.2, 1.2), oma = c(0, 0, 1.2, 0))
for (type in types) {
  subset <- data[data$Type == type, ]
  plot(
    NA,
    xlim = range(data$conc),
    ylim = range(data$uptake),
    xlab = expression("CO2 concentration (" * mu * "L/L; Table 1)"),
    ylab = expression("CO2 uptake (" * mu * "mol/m"^2 * " sec)"),
    main = type,
    bty = "l"
  )
  for (treatment in treatments) {
    plant_names <- levels(droplevels(subset$Plant[
      subset$Treatment == treatment
    ]))
    for (plant_name in plant_names) {
      plant <- subset[subset$Plant == plant_name, ]
      plant <- plant[order(plant$conc), ]
      lines(
        plant$conc,
        plant$uptake,
        col = grDevices::adjustcolor("#66717C", alpha.f = 0.45),
        lwd = 1
      )
      points(
        plant$conc,
        plant$uptake,
        col = grDevices::adjustcolor("#66717C", alpha.f = 0.55),
        pch = 16,
        cex = 0.55
      )
    }
    mean_curve <- curve_means[
      curve_means$Type == type & curve_means$Treatment == treatment,
    ]
    mean_curve <- mean_curve[order(mean_curve$conc), ]
    lines(
      mean_curve$conc,
      mean_curve$uptake,
      col = colors[[treatment]],
      lwd = 3
    )
    points(
      mean_curve$conc,
      mean_curve$uptake,
      col = colors[[treatment]],
      pch = 19,
      cex = 1.0
    )
  }
  legend(
    "topleft",
    legend = c("Nonchilled mean", "Chilled mean", "Individual plant"),
    col = c(colors[["nonchilled"]], colors[["chilled"]], "#66717C"),
    lty = c(1L, 1L, 1L),
    lwd = c(3L, 3L, 1L),
    pch = c(19, 19, 16),
    bty = "n",
    cex = 0.82
  )
}
mtext(
  "Plant trajectories and treatment means across the observed CO2 range",
  outer = TRUE,
  cex = 1.1
)
invisible(grDevices::dev.off())

fmt <- function(x) formatC(x, digits = 1, format = "f")
cat(paste(
  "Each plant's seven repeated measurements were summarized by the",
  "trapezoidal area under its uptake-versus-concentration curve over the",
  "observed concentration range. This is an integrated descriptive index,",
  "not total carbon fixed. Pair bootstrap intervals resample the three",
  "pre-specified pair differences within each origin (20,000 draws,",
  "percentile interval, seed 20261114). Pairs follow original Table 1 and",
  "are mapped to R plant IDs in the archived CSVs.\n"
))
for (type in types) {
  interval <- percentile_interval(bootstrap[, type])
  cat(sprintf(
    "%s mean paired AUC contrast (chilled - nonchilled) = %s umol/m^2 sec x µL/L; pair bootstrap 95%% percentile interval [%s, %s].\n",
    type,
    fmt(contrast_by_type[[type]]),
    fmt(interval[[1L]]),
    fmt(interval[[2L]])
  ))
}
interval <- percentile_interval(bootstrap[, "interaction"])
cat(sprintf(
  "AUC interaction (Mississippi minus Quebec paired effect) = %s umol/m^2 sec x µL/L; pair bootstrap 95%% percentile interval [%s, %s].\n",
  fmt(interaction),
  fmt(interval[[1L]]),
  fmt(interval[[2L]])
))
cat(paste(
  "The figure shows all 12 plant trajectories and origin-specific treatment",
  "means. The interaction contrast describes how the chilling contrast differs",
  "between the two origins; with three pairs per origin, it is exploratory.",
  "The plot is descriptive and does not fit a repeated-measures inferential",
  "model. The R help labels concentration mL/L while original Table 1 uses",
  "µL/L; the source notes flag this discrepancy. Neither these small groups",
  "nor the dataset alone justifies a",
  "causal interpretation or generalization beyond this experiment.\n"
))
