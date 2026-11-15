# Base R only: uses datasets::CO2, with no downloads or package installation.
x <- datasets::CO2
cat('R version:', R.version.string, '\n')
cat('datasets version:', as.character(utils::packageVersion('datasets')), '\n')
# Inspect metadata before any data.frame conversion.
cat('\nObserved units attribute of datasets::CO2, printed without reinterpretation:\n')
print(attr(datasets::CO2, 'units'))
cat('This attribute is observed dataset metadata, not independent confirmation of physical units.\n')
cat('Concentrations remain unchanged; their physical units are unresolved here.\n')
cat('Uptake units below follow the inspected R help page, not an inferred conversion.\n')

d <- data.frame(Plant = as.character(x$Plant),
                Type = as.character(x$Type),
                Treatment = as.character(x$Treatment),
                conc = x$conc, uptake = x$uptake,
                stringsAsFactors = FALSE)
cat('\nMissing values by column:\n')
print(colSums(is.na(d)))
stopifnot(!anyNA(d), all(is.finite(d$conc)), all(is.finite(d$uptake)))
stopifnot(!anyDuplicated(d[c('Plant', 'conc')]))
by_plant <- split(d, d$Plant)
stopifnot(all(vapply(by_plant, function(z) length(unique(z$Type)) == 1L &&
  length(unique(z$Treatment)) == 1L, logical(1))))
grid <- sort(unique(d$conc))
stopifnot(length(grid) == 7L)
stopifnot(all(vapply(by_plant, function(z) identical(sort(z$conc), grid), logical(1))))
plants <- unique(d[c('Plant', 'Type', 'Treatment')])
plant_counts <- with(plants, table(Type, Treatment))
stopifnot(nrow(plants) == 12L, all(dim(plant_counts) == c(2L, 2L)),
          all(plant_counts == 3L))
cat('\nDataset dimensions and concentration values as stored:\n')
print(dim(d)); print(grid)
cat('\nRows per plant: repeated measurements, not independent replicates:\n')
print(table(d$Plant))
cat('\nPlant counts by population and treatment:\n')
print(plant_counts)

# Export the actual installed dataset, not reconstructed or simulated values.
utils::write.csv(x, 'co2_raw.csv', row.names = FALSE)

# Flatten aggregate output so each exported statistic has its own CSV column.
pw <- aggregate(uptake ~ Type + Treatment + conc, d,
  function(v) c(n_plants = length(v), mean_uptake = mean(v), sd_uptake = sd(v),
                minimum_uptake = min(v), maximum_uptake = max(v)))
pointwise <- data.frame(pw[c('Type', 'Treatment', 'conc')],
                        as.data.frame(pw$uptake), check.names = FALSE)
pointwise <- pointwise[order(pointwise$Type, pointwise$Treatment, pointwise$conc), ]
cat('\nPointwise descriptive summaries across plants:\n')
print(pointwise, row.names = FALSE)
utils::write.csv(pointwise, 'co2_pointwise_summaries.csv', row.names = FALSE)

# Each plant contributes one equally weighted mean across measured levels.
pm <- aggregate(uptake ~ Plant + Type + Treatment, d, mean)
names(pm)[names(pm) == 'uptake'] <- 'mean_uptake'
pm <- pm[order(pm$Type, pm$Treatment, pm$Plant), ]
cat('\nPlant-level means across measured concentration levels:\n')
print(pm, row.names = FALSE)
utils::write.csv(pm, 'co2_plant_means.csv', row.names = FALSE)
ps <- aggregate(mean_uptake ~ Type + Treatment, pm,
  function(v) c(n_plants = length(v), mean = mean(v), sd = sd(v),
                minimum = min(v), maximum = max(v)))
plant_summary <- data.frame(ps[c('Type', 'Treatment')],
                            as.data.frame(ps$mean_uptake), check.names = FALSE)
cat('\nDescriptive summaries of plant-level means:\n')
print(plant_summary, row.names = FALSE)
utils::write.csv(plant_summary, 'co2_plant_mean_summaries.csv', row.names = FALSE)

contrast <- do.call(rbind, lapply(sort(unique(pm$Type)), function(tp) {
  z <- pm[pm$Type == tp, ]
  stopifnot(all(c('chilled', 'nonchilled') %in% z$Treatment))
  chilled <- mean(z$mean_uptake[z$Treatment == 'chilled'])
  nonchilled <- mean(z$mean_uptake[z$Treatment == 'nonchilled'])
  data.frame(Type = tp, chilled_mean = chilled, nonchilled_mean = nonchilled,
             chilled_minus_nonchilled = chilled - nonchilled)
}))
cat('\nDescriptive group-mean differences only; not paired differences:\n')
print(contrast, row.names = FALSE)
utils::write.csv(contrast, 'co2_descriptive_contrasts.csv', row.names = FALSE)
cat('\nNo hypothesis tests, confidence intervals, fitted asymptotes, or paired calculations are produced.\n')
cat('Concentration and increasing measurement order cannot be separated by these profiles.\n')
cat('Treatment timing: source describes 14 h daytime chilling at 7 C, then 10 h recovery at 20 C, then measurement at 26 C; R documentation describes overnight chilling.\n')
cat('These are not direct measurements during chilling.\n')

# Figure 1: consistent axes and plant-specific trajectories in each group.
groups <- unique(d[c('Type', 'Treatment')])
groups <- groups[order(groups$Type, groups$Treatment), ]
plant_palette <- c('#376C82', '#A36545', '#647C55')
ink <- '#25313A'
grid_color <- '#E7EBED'
pretty_treatment <- function(v) ifelse(v == 'nonchilled', 'Nonchilled', 'Chilled')
y_limits <- range(d$uptake)
y_pad <- diff(y_limits) * 0.06
if (y_pad == 0) y_pad <- 1
y_limits <- y_limits + c(-y_pad, y_pad)
grDevices::png('co2_response_curves.png', width = 3000, height = 2400,
               res = 300, pointsize = 12, bg = 'white')
par(mfrow = c(2, 2), mar = c(5.2, 5.0, 3.0, 1.0),
    oma = c(2.0, 0, 2.8, 0), col = ink, col.axis = ink,
    col.lab = ink, col.main = ink, family = 'sans', las = 1)
for (i in seq_len(nrow(groups))) {
  z <- d[d$Type == groups$Type[i] & d$Treatment == groups$Treatment[i], ]
  ids <- sort(unique(z$Plant))
  plot(range(d$conc), y_limits, type = 'n', axes = FALSE,
       xlab = '', ylab = '',
       main = paste(groups$Type[i], pretty_treatment(groups$Treatment[i]), sep = ' | '))
  abline(h = pretty(y_limits), col = grid_color, lwd = 0.8)
  axis(1, at = grid, labels = grid, cex.axis = 0.8)
  axis(2, cex.axis = 0.9)
  box(col = '#B8C1C6')
  mtext('CO2 concentration (stored values; units unresolved)', side = 1, line = 3.1, cex = 0.8)
  mtext('Uptake (micromol m^-2 s^-1)', side = 2, line = 3.2, las = 0, cex = 0.85)
  for (j in seq_along(ids)) {
    zz <- z[z$Plant == ids[j], ]; zz <- zz[order(zz$conc), ]
    lines(zz$conc, zz$uptake, type = 'b', pch = c(16, 17, 15)[j],
          col = plant_palette[j], lwd = 1.8, cex = 0.7)
  }
  legend('bottomright', legend = ids, col = plant_palette[seq_along(ids)],
         pch = c(16, 17, 15)[seq_along(ids)], lty = 1, lwd = 1.8,
         bty = 'n', cex = 0.85)
}
mtext('Individual plant uptake profiles', outer = TRUE, side = 3, line = 1.0,
      cex = 1.25, font = 2)
mtext('Lines connect repeated observations; concentration increased in measurement order.',
      outer = TRUE, side = 1, line = 0.6, cex = 0.85)
grDevices::dev.off()

# Figure 2: open a new device only after closing the first one.
mean_limits <- range(pm$mean_uptake)
mean_pad <- diff(mean_limits) * 0.12
if (mean_pad == 0) mean_pad <- 1
mean_limits <- mean_limits + c(-mean_pad, mean_pad)
group_colors <- ifelse(groups$Treatment == 'chilled', '#376C82', '#A36545')
grDevices::png('co2_plant_level_comparison.png', width = 3000, height = 2100,
               res = 300, pointsize = 12, bg = 'white')
par(mar = c(6.5, 5.5, 4.5, 1.5), oma = c(2.0, 0, 0, 0),
    col = ink, col.axis = ink, col.lab = ink, col.main = ink,
    family = 'sans', las = 1)
plot(c(0.5, nrow(groups) + 0.5), mean_limits, type = 'n', axes = FALSE,
     xlab = '', ylab = '', main = 'Plant-level uptake: individuals and group means')
abline(h = pretty(mean_limits), col = grid_color, lwd = 0.8)
axis(2)
axis(1, at = seq_len(nrow(groups)),
     labels = paste(groups$Type, pretty_treatment(groups$Treatment), sep = '\n'),
     tick = FALSE, cex.axis = 0.95)
box(col = '#B8C1C6')
mtext('Plant mean uptake (micromol m^-2 s^-1)', side = 2, line = 3.6, las = 0)
for (i in seq_len(nrow(groups))) {
  z <- pm[pm$Type == groups$Type[i] & pm$Treatment == groups$Treatment[i], ]
  z <- z[order(z$Plant), ]
  offsets <- seq(-0.13, 0.13, length.out = nrow(z))
  points(i + offsets, z$mean_uptake, pch = 21, bg = group_colors[i],
         col = 'white', lwd = 0.8, cex = 1.6)
  text(i + offsets, z$mean_uptake, labels = z$Plant, pos = 3,
       offset = 0.7, cex = 0.8, col = ink)
  segments(i - 0.23, mean(z$mean_uptake), i + 0.23, mean(z$mean_uptake),
           col = ink, lwd = 2.5)
}
mtext('Dots: individual plants   |   Short horizontal lines: group means',
      side = 3, line = 0.7, cex = 0.9)
mtext('Each plant mean weights the seven measured concentration levels equally; no inferential intervals.',
      outer = TRUE, side = 1, line = 0.5, cex = 0.85)
grDevices::dev.off()

cat('\nExported files:\n')
print(c('co2_raw.csv', 'co2_plant_means.csv', 'co2_pointwise_summaries.csv',
        'co2_plant_mean_summaries.csv', 'co2_descriptive_contrasts.csv',
        'co2_response_curves.png', 'co2_plant_level_comparison.png'))
print(utils::sessionInfo())
