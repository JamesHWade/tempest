# Source notes for the CO2 worked example

## R dataset help

The analysis reads the copy of `CO2` shipped with R's `datasets` package. The
official help page describes 84 uptake rows from 12 plants of *Echinochloa
crus-galli*, six from Quebec and six from Mississippi. It says that half of
each origin group was chilled overnight and lists `Plant`, `Type`,
`Treatment`, `conc`, and `uptake` as the data columns.

- [R `datasets::CO2` help page](https://www.stat.ethz.ch/R-manual/R-devel/library/datasets/html/zCO2.html)

## Original study and design

The help page cites Potvin, Lechowicz, and Tardif (1990), “The Statistical
Analysis of Ecophysiological Response Curves Obtained from Experiments
Involving Repeated Measures,” *Ecology* 71(4), 1389–1400,
[doi:10.2307/1938276](https://doi.org/10.2307/1938276). The original article is
available from the authors' lab as a [PDF](https://biology.mcgill.ca/faculty/potvin/articles/PotvinLech_90.pdf).
The authored offline source record summarizes the Methods and Table 1 on
pages 1390–1391; it does not reproduce the paper's analyses or full text.

The Methods describe three plants per population-by-treatment combination,
with control plants paired with chilled plants before treatment. The chilled
plants received 14 hours at 7°C, followed by 10 hours of recovery at 20°C.
Table 1 gives the pairings by table number and reports the CO2 concentrations
as 95, 175, 250, 350, 500, 675, and 1000 µL/L. Its first tabulated value is
95, while the Methods prose describes the lowest level as 100.

The table numbers map to R plant IDs as follows:

| Origin | Matched control and chilled plants |
|---|---|
| Quebec | Qn1–Qc3, Qn2–Qc1, Qn3–Qc2 |
| Mississippi | Mn1–Mc3, Mn2–Mc2, Mn3–Mc1 |

The scripts preserve these pairs in downloadable CSV files. At the highest
concentration, the table's uptake values match the corresponding R dataset
records. The table supports the pairing and concentration labels used here.

## Source discrepancy

The R help page labels `conc` in mL/L. The original article's Table 1 labels
the same recorded concentration values in µL/L, which is also consistent with
the table's study context. The vignette flags this discrepancy and the plot
uses the original Table 1 label. It does not convert the data values. The
small difference between the Table 1 and Methods text for the lowest level
(95 versus 100) is also retained as reported; this analysis uses the dataset's
tabulated value, 95.

## Analysis choices and limits

- At the maximum observed level, each plant contributes one uptake value. The
  treatment contrast is calculated within the six pre-specified pairs, then
  summarized within origin and as an equal-origin average.
- The full response analysis calculates a trapezoidal area for each plant over
  its seven recorded concentration levels. This is an integrated descriptive
  index, not total carbon fixed.
- Both seeded bootstrap intervals resample the three paired differences
  within each origin (20,000 draws; seed `20261114`). With only three pairs per
  origin, the percentile intervals are exploratory and sensitive to individual
  pairs.
- The response curves are descriptive. This worked example does not reproduce
  the paper's repeated-measures model comparisons, make a causal claim, or
  generalize beyond these plants.
- The greenhouse image is AI-generated conceptual artwork. It is not a data
  visualization, a record of the original setup, or scientific evidence.
