# Recreate the authored offline STORM product and its enriched report.
# Usage: Rscript run-example.R [new-report-directory]

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
example_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1L]])))
} else {
  system.file("examples", "enriched-research", package = "tempest")
}
if (!nzchar(example_dir) || !file.exists(file.path(example_dir, "fixture.R"))) {
  stop("Run this script from an installed Tempest package or the source tree.")
}

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) {
  args[[1L]]
} else {
  file.path(tempdir(), "tempest-co2-enriched-research")
}

source(file.path(example_dir, "fixture.R"), local = TRUE)
source(file.path(example_dir, "plan.R"), local = TRUE)
research <- tempest_co2_replay_research()
plan <- tempest_co2_enrichment_plan(research, example_dir)

image_path <- file.path(example_dir, "greenhouse-illustration.png")
image_generator <- function(task, directory) {
  if (!file.copy(image_path, file.path(directory, "greenhouse.png"))) {
    stop("Could not copy the authored illustration into the task directory.")
  }
  list(
    file = "greenhouse.png",
    metadata = list(
      provider = "OpenAI image generation tool",
      model = "not exposed by host",
      generated_at = "2026-11-15"
    )
  )
}

report <- tempest::tempest_enrich_report(
  research,
  output_dir,
  plan = plan,
  executor = tempest::tempest_r_executor(timeout = 120),
  image_generator = image_generator,
  allow_code = TRUE,
  max_analyses = 2L,
  max_illustrations = 1L
)
verified <- tempest::tempest_read_enriched_report(output_dir)
stopifnot(identical(report@status, verified@status))
stopifnot(identical(verified@status, "complete"))
cat("Authored offline replay; no live model or search evaluation.\n")
cat("Verified enriched report:", verified@path, "\n")
cat("Status:", verified@status, "\n")
