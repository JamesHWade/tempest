test_enriched_plan <- function(source_id, analyses = TRUE) {
  list(
    title = "A reproducible research brief",
    summary = "An authored interpretation with explicit limitations.",
    analyses = if (analyses) {
      list(list(
        id = "comparison",
        title = "Measured comparison",
        question = "What do these measurements show?",
        code = "print(mean(1:3))",
        source_ids = source_id,
        assumptions = "Illustrative calculation, not a new observation."
      ))
    } else {
      list()
    },
    illustrations = list()
  )
}

test_enriched_executor <- function(task, directory) {
  write.csv(
    data.frame(value = 1:3),
    file.path(directory, "values.csv"),
    row.names = FALSE
  )
  list(
    output = "[1] 2",
    files = "values.csv",
    metadata = list(engine = "offline-fixture")
  )
}
