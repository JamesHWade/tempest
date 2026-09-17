test_artifact_knowledge_input <- function(
  text = "The pilot recovered 82%.",
  revision = "v1"
) {
  contents <- list(
    "claim:pilot" = paste0("statement_text: ", text, "\nstatus: active"),
    "source:pilot" = paste0("excerpt: ", text)
  )
  refs <- lapply(names(contents), function(id) {
    list(
      record_id = id,
      revision_id = revision,
      class = if (startsWith(id, "claim")) "Claim" else "Source",
      sha256 = digest::digest(
        charToRaw(contents[[id]]),
        algo = "sha256",
        serialize = FALSE
      ),
      dependencies = if (startsWith(id, "claim")) {
        list(list(record_id = "source:pilot", revision_id = revision))
      } else {
        list()
      }
    )
  })
  list(
    selection = list(
      selection_id = paste0("selection:", revision),
      purpose = "briefing",
      records = refs,
      provenance = list(producer = "offline-test")
    ),
    contents = contents
  )
}
