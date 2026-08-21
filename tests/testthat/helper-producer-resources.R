producer_test_workspace <- function(n = 1L) {
  workspace <- test_research_workspace()
  for (index in seq_len(n)) {
    workspace$upsert_retrieved_resource(test_typed_web_resource(
      url = paste0("https://example.org/", index),
      title = paste("Example", index),
      content = paste("Body text for source", index)
    ))
  }
  workspace
}
