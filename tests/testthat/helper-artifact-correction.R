artifact_correction_fixture <- function(.local_envir = parent.frame()) {
  fixture <- storm_product_fixture(.local_envir = .local_envir)
  statement <- "STORM no longer emits stage events."
  source <- fake_source(
    url = "https://example.org/corrected-progress",
    title = "Corrected progress evidence",
    content_text = paste(statement, "The previous report is superseded.")
  )
  fixture$store <- tempest_research_workspace()
  fixture$store$upsert_retrieved_resource(source)
  original_chat <- fixture$config@chat_fn
  fixture$config@chat_fn <- function(role, model, system_prompt, echo) {
    if (identical(role, "expert")) {
      return(fake_chat(
        text = list(paste0(
          statement,
          " [",
          source@resource_id,
          "]."
        ))
      ))
    }
    if (
      identical(role, "judge") &&
        identical(system_prompt, tempest_prompt("fact_extractor_system"))
    ) {
      return(fake_chat_r6(list(
        chat_structured = function(...) {
          list(
            facts = list(list(
              claim = statement,
              sources = list(list(
                source_id = source@resource_id,
                quote = statement
              )),
              confidence = "high"
            ))
          )
        },
        chat = function(...) "",
        register_tools = function(...) invisible(NULL)
      )))
    }
    chat <- original_chat(role, model, system_prompt, echo)
    if (identical(role, "writer")) {
      structured <- chat$chat_structured
      chat$chat_structured <- function(...) {
        value <- structured(...)
        if (is.list(value$items)) {
          value$items <- lapply(value$items, function(item) {
            item$text <- statement
            item
          })
        }
        value
      }
    }
    chat
  }
  fixture$retriever <- tempest_retriever(
    config = fixture$config,
    workspace = fixture$store
  )
  fixture$statement <- statement
  fixture$source <- source
  fixture
}
