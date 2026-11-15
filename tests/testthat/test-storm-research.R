test_that("claim extraction receives bounded untrusted source excerpts", {
  store <- test_research_workspace()
  source_bodies <- lapply(seq_len(5L), function(index) {
    paste0(
      "SOURCE-",
      index,
      "-START|",
      strrep(as.character(index), 30050L),
      "|SOURCE-",
      index,
      "-TAIL"
    )
  })
  sources <- lapply(seq_len(5L), function(index) {
    if (index == 2L) {
      tempest_resource(
        resource_kind = "web",
        locator = paste0("https://example.org/", index),
        title = paste("Example", index),
        media_type = "text/html",
        metadata = list(snippet = "CAPTURED_SNIPPET_ONLY_SOURCE_2")
      )
    } else {
      tempest_resource(
        resource_kind = "web",
        locator = paste0("https://example.org/", index),
        title = paste("Example", index),
        media_type = "text/html",
        content = source_bodies[[index]]
      )
    }
  })
  for (source in sources) {
    store$upsert_retrieved_resource(source)
  }
  source_ids <- vapply(sources, \(source) source@resource_id, character(1))
  module <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims",
    trace_context = list(stage = "extract_claims")
  )
  captured_inputs <- NULL
  local_mocked_bindings(
    tempest_run_dsprrr_module_structured = function(
      module,
      chat,
      inputs,
      step
    ) {
      captured_inputs <<- inputs
      structure(list(output = list(facts = list())), class = "dsprrr_result")
    }
  )

  answer_text <- paste(
    "ANSWER_ONLY_MUST_NOT_APPEAR_AS_SOURCE",
    paste(source_ids, collapse = " ")
  )
  tempest:::tempest_extract_facts_from_answer(
    chat = NULL,
    answer_text = answer_text,
    store = store,
    module = module,
    source_ids = source_ids
  )

  source_context <- captured_inputs$source_context
  expect_match(source_context, "untrusted data", fixed = TRUE)
  expect_match(source_context, source_ids[[1L]], fixed = TRUE)
  expect_match(source_context, source_ids[[2L]], fixed = TRUE)
  expect_match(
    source_context,
    substr(source_bodies[[1L]], 1L, 30000L),
    fixed = TRUE
  )
  expect_match(
    source_context,
    "CAPTURED_SNIPPET_ONLY_SOURCE_2",
    fixed = TRUE
  )
  expect_match(
    source_context,
    substr(source_bodies[[3L]], 1L, 30000L),
    fixed = TRUE
  )
  expect_match(
    source_context,
    substr(source_bodies[[4L]], 1L, 30000L),
    fixed = TRUE
  )
  expected_fifth_excerpt_length <- 120000L -
    3L * 30000L -
    nchar("CAPTURED_SNIPPET_ONLY_SOURCE_2", type = "chars")
  expect_match(
    source_context,
    substr(source_bodies[[5L]], 1L, expected_fifth_excerpt_length),
    fixed = TRUE
  )
  expect_identical(
    grepl("SOURCE-1-TAIL", source_context, fixed = TRUE),
    FALSE
  )
  expect_identical(
    grepl("SOURCE-5-TAIL", source_context, fixed = TRUE),
    FALSE
  )
  expect_identical(captured_inputs$answer_text, answer_text)
  expect_identical(
    grepl(
      "ANSWER_ONLY_MUST_NOT_APPEAR_AS_SOURCE",
      source_context,
      fixed = TRUE
    ),
    FALSE
  )
})
