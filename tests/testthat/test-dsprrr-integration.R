test_that("tempest_has_dsprrr returns logical", {
  result <- tempest:::tempest_has_dsprrr()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("tempest_make_dsprrr_modules returns NULL without dsprrr", {
  skip_if(
    tempest:::tempest_has_dsprrr(),
    "dsprrr is installed, skipping absence test"
  )

  cfg <- tempest_config()
  result <- tempest:::tempest_make_dsprrr_modules(cfg)
  expect_null(result)
})

test_that("tempest_make_dsprrr_modules creates modules when available", {
  skip_if_not_installed("dsprrr")

  cfg <- tempest_config()
  result <- tempest:::tempest_make_dsprrr_modules(cfg)

  expect_type(result, "list")
  expect_contains(
    names(result),
    c(
      "perspectives",
      "personas",
      "query_decomposition",
      "fact_extraction",
      "next_question",
      "draft_outline",
      "refined_outline",
      "section_writing",
      "lead_section"
    )
  )
})

test_that("tempest_optimize_dsprrr_modules compiles named modules", {
  skip_if_not_installed("dsprrr")

  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  skip_if(is.null(modules), "dsprrr modules unavailable")

  train <- data.frame(
    question = c(
      "What are battery recycling bottlenecks?",
      "How are EV batteries regulated?"
    ),
    topic = c("lithium batteries", "lithium batteries"),
    stringsAsFactors = FALSE
  )
  train$queries <- I(list(
    c("lithium battery recycling bottlenecks", "EV battery recycling capacity"),
    c("EV battery regulation policy", "lithium battery safety regulation")
  ))

  optimized <- tempest_optimize_dsprrr_modules(
    trainsets = list(query_decomposition = train),
    modules = modules,
    verbose = FALSE
  )

  expect_true(optimized$query_decomposition$is_compiled())
  meta <- attr(optimized, "tempest_dsprrr_optimization")
  expect_equal(meta$query_decomposition$n_train, 2L)
  expect_true(meta$query_decomposition$compiled)
})

test_that("compiled dsprrr module sets can be saved and loaded", {
  skip_if_not_installed("dsprrr")

  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  skip_if(is.null(modules), "dsprrr modules unavailable")
  path <- withr::local_tempdir()

  saved_path <- tempest_save_dsprrr_modules(modules, path)
  expect_true(file.exists(saved_path))

  loaded <- tempest_load_dsprrr_modules(path)
  expect_named(loaded, names(modules))
})

test_that("tempest_run_dsprrr_module ignores missing module", {
  result <- tempest:::tempest_run_dsprrr_module(
    module = NULL,
    chat = NULL,
    inputs = list(question = "What changed?", topic = "Topic"),
    step = "test"
  )
  expect_null(result)
})

test_that("dsprrr query output normalizes to bounded character queries", {
  result <- tempest:::tempest_normalize_query_decomposition(
    list(
      queries = list(
        " alpha ",
        "beta",
        "alpha",
        "",
        NA_character_,
        "gamma",
        "delta",
        "epsilon"
      )
    ),
    fallback = "fallback"
  )

  expect_equal(result$queries, c("alpha", "beta", "gamma", "delta"))
})

test_that("perspective output normalizes and falls back", {
  result <- tempest:::tempest_normalize_perspectives(
    list(
      title = "Research title",
      perspectives = list(
        list(
          name = "Policy",
          description = "Rules",
          key_questions = list("Q1", "Q2")
        ),
        list(name = "Technical", description = "Systems", key_questions = "Q3")
      )
    ),
    topic = "Topic",
    n_experts = 1
  )

  expect_equal(result$title, "Research title")
  expect_equal(length(result$perspectives), 1)
  expect_equal(result$perspectives[[1]]$key_questions, c("Q1", "Q2"))

  fallback <- tempest:::tempest_normalize_perspectives(NULL, topic = "Topic")
  expect_equal(fallback$perspectives[[1]]$name, "Overview")
})

test_that("outline output normalizes nested subsections", {
  outline <- tempest:::tempest_normalize_outline(
    list(
      title = "Title",
      sections = list(
        list(
          title = "Section",
          summary = "Summary",
          subsections = list(
            list(
              title = "Subsection",
              bullets = list("A", "B"),
              needed = list("C")
            )
          )
        )
      )
    ),
    fallback_title = "Fallback"
  )

  expect_equal(outline$title, "Title")
  expect_equal(outline$sections[[1]]$subsections[[1]]$bullets, c("A", "B"))
  expect_equal(outline$sections[[1]]$subsections[[1]]$needed, "C")
})

test_that("fact output normalizes source_ids shorthand", {
  facts <- tempest:::tempest_normalize_fact_output(list(
    facts = list(
      list(
        claim = "Claim",
        source_ids = c("Sabc", "Sdef"),
        confidence = "high"
      )
    )
  ))

  expect_equal(facts[[1]]$claim, "Claim")
  expect_equal(
    vapply(facts[[1]]$sources, function(x) x$source_id, character(1)),
    c("Sabc", "Sdef")
  )
})
