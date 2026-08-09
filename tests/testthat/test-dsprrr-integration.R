test_that("tempest_make_dsprrr_modules creates modules", {
  cfg <- tempest_config()
  result <- tempest:::tempest_make_dsprrr_modules(cfg)

  expect_type(result, "list")
  expect_contains(
    names(result),
    c(
      "perspectives",
      "personas",
      "query_decomposition",
      "extract_claims",
      "verify_claim_support",
      "next_question",
      "draft_outline",
      "refined_outline",
      "section_writing",
      "lead_section"
    )
  )
})

test_that("tempest_optimize_dsprrr_modules compiles named modules", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())

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

  expect_identical(optimized$query_decomposition$is_compiled(), TRUE)
  meta <- attr(optimized, "tempest_dsprrr_optimization")
  expect_equal(meta$query_decomposition$n_train, 2L)
  expect_identical(meta$query_decomposition$compiled, TRUE)
  expect_s3_class(
    meta$query_decomposition$summary,
    "dsprrr_optimization_summary"
  )
})

test_that("dsprrr module sets use versioned program artifact bundles", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  path <- file.path(withr::local_tempdir(), "programs.rds")

  saved_path <- tempest_save_dsprrr_modules(modules, path)
  expect_equal(file.exists(saved_path), TRUE)
  bundle <- readRDS(saved_path)
  expect_s3_class(bundle, "tempest_dsprrr_program_bundle")
  expect_identical(bundle$bundle_type, "tempest_dsprrr_programs")
  expect_identical(bundle$schema_version, 1L)
  expect_named(bundle$programs, names(modules))
  expect_s3_class(bundle$programs[[1]], "dsprrr_program_artifact")

  loaded <- tempest_load_dsprrr_modules(path)
  expect_named(loaded, names(modules))
  expect_r6_class(loaded$query_decomposition, "Module")
})

test_that("dsprrr program bundles preserve compiled module state", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  train <- data.frame(question = "Why?", topic = "Topic")
  train$queries <- I(list(c("topic evidence", "topic sources")))
  optimized <- tempest_optimize_dsprrr_modules(
    trainsets = list(query_decomposition = train),
    modules = modules,
    verbose = FALSE
  )
  path <- withr::local_tempfile(fileext = ".rds")

  tempest_save_dsprrr_modules(optimized, path)
  loaded <- tempest_load_dsprrr_modules(path)

  expect_identical(loaded$query_decomposition$is_compiled(), TRUE)
  expect_length(loaded$query_decomposition$demos, 1L)
})

test_that("dsprrr program bundles reject modified artifacts", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  path <- file.path(withr::local_tempdir(), "programs.rds")
  tempest_save_dsprrr_modules(modules, path)

  bundle <- readRDS(path)
  bundle$programs[[1]]$format_version <- 999L
  saveRDS(bundle, path)

  expect_error(
    tempest_load_dsprrr_modules(path),
    class = "tempest_dsprrr_bundle_error"
  )
})

test_that("dsprrr program bundles replace only validated bundles", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  root <- withr::local_tempdir()
  path <- file.path(root, "programs.rds")
  tempest_save_dsprrr_modules(modules, path)

  expect_error(
    tempest_save_dsprrr_modules(modules, path),
    class = "tempest_dsprrr_bundle_error"
  )
  expect_no_error(
    tempest_save_dsprrr_modules(modules, path, overwrite = TRUE)
  )

  unrelated <- file.path(root, "unrelated.rds")
  saveRDS(list(note = "keep"), unrelated)
  expect_error(
    tempest_save_dsprrr_modules(modules, unrelated, overwrite = TRUE),
    class = "tempest_dsprrr_bundle_error"
  )
  expect_identical(readRDS(unrelated), list(note = "keep"))
})

test_that("dsprrr compile args merge defaults with module overrides", {
  args <- tempest:::tempest_select_dsprrr_compile_args(
    list(
      .default = list(.cache = FALSE, shared = "default"),
      extract_claims = list(shared = "module", runner = "runner")
    ),
    "extract_claims"
  )

  expect_identical(
    args,
    list(.cache = FALSE, shared = "module", runner = "runner")
  )
  expect_error(
    tempest:::tempest_select_dsprrr_compile_args(
      list(.default = list(program = "invalid")),
      "extract_claims"
    ),
    class = "tempest_dsprrr_optimization_error"
  )
})

test_that("dsprrr optimization rejects misspelled teleprompter names", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  train <- data.frame(question = "Why?", topic = "Topic")
  train$queries <- list(c("topic evidence", "topic sources"))

  expect_error(
    tempest_optimize_dsprrr_modules(
      trainsets = list(query_decomposition = train),
      modules = modules,
      teleprompter = list(
        query_decompostion = dsprrr::LabeledFewShot(k = 1L)
      ),
      verbose = FALSE
    ),
    class = "tempest_dsprrr_optimization_error"
  )
})

test_that("dsprrr optimization rejects misspelled compile argument names", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  train <- data.frame(question = "Why?", topic = "Topic")
  train$queries <- list(c("topic evidence", "topic sources"))

  expect_error(
    tempest_optimize_dsprrr_modules(
      trainsets = list(query_decomposition = train),
      modules = modules,
      compile_args = list(query_decompostion = list(.cache = FALSE)),
      verbose = FALSE
    ),
    class = "tempest_dsprrr_optimization_error"
  )
})

test_that("dsprrr program bundle paths are explicit", {
  modules <- tempest:::tempest_make_dsprrr_modules(tempest_config())
  path <- file.path(withr::local_tempdir(), "programs")

  expect_error(
    tempest_save_dsprrr_modules(modules, path),
    class = "tempest_dsprrr_bundle_error"
  )
  expect_equal(file.exists(path), FALSE)
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
      title = c("Title", "Extra"),
      sections = list(
        list(
          title = c("Section", "Variant"),
          summary = c("Summary", "More detail"),
          subsections = list(
            list(
              title = c("Subsection", "Variant"),
              bullets = list("A", "B"),
              needed = list("C")
            )
          )
        )
      )
    ),
    fallback_title = "Fallback"
  )

  expect_equal(outline$title, "Title Extra")
  expect_equal(outline$sections[[1]]$title, "Section Variant")
  expect_equal(outline$sections[[1]]$summary, "Summary More detail")
  expect_equal(
    outline$sections[[1]]$subsections[[1]]$title,
    "Subsection Variant"
  )
  expect_equal(outline$sections[[1]]$subsections[[1]]$bullets, c("A", "B"))
  expect_equal(outline$sections[[1]]$subsections[[1]]$needed, "C")
})

test_that("fact output normalizes source_ids shorthand", {
  facts <- tempest:::tempest_normalize_fact_output(list(
    facts = list(
      list(
        claim = "Claim",
        source_ids = c("Sabc", "Sdef"),
        confidence = "high",
        support_score = 0.84
      )
    )
  ))

  expect_equal(facts[[1]]$claim, "Claim")
  expect_equal(facts[[1]]$support_score, 0.84)
  expect_equal(
    vapply(facts[[1]]$sources, function(x) x$source_id, character(1)),
    c("Sabc", "Sdef")
  )
})

test_that("fact output clamps invalid support scores and leaves missing explicit", {
  facts <- tempest:::tempest_normalize_fact_output(list(
    facts = list(
      list(claim = "High", source_ids = "Sabc", score = 2),
      list(claim = "Missing", source_ids = "Sdef")
    )
  ))

  expect_equal(facts[[1]]$support_score, 1)
  expect_equal(facts[[2]]$support_score, NA_real_)
})
