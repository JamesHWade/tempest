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

test_that("dsprrr structured results cross as canonical JSON records", {
  execution <- tempest:::tempest_program_set_execution(
    tempest_program_set(),
    "extract_claims"
  )
  provider_output <- list(
    facts = tibble::tibble(
      claim = "A captured claim.",
      sources = list(tibble::tibble(
        source_id = "S123456789abc",
        url = "https://example.org/source",
        quote = "captured claim"
      )),
      confidence = factor("high", levels = c("low", "medium", "high")),
      support_score = 1,
      note = NA_character_
    )
  )
  local_mocked_bindings(
    tempest_dsprrr_run = function(...) {
      structure(
        list(
          output = provider_output,
          metadata = list(
            program_artifact_id = execution$program_artifact_id,
            trace_context = execution$trace_context
          )
        ),
        class = "dsprrr_result"
      )
    }
  )

  result <- tempest:::tempest_run_dsprrr_module_structured(
    execution,
    chat = NULL,
    inputs = list(),
    step = "extract_claims"
  )

  expect_type(result$output$facts, "list")
  expect_null(attr(result$output$facts, "class", exact = TRUE))
  expect_named(result$output$facts, NULL)
  expect_named(
    result$output$facts[[1L]],
    c("claim", "sources", "confidence", "support_score")
  )
  expect_identical(result$output$facts[[1L]]$confidence, "high")
  expect_type(result$output$facts[[1L]]$sources, "list")
  expect_null(
    attr(result$output$facts[[1L]]$sources[[1L]], "class", exact = TRUE)
  )
})
test_that("tempest_run_dsprrr_module rejects a missing bound module", {
  expect_error(
    tempest:::tempest_run_dsprrr_module(
      module = NULL,
      chat = NULL,
      inputs = list(question = "What changed?", topic = "Topic"),
      step = "query_decomposition"
    ),
    class = "tempest_ecosystem_contract_error"
  )
})

test_that("perspective output requires the exact requested batch", {
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
    n_experts = 2
  )

  expect_equal(result$title, "Research title")
  expect_equal(length(result$perspectives), 2)
  expect_equal(result$perspectives[[1]]$key_questions, c("Q1", "Q2"))

  expect_error(
    tempest:::tempest_normalize_perspectives(NULL, topic = "Topic"),
    class = "tempest_stage_output_error"
  )
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
    )
  )

  expect_equal(outline$title, "Title")
  expect_equal(outline$sections[[1]]$title, "Section")
  expect_equal(outline$sections[[1]]$summary, "Summary")
  expect_equal(
    outline$sections[[1]]$subsections[[1]]$title,
    "Subsection"
  )
  expect_equal(outline$sections[[1]]$subsections[[1]]$bullets, c("A", "B"))
  expect_equal(outline$sections[[1]]$subsections[[1]]$needed, "C")
})

test_that("builtin ProgramSets expose the exact portable stage contract", {
  program_set <- tempest_program_set()
  stages <- tempest:::tempest_program_set_stages()
  entries <- tempest:::tempest_program_set_entries(program_set)

  expect_identical(
    S7::S7_inherits(program_set, tempest:::TempestProgramSet),
    TRUE
  )
  expect_identical(program_set@schema_version, 2L)
  expect_identical(names(entries), stages)
  expect_length(program_set@bundle_root, 0L)
  for (stage in stages) {
    expect_named(
      entries[[stage]],
      c(
        "stage",
        "contract_version",
        "program_artifact_id",
        "artifact_reference",
        "governed_procedure_ref",
        "evaluator_id",
        "evaluator_version"
      )
    )
    expect_identical(entries[[stage]]$stage, stage)
    expect_identical(entries[[stage]]$contract_version, 1L)
    expect_match(
      entries[[stage]]$program_artifact_id,
      "^sha256:[a-f0-9]{64}$"
    )
    expect_identical(
      entries[[stage]]$artifact_reference,
      list(type = "builtin", id = paste0("tempest::", stage))
    )
    expect_identical(
      entries[[stage]]$evaluator_id,
      paste0("tempest::evaluator/", stage)
    )
    expect_identical(entries[[stage]]$evaluator_version, "1")
    expect_s3_class(
      tempest:::tempest_program_set_program(program_set, stage),
      "Module"
    )
  }

  manifest_programs <- tempest:::tempest_program_set_manifest_programs(
    program_set
  )
  expect_identical(names(manifest_programs), sort(stages))
  expect_identical(test_contains_runtime_value(manifest_programs), FALSE)
  expect_identical(
    names(tempest:::tempest_program_set_identity_references(program_set)[[1]]),
    c(
      "stage",
      "contract_version",
      "program_artifact_id",
      "governed_procedure_ref",
      "evaluator_id",
      "evaluator_version"
    )
  )
})

test_that("ProgramSet construction rejects partial and ambiguous contracts", {
  programs <- test_program_set_programs()

  expect_error(
    tempest_program_set(programs = programs),
    class = "tempest_program_set_error",
    regexp = "path.*required"
  )
  expect_error(
    tempest_program_set(contract_versions = 1.2),
    class = "tempest_program_set_error",
    regexp = "contract_versions"
  )
  expect_error(
    tempest_program_set(
      programs = programs[-1],
      path = file.path(withr::local_tempdir(), "partial")
    ),
    class = "tempest_program_set_error",
    regexp = "exact Tempest stages"
  )
  expect_error(
    tempest_program_set(
      evaluators = list(
        perspectives = list(
          evaluator_id = "test",
          evaluator_version = "1"
        )
      )
    ),
    class = "tempest_program_set_error",
    regexp = "exact Tempest stages"
  )

  duplicate_evaluators <- tempest:::tempest_program_set_default_evaluators()
  duplicate_evaluators$personas <- structure(
    list("first", "1", "second"),
    names = c("evaluator_id", "evaluator_version", "evaluator_id")
  )
  expect_error(
    tempest_program_set(evaluators = duplicate_evaluators),
    class = "tempest_program_set_error",
    regexp = "contain exactly.*evaluator_id.*evaluator_version"
  )

  builtin <- tempest_program_set()
  expect_error(
    tempest:::TempestProgramSet(
      schema_version = 2L,
      bundle_root = "relative/program-set",
      entries = builtin@entries,
      programs = builtin@programs
    ),
    class = "simpleError",
    regexp = "absolute directory path"
  )

  evaluators <- tempest:::tempest_program_set_default_evaluators()
  evaluators <- lapply(
    evaluators,
    \(evaluator) evaluator[c("evaluator_version", "evaluator_id")]
  )
  reordered <- tempest_program_set(evaluators = evaluators)
  expect_named(
    reordered@entries$personas,
    c(
      "stage",
      "contract_version",
      "program_artifact_id",
      "artifact_reference",
      "governed_procedure_ref",
      "evaluator_id",
      "evaluator_version"
    )
  )
})

test_that("ProgramSet access fails closed after live module mutation", {
  program_set <- tempest_program_set()
  program <- program_set@programs$personas
  program$config <- c(program$config, list(test_mutation = "changed"))

  expect_error(
    tempest:::tempest_program_set_program(program_set, "personas"),
    class = "tempest_program_set_verification_error",
    regexp = "identity changed"
  )
  expect_error(
    tempest:::tempest_program_set_manifest_programs(program_set),
    class = "tempest_program_set_verification_error",
    regexp = "identity changed"
  )
})

test_that("single-stage ProgramSet access avoids whole-set revalidation", {
  program_set <- tempest_program_set()
  expected_entry <- program_set@entries$personas

  local_mocked_bindings(
    tempest_program_set_assert = function(...) {
      stop("whole-set validation reached", call. = FALSE)
    }
  )

  expect_identical(
    tempest:::tempest_program_set_entry(program_set, "personas"),
    expected_entry
  )
  expect_identical(
    tempest:::tempest_program_set_program(program_set, "personas"),
    program_set@programs$personas
  )
  expect_error(
    tempest:::tempest_program_set_programs(program_set),
    regexp = "whole-set validation reached"
  )
})

test_that("single-stage ProgramSet access checks file-backed inventory", {
  root <- file.path(withr::local_tempdir(), "program-set")
  file_backed <- tempest_program_set(path = root)
  writeLines("undeclared", file.path(root, "rogue.txt"))

  expect_error(
    tempest:::tempest_program_set_entry(file_backed, "personas"),
    class = "tempest_program_set_verification_error",
    regexp = "failed live stage validation"
  )
  expect_error(
    tempest:::tempest_program_set_program(file_backed, "personas"),
    class = "tempest_program_set_verification_error",
    regexp = "failed live stage validation"
  )
})

test_that("ProgramSet publication rolls back invalid governed bindings", {
  root <- withr::local_tempdir()
  programs <- test_program_set_programs()
  governed_reference <- test_governed_procedure_ref(
    "personas",
    dsprrr::program_artifact_id(programs$personas)
  )
  source <- tempest_program_set(
    programs = programs,
    path = file.path(root, "source"),
    governed_procedure_refs = list(personas = governed_reference)
  )
  candidate_programs <- tempest:::tempest_program_set_programs(source)
  candidate_programs$personas <- candidate_programs$personas$copy(deep = TRUE)
  candidate_programs$personas$config$temperature <- 0.321
  metadata <- tempest:::tempest_program_set_metadata(source)
  output <- file.path(root, "candidate")

  expect_error(
    tempest:::tempest_program_set_write_bundle(
      candidate_programs,
      output,
      metadata$contract_versions,
      metadata$evaluators,
      metadata$governed_references
    ),
    class = "tempest_research_manifest_error",
    regexp = "governed_procedure_ref.*must match"
  )
  expect_identical(unname(fs::file_exists(output)), FALSE)
  expect_identical(unname(fs::dir_exists(output)), FALSE)
})
