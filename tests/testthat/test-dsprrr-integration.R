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

test_that("ProgramSet save and load use exact closed file bundles", {
  root <- withr::local_tempdir()
  path <- file.path(root, "program-set")
  programs <- test_program_set_programs()
  persona_id <- dsprrr::program_artifact_id(programs$personas)
  procedure_ref <- test_governed_procedure_ref(
    "personas",
    persona_id,
    revision_id = "procedure-revision-7"
  )
  program_set <- tempest_program_set(
    programs = programs,
    path = path,
    governed_procedure_refs = list(personas = procedure_ref)
  )
  loaded <- tempest_load_program_set(path)
  manifest <- test_program_set_manifest(path)
  stages <- tempest:::tempest_program_set_stages()

  expect_identical(
    sort(list.files(path, recursive = TRUE)),
    sort(tempest:::tempest_program_set_expected_files())
  )
  expect_identical(program_set@bundle_root, as.character(fs::path_abs(path)))
  expect_identical(loaded@bundle_root, as.character(fs::path_abs(path)))
  expect_identical(manifest$schema_version, 2L)
  expect_identical(
    loaded@entries$personas$governed_procedure_ref,
    tempest:::tempest_governed_procedure_record(procedure_ref)
  )
  for (stage in stages) {
    expect_identical(
      loaded@entries[[stage]]$artifact_reference,
      list(type = "file", path = paste0("programs/", stage, ".rds"))
    )
    expect_identical(
      dsprrr::program_artifact_id(
        tempest:::tempest_program_set_program(loaded, stage)
      ),
      loaded@entries[[stage]]$program_artifact_id
    )
  }
  expect_identical(
    tempest:::tempest_program_set_identity_equal(program_set, loaded),
    TRUE
  )
  changed_identity <- tempest:::tempest_program_set_manifest_programs(loaded)
  changed_identity$personas$evaluator_version <- "2"
  expect_error(
    tempest:::tempest_program_set_identity_equal(loaded, changed_identity),
    class = "tempest_research_manifest_error",
    regexp = "must match its exact ProgramSet"
  )

  copied_path <- file.path(root, "copied-program-set")
  fs::dir_copy(path, copied_path)
  copied <- tempest_load_program_set(copied_path)
  expect_identical(
    tempest:::tempest_program_set_manifest_programs(copied),
    tempest:::tempest_program_set_manifest_programs(loaded)
  )

  builtin <- tempest_program_set()
  saved_builtin <- tempest_save_program_set(
    builtin,
    file.path(root, "saved-builtin")
  )
  expect_identical(
    tempest:::tempest_program_set_identity_equal(builtin, saved_builtin),
    TRUE
  )
  expect_identical(
    saved_builtin@entries[[1]]$artifact_reference$type,
    "file"
  )
  expect_error(
    tempest_save_program_set(loaded, path),
    class = "tempest_program_set_persistence_error",
    regexp = "Refusing to replace"
  )
})

test_that("ProgramSet load rejects inventory and manifest drift", {
  root <- withr::local_tempdir()

  extra_path <- file.path(root, "extra")
  test_program_set(extra_path)
  writeLines("undeclared", file.path(extra_path, "extra.txt"))
  expect_error(
    tempest_load_program_set(extra_path),
    class = "tempest_program_set_persistence_error",
    regexp = "undeclared"
  )

  missing_path <- file.path(root, "missing")
  test_program_set(missing_path)
  unlink(file.path(missing_path, "programs", "personas.rds"))
  expect_error(
    tempest_load_program_set(missing_path),
    class = "tempest_program_set_persistence_error",
    regexp = "incomplete"
  )

  unknown_field_path <- file.path(root, "unknown-field")
  test_program_set(unknown_field_path)
  manifest <- test_program_set_manifest(unknown_field_path)
  manifest$unknown <- "not allowed"
  test_write_program_set_manifest(unknown_field_path, manifest)
  expect_error(
    tempest_load_program_set(unknown_field_path),
    class = "tempest_program_set_persistence_error",
    regexp = "exactly format"
  )

  reordered_path <- file.path(root, "reordered")
  test_program_set(reordered_path)
  manifest <- test_program_set_manifest(reordered_path)
  manifest <- manifest[c("entries", "schema_version", "format")]
  test_write_program_set_manifest(reordered_path, manifest)
  expect_error(
    tempest_load_program_set(reordered_path),
    class = "tempest_program_set_persistence_error"
  )

  reordered_entry_path <- file.path(root, "reordered-entry")
  test_program_set(reordered_entry_path)
  manifest <- test_program_set_manifest(reordered_entry_path)
  manifest$entries$personas <- manifest$entries$personas[
    rev(names(manifest$entries$personas))
  ]
  test_write_program_set_manifest(reordered_entry_path, manifest)
  expect_error(
    tempest_load_program_set(reordered_entry_path),
    class = "tempest_program_set_persistence_error"
  )

  reordered_artifact_path <- file.path(root, "reordered-artifact")
  test_program_set(reordered_artifact_path)
  manifest <- test_program_set_manifest(reordered_artifact_path)
  manifest$entries$personas$artifact_reference <-
    manifest$entries$personas$artifact_reference[
      rev(names(manifest$entries$personas$artifact_reference))
    ]
  test_write_program_set_manifest(reordered_artifact_path, manifest)
  expect_error(
    tempest_load_program_set(reordered_artifact_path),
    class = "tempest_program_set_persistence_error"
  )

  missing_field_path <- file.path(root, "missing-field")
  test_program_set(missing_field_path)
  manifest <- test_program_set_manifest(missing_field_path)
  manifest$entries$personas$evaluator_id <- NULL
  test_write_program_set_manifest(missing_field_path, manifest)
  expect_error(
    tempest_load_program_set(missing_field_path),
    class = "tempest_program_set_persistence_error",
    regexp = "entries are invalid"
  )

  unknown_entry_path <- file.path(root, "unknown-entry-field")
  test_program_set(unknown_entry_path)
  manifest <- test_program_set_manifest(unknown_entry_path)
  manifest$entries$personas$legacy_module_id <- "removed"
  test_write_program_set_manifest(unknown_entry_path, manifest)
  expect_error(
    tempest_load_program_set(unknown_entry_path),
    class = "tempest_program_set_persistence_error",
    regexp = "entries are invalid"
  )

  wrong_type_path <- file.path(root, "wrong-type")
  test_program_set(wrong_type_path)
  manifest <- test_program_set_manifest(wrong_type_path)
  manifest$entries$personas$contract_version <- "1"
  test_write_program_set_manifest(wrong_type_path, manifest)
  expect_error(
    tempest_load_program_set(wrong_type_path),
    class = "tempest_program_set_persistence_error",
    regexp = "entries are invalid"
  )
})

test_that("ProgramSet manifests require exact unique non-null top-level keys", {
  root <- withr::local_tempdir()
  seed_path <- file.path(root, "seed")
  test_program_set(seed_path)
  seed <- test_program_set_manifest(seed_path)
  entries_json <- jsonlite::toJSON(
    seed$entries,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  bundle_with_manifest <- function(name, text) {
    path <- file.path(root, name)
    test_program_set(path)
    writeLines(text, file.path(path, "program-set.json"))
    path
  }

  duplicate <- bundle_with_manifest(
    "duplicate",
    paste0(
      '{"format":"tempest-program-set",',
      '"format":"tempest-program-set",',
      '"schema_version":2,"entries":',
      entries_json,
      "}"
    )
  )
  expect_error(
    tempest_load_program_set(duplicate),
    class = "tempest_program_set_persistence_error",
    regexp = "contain exactly format"
  )

  missing <- bundle_with_manifest(
    "missing",
    '{"format":"tempest-program-set","schema_version":2}'
  )
  expect_error(
    tempest_load_program_set(missing),
    class = "tempest_program_set_persistence_error",
    regexp = "contain exactly format"
  )

  wrong_count <- bundle_with_manifest(
    "wrong-count",
    paste0(
      '{"format":"tempest-program-set","schema_version":2,',
      '"entries":',
      entries_json,
      ',"extra":1}'
    )
  )
  expect_error(
    tempest_load_program_set(wrong_count),
    class = "tempest_program_set_persistence_error",
    regexp = "contain exactly format"
  )

  null_format <- bundle_with_manifest(
    "null-format",
    paste0(
      '{"format":null,"schema_version":2,"entries":',
      entries_json,
      "}"
    )
  )
  expect_error(
    tempest_load_program_set(null_format),
    class = "tempest_program_set_persistence_error",
    regexp = "format is unsupported"
  )

  null_schema <- bundle_with_manifest(
    "null-schema",
    paste0(
      '{"format":"tempest-program-set","schema_version":null,',
      '"entries":',
      entries_json,
      "}"
    )
  )
  expect_error(
    tempest_load_program_set(null_schema),
    class = "tempest_program_set_persistence_error",
    regexp = "schema version is unsupported"
  )

  null_entries <- bundle_with_manifest(
    "null-entries",
    paste0(
      '{"format":"tempest-program-set","schema_version":2,',
      '"entries":null}'
    )
  )
  expect_error(
    tempest_load_program_set(null_entries),
    class = "tempest_program_set_persistence_error",
    regexp = "entries are invalid"
  )

  duplicate_evaluator_entries <- sub(
    '("evaluator_id":"[^"]+")',
    '\\1,"evaluator_id":"duplicate"',
    entries_json
  )
  duplicate_evaluator <- bundle_with_manifest(
    "duplicate-evaluator",
    paste0(
      '{"format":"tempest-program-set","schema_version":2,',
      '"entries":',
      duplicate_evaluator_entries,
      "}"
    )
  )
  expect_error(
    tempest_load_program_set(duplicate_evaluator),
    class = "tempest_program_set_persistence_error",
    regexp = "entries are invalid"
  )
})

test_that("ProgramSet bundles reject declared symbolic links", {
  root <- withr::local_tempdir()
  probe_target <- file.path(root, "probe-target")
  probe_link <- file.path(root, "probe-link")
  writeLines("probe", probe_target)
  can_link <- file.symlink(probe_target, probe_link)
  unlink(probe_link)
  skip_if_not(can_link, "symbolic links are unavailable")

  manifest_path <- file.path(root, "manifest-link")
  test_program_set(manifest_path)
  manifest_target <- file.path(root, "manifest-target.json")
  expect_identical(
    file.rename(
      file.path(manifest_path, "program-set.json"),
      manifest_target
    ),
    TRUE
  )
  expect_identical(
    file.symlink(
      manifest_target,
      file.path(manifest_path, "program-set.json")
    ),
    TRUE
  )
  expect_error(
    tempest_load_program_set(manifest_path),
    class = "tempest_program_set_persistence_error",
    regexp = "symbolic links"
  )

  programs_path <- file.path(root, "programs-link")
  test_program_set(programs_path)
  programs_target <- file.path(root, "programs-target")
  expect_identical(
    file.rename(file.path(programs_path, "programs"), programs_target),
    TRUE
  )
  expect_identical(
    file.symlink(programs_target, file.path(programs_path, "programs")),
    TRUE
  )
  expect_error(
    tempest_load_program_set(programs_path),
    class = "tempest_program_set_persistence_error",
    regexp = "symbolic links"
  )

  artifact_path <- file.path(root, "artifact-link")
  test_program_set(artifact_path)
  artifact <- file.path(artifact_path, "programs", "perspectives.rds")
  artifact_target <- file.path(root, "artifact-target.rds")
  expect_identical(file.rename(artifact, artifact_target), TRUE)
  expect_identical(file.symlink(artifact_target, artifact), TRUE)
  expect_error(
    tempest_load_program_set(artifact_path),
    class = "tempest_program_set_persistence_error",
    regexp = "symbolic links"
  )
})

test_that("ProgramSet declared paths must have regular filesystem types", {
  root <- withr::local_tempdir()

  manifest_path <- file.path(root, "manifest-directory")
  test_program_set(manifest_path)
  unlink(file.path(manifest_path, "program-set.json"))
  dir.create(file.path(manifest_path, "program-set.json"))
  expect_error(
    tempest_load_program_set(manifest_path),
    class = "tempest_program_set_persistence_error",
    regexp = "manifest must be a regular file"
  )

  programs_path <- file.path(root, "programs-file")
  test_program_set(programs_path)
  unlink(file.path(programs_path, "programs"), recursive = TRUE)
  writeLines("not a directory", file.path(programs_path, "programs"))
  expect_error(
    tempest_load_program_set(programs_path),
    class = "tempest_program_set_persistence_error",
    regexp = "programs path must be a directory"
  )

  artifact_path <- file.path(root, "artifact-directory")
  test_program_set(artifact_path)
  artifact <- file.path(artifact_path, "programs", "perspectives.rds")
  unlink(artifact)
  dir.create(artifact)
  expect_error(
    tempest_load_program_set(artifact_path),
    class = "tempest_program_set_persistence_error",
    regexp = "artifact must be a regular file"
  )
})

test_that("ProgramSet load rejects altered and corrupt dsprrr artifacts", {
  root <- withr::local_tempdir()

  altered_path <- file.path(root, "altered")
  test_program_set(altered_path)
  file.copy(
    file.path(altered_path, "programs", "personas.rds"),
    file.path(altered_path, "programs", "perspectives.rds"),
    overwrite = TRUE
  )
  expect_error(
    tempest_load_program_set(altered_path),
    class = "tempest_program_set_verification_error",
    regexp = "identity mismatch"
  )

  corrupt_path <- file.path(root, "corrupt")
  test_program_set(corrupt_path)
  writeLines(
    "not an RDS artifact",
    file.path(corrupt_path, "programs", "perspectives.rds")
  )
  expect_error(
    tempest_load_program_set(corrupt_path),
    class = "tempest_program_set_persistence_error",
    regexp = "Could not load"
  )
})

test_that("registry-backed programs retain live bindings without metadata", {
  root <- withr::local_tempdir()
  path <- file.path(root, "registry-backed")
  programs <- test_program_set_programs()
  credential <- "must-not-enter-program-set-metadata"
  forward <- function(input) {
    credential
    list(output = paste0("bound:", input))
  }
  programs$extract_claims <- dsprrr::module_fn(
    "input -> output",
    forward,
    name = "registry fixture"
  )
  registry <- list("fixture-forward-v1" = forward)
  tempest_program_set(
    programs = programs,
    path = path,
    registry = registry
  )

  manifest_text <- paste(
    readLines(file.path(path, "program-set.json")),
    collapse = "\n"
  )
  expect_no_match(manifest_text, "fixture-forward-v1")
  expect_no_match(manifest_text, credential)
  expect_error(
    tempest_load_program_set(path),
    class = "tempest_program_set_persistence_error",
    regexp = "Could not load"
  )

  loaded <- tempest_load_program_set(path, registry = registry)
  metadata <- tempest:::tempest_program_set_manifest_programs(loaded)
  expect_identical(test_contains_runtime_value(metadata), FALSE)
  rm(registry, forward)
  result <- dsprrr::run(
    tempest:::tempest_program_set_program(loaded, "extract_claims"),
    input = "value"
  )
  expect_identical(result$output, "bound:value")
})

test_that("ProgramSet compilation preserves detached runtime bindings", {
  root <- withr::local_tempdir()
  programs <- test_program_set_programs()
  forward <- function(input) list(output = paste0("bound:", input))
  programs$extract_claims <- dsprrr::module_fn(
    "input -> output",
    forward,
    name = "registry compile fixture"
  )
  registry <- list("fixture-forward-v1" = forward)
  source_path <- file.path(root, "source")
  source_written <- tempest_program_set(
    programs = programs,
    path = source_path,
    registry = registry
  )
  source <- tempest_load_program_set(source_path, registry = registry)
  expect_identical(
    source_written@entries$extract_claims$program_artifact_id,
    source@entries$extract_claims$program_artifact_id
  )
  local_mocked_bindings(
    tempest_program_set_compile_module = function(program, ...) program
  )

  candidate <- tempest_compile_programs(
    source,
    trainsets = list(
      extract_claims = data.frame(input = "a", output = "A")
    ),
    teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 1L),
    path = file.path(root, "candidate"),
    registry = registry
  )

  rm(registry, forward)
  result <- dsprrr::run(
    tempest:::tempest_program_set_program(candidate, "extract_claims"),
    input = "value"
  )
  expect_identical(result$output, "bound:value")
  expect_identical(
    candidate@entries$extract_claims$program_artifact_id,
    source@entries$extract_claims$program_artifact_id
  )
})

test_that("ProgramSet compilation publishes only complete verified results", {
  root <- withr::local_tempdir()
  source <- test_program_set(file.path(root, "source"))
  output <- file.path(root, "compiled")
  trainset <- data.frame(input = c("a", "b"), output = c("A", "B"))
  teleprompter <- dsprrr::LabeledFewShot(k = 1L, seed = 1L)

  compiled <- tempest_compile_programs(
    source,
    trainsets = list(personas = trainset),
    teleprompters = teleprompter,
    path = output
  )
  expect_identical(unname(fs::dir_exists(output)), TRUE)
  expect_identical(
    identical(
      source@entries$personas$program_artifact_id,
      compiled@entries$personas$program_artifact_id
    ),
    FALSE
  )
  unchanged <- setdiff(tempest:::tempest_program_set_stages(), "personas")
  expect_identical(
    vapply(
      source@entries[unchanged],
      `[[`,
      character(1),
      "program_artifact_id"
    ),
    vapply(
      compiled@entries[unchanged],
      `[[`,
      character(1),
      "program_artifact_id"
    )
  )
  expect_identical(
    source@entries$personas$evaluator_id,
    compiled@entries$personas$evaluator_id
  )

  failed_path <- file.path(root, "failed")
  expect_error(
    tempest_compile_programs(
      source,
      trainsets = list(personas = list()),
      teleprompters = teleprompter,
      path = failed_path
    ),
    class = "tempest_program_set_compile_error",
    regexp = "Compilation failed"
  )
  expect_identical(unname(fs::file_exists(failed_path)), FALSE)
  expect_identical(unname(fs::dir_exists(failed_path)), FALSE)

  expect_error(
    tempest_compile_programs(
      source,
      trainsets = list(personas = trainset),
      teleprompters = teleprompter,
      path = file.path(root, "unnamed-args"),
      compile_args = list(personas = list(10L))
    ),
    class = "tempest_program_set_error",
    regexp = "uniquely named"
  )
  expect_error(
    tempest_compile_programs(
      source,
      trainsets = list(personas = trainset),
      teleprompters = teleprompter,
      path = file.path(root, "duplicate-args"),
      compile_args = list(personas = list(trainset = trainset))
    ),
    class = "tempest_program_set_error",
    regexp = "managed arguments"
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

test_that("ProgramSet compilation isolates modules and clears stale governance", {
  root <- withr::local_tempdir()
  programs <- test_program_set_programs()
  governed_stages <- c("perspectives", "personas")
  governed_references <- stats::setNames(
    lapply(
      governed_stages,
      \(stage) {
        test_governed_procedure_ref(
          stage,
          dsprrr::program_artifact_id(programs[[stage]])
        )
      }
    ),
    governed_stages
  )
  source <- tempest_program_set(
    programs = programs,
    path = file.path(root, "source"),
    governed_procedure_refs = governed_references
  )
  baseline_ids <- vapply(
    source@entries,
    `[[`,
    character(1),
    "program_artifact_id"
  )
  baseline_config <- rlang::duplicate(
    source@programs$personas$config,
    shallow = FALSE
  )
  source@programs$personas$state$traces <- list(list(
    prompt = "private baseline prompt",
    response = "private baseline response"
  ))
  source@programs$personas$state$cache <- list(
    private = "private baseline cache"
  )
  baseline_state <- rlang::duplicate(
    source@programs$personas$state,
    shallow = FALSE
  )
  baseline_runtime <- attr(
    source@programs$personas,
    "dsprrr_artifact_runtime",
    exact = TRUE
  )
  compiled_program <- NULL
  local_mocked_bindings(
    tempest_program_set_compile_module = function(program, ...) {
      compiled_program <<- program
      program$config$temperature <- 0.123
      program
    }
  )

  candidate <- tempest_compile_programs(
    source,
    trainsets = list(personas = data.frame(input = "a", output = "A")),
    teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 1L),
    path = file.path(root, "candidate")
  )

  expect_identical(
    identical(compiled_program, source@programs$personas),
    FALSE
  )
  expect_identical(source@programs$personas$config, baseline_config)
  expect_identical(source@programs$personas$state, baseline_state)
  expect_length(compiled_program$state$traces, 0L)
  expect_length(compiled_program$state$cache, 0L)
  expect_identical(
    attr(
      compiled_program,
      "dsprrr_artifact_runtime",
      exact = TRUE
    ),
    baseline_runtime
  )
  expect_identical(
    vapply(
      tempest:::tempest_program_set_programs(source),
      dsprrr::program_artifact_id,
      character(1)
    ),
    baseline_ids
  )
  expect_null(candidate@entries$personas$governed_procedure_ref)
  expect_identical(
    candidate@entries$perspectives$governed_procedure_ref,
    source@entries$perspectives$governed_procedure_ref
  )
  expect_identical(
    identical(
      candidate@entries$personas$program_artifact_id,
      baseline_ids[["personas"]]
    ),
    FALSE
  )

  local_mocked_bindings(
    tempest_program_set_compile_module = function(program, ...) program
  )
  unchanged <- tempest_compile_programs(
    source,
    trainsets = list(personas = data.frame(input = "a", output = "A")),
    teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 1L),
    path = file.path(root, "unchanged")
  )
  expect_identical(
    unchanged@entries$personas$governed_procedure_ref,
    source@entries$personas$governed_procedure_ref
  )
})

test_that("cross-stage compiler mutations are reverified before publication", {
  root <- withr::local_tempdir()
  programs <- test_program_set_programs()
  selected <- c("perspectives", "personas")
  governed_references <- stats::setNames(
    lapply(
      selected,
      \(stage) {
        test_governed_procedure_ref(
          stage,
          dsprrr::program_artifact_id(programs[[stage]])
        )
      }
    ),
    selected
  )
  source <- tempest_program_set(
    programs = programs,
    path = file.path(root, "source"),
    governed_procedure_refs = governed_references
  )
  baseline_ids <- vapply(
    source@entries,
    `[[`,
    character(1),
    "program_artifact_id"
  )
  baseline_configs <- lapply(
    source@programs[selected],
    \(program) rlang::duplicate(program$config, shallow = FALSE)
  )
  first_program <- NULL
  compile_call <- 0L
  mutation <- "changed"
  local_mocked_bindings(
    tempest_program_set_compile_module = function(program, ...) {
      compile_call <<- compile_call + 1L
      if (compile_call == 1L) {
        first_program <<- program
      } else if (identical(mutation, "changed")) {
        first_program$config$temperature <- 0.456
      } else {
        first_program$config$unregistered_runtime <- new.env(
          parent = emptyenv()
        )
      }
      program
    }
  )
  trainsets <- stats::setNames(
    rep(list(data.frame(input = "a", output = "A")), length(selected)),
    selected
  )

  candidate <- tempest_compile_programs(
    source,
    trainsets = trainsets,
    teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 1L),
    path = file.path(root, "candidate")
  )

  expect_null(candidate@entries$perspectives$governed_procedure_ref)
  expect_identical(
    candidate@entries$personas$governed_procedure_ref,
    source@entries$personas$governed_procedure_ref
  )
  expect_identical(
    identical(
      candidate@entries$perspectives$program_artifact_id,
      baseline_ids[["perspectives"]]
    ),
    FALSE
  )

  mutation <- "invalid"
  compile_call <- 0L
  first_program <- NULL
  failed_path <- file.path(root, "failed")
  expect_error(
    tempest_compile_programs(
      source,
      trainsets = trainsets,
      teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 1L),
      path = failed_path
    ),
    class = "tempest_program_set_verification_error",
    regexp = "Could not verify"
  )
  expect_identical(unname(fs::file_exists(failed_path)), FALSE)
  expect_identical(unname(fs::dir_exists(failed_path)), FALSE)
  expect_identical(
    source@programs$perspectives$config,
    baseline_configs$perspectives
  )
  expect_identical(
    source@programs$personas$config,
    baseline_configs$personas
  )
  expect_identical(
    vapply(
      tempest:::tempest_program_set_programs(source),
      dsprrr::program_artifact_id,
      character(1)
    ),
    baseline_ids
  )
})

test_that("throwing mutating compilers cannot alter a baseline ProgramSet", {
  root <- withr::local_tempdir()
  source <- test_program_set(file.path(root, "source"))
  baseline_ids <- vapply(
    source@entries,
    `[[`,
    character(1),
    "program_artifact_id"
  )
  baseline_config <- rlang::duplicate(
    source@programs$personas$config,
    shallow = FALSE
  )
  source@programs$personas$state$traces <- list(list(
    prompt = "private baseline prompt",
    response = "private baseline response"
  ))
  source@programs$personas$state$cache <- list(
    private = "private baseline cache"
  )
  baseline_state <- rlang::duplicate(
    source@programs$personas$state,
    shallow = FALSE
  )
  local_mocked_bindings(
    tempest_program_set_compile_module = function(program, ...) {
      program$config$temperature <- 0.987
      stop("adversarial compiler failure")
    }
  )
  output <- file.path(root, "candidate")

  expect_error(
    tempest_compile_programs(
      source,
      trainsets = list(personas = data.frame(input = "a", output = "A")),
      teleprompters = dsprrr::LabeledFewShot(k = 1L, seed = 1L),
      path = output
    ),
    class = "tempest_program_set_compile_error",
    regexp = "Compilation failed"
  )
  expect_identical(source@programs$personas$config, baseline_config)
  expect_identical(source@programs$personas$state, baseline_state)
  expect_identical(
    vapply(
      tempest:::tempest_program_set_programs(source),
      dsprrr::program_artifact_id,
      character(1)
    ),
    baseline_ids
  )
  expect_identical(unname(fs::file_exists(output)), FALSE)
  expect_identical(unname(fs::dir_exists(output)), FALSE)
})
