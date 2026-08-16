# Governed dsprrr program sets ----------------------------------------------

tempest_program_set_abort <- function(
  message,
  ...,
  parent = NULL,
  class = NULL
) {
  tempest_abort(
    message,
    ...,
    class = c(class, "tempest_program_set_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_program_set_stages <- function() {
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
}

tempest_program_set_string <- function(value, arg) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(tempest_trim(value))
  ) {
    tempest_program_set_abort(
      "{.arg {arg}} must be a single non-empty string."
    )
  }
  tempest_trim(value)
}

tempest_program_set_named_list <- function(value, arg, names = NULL) {
  if (
    !is.list(value) ||
      is.data.frame(value) ||
      is.null(base::names(value)) ||
      anyNA(base::names(value)) ||
      any(!nzchar(base::names(value))) ||
      anyDuplicated(base::names(value))
  ) {
    tempest_program_set_abort(
      "{.arg {arg}} must be a uniquely named list."
    )
  }
  if (!is.null(names) && !setequal(base::names(value), names)) {
    missing <- setdiff(names, base::names(value))
    extra <- setdiff(base::names(value), names)
    details <- c(
      if (length(missing) > 0L) {
        paste0("missing: ", paste(missing, collapse = ", "))
      },
      if (length(extra) > 0L) paste0("unknown: ", paste(extra, collapse = ", "))
    )
    tempest_program_set_abort(
      "{.arg {arg}} must contain the exact Tempest stages ({details})."
    )
  }
  if (!is.null(names)) {
    value <- value[names]
  }
  value
}

tempest_program_set_contract_versions <- function(value) {
  stages <- tempest_program_set_stages()
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value == 1L
  ) {
    value <- rep(as.integer(value), length(stages))
    names(value) <- stages
  }
  if (
    !is.numeric(value) ||
      length(value) != length(stages) ||
      is.null(names(value)) ||
      !setequal(names(value), stages)
  ) {
    tempest_program_set_abort(
      paste0(
        "{.arg contract_versions} must be `1` or an exact named integer ",
        "vector for all Tempest stages."
      )
    )
  }
  value <- value[stages]
  if (anyNA(value) || any(!is.finite(value)) || any(value != 1L)) {
    tempest_program_set_abort(
      "{.arg contract_versions} only supports contract version `1`."
    )
  }
  stats::setNames(as.integer(value), stages)
}

tempest_program_set_default_evaluators <- function() {
  stages <- tempest_program_set_stages()
  stats::setNames(
    lapply(
      stages,
      \(stage) {
        list(
          evaluator_id = paste0("tempest::evaluator/", stage),
          evaluator_version = "1"
        )
      }
    ),
    stages
  )
}

tempest_program_set_evaluators <- function(value) {
  stages <- tempest_program_set_stages()
  if (is.null(value)) {
    return(tempest_program_set_default_evaluators())
  }
  value <- tempest_program_set_named_list(value, "evaluators", stages)
  stats::setNames(
    lapply(
      stages,
      \(stage) {
        evaluator <- value[[stage]]
        fields <- c("evaluator_id", "evaluator_version")
        evaluator_names <- names(evaluator)
        if (
          !is.list(evaluator) ||
            is.data.frame(evaluator) ||
            length(evaluator) != length(fields) ||
            is.null(evaluator_names) ||
            anyNA(evaluator_names) ||
            any(!nzchar(evaluator_names)) ||
            anyDuplicated(evaluator_names) ||
            !setequal(evaluator_names, fields)
        ) {
          tempest_program_set_abort(
            paste0(
              "{.arg evaluators[[stage]]} must contain exactly ",
              "{.field evaluator_id} and {.field evaluator_version}."
            )
          )
        }
        list(
          evaluator_id = tempest_program_set_string(
            evaluator$evaluator_id,
            paste0("evaluators$", stage, "$evaluator_id")
          ),
          evaluator_version = tempest_program_set_string(
            evaluator$evaluator_version,
            paste0("evaluators$", stage, "$evaluator_version")
          )
        )
      }
    ),
    stages
  )
}

tempest_program_set_governed_revisions <- function(value) {
  stages <- tempest_program_set_stages()
  if (is.null(value) || length(value) == 0L) {
    return(stats::setNames(rep(list(NULL), length(stages)), stages))
  }
  if (
    !is.list(value) &&
      !(is.character(value) && !is.null(names(value)))
  ) {
    tempest_program_set_abort(
      paste0(
        "{.arg governed_procedure_revision_ids} must be a named list or ",
        "character vector."
      )
    )
  }
  if (
    is.null(names(value)) ||
      anyNA(names(value)) ||
      any(!nzchar(names(value))) ||
      anyDuplicated(names(value)) ||
      any(!names(value) %in% stages)
  ) {
    tempest_program_set_abort(
      paste0(
        "{.arg governed_procedure_revision_ids} must use unique known ",
        "Tempest stage names."
      )
    )
  }
  result <- stats::setNames(rep(list(NULL), length(stages)), stages)
  for (stage in names(value)) {
    revision <- value[[stage]]
    if (!is.null(revision)) {
      revision <- tempest_program_set_string(
        revision,
        paste0("governed_procedure_revision_ids$", stage)
      )
    }
    result[[stage]] <- revision
  }
  result
}

tempest_program_set_validate_programs <- function(value, arg = "programs") {
  stages <- tempest_program_set_stages()
  value <- tempest_program_set_named_list(value, arg, stages)
  invalid <- stages[!vapply(value, inherits, logical(1), what = "Module")]
  if (length(invalid) > 0L) {
    tempest_program_set_abort(
      "{.arg {arg}} must contain only dsprrr Module objects; invalid: {invalid}."
    )
  }
  value
}

tempest_program_set_program_id <- function(program, stage, registry = list()) {
  id <- tryCatch(
    dsprrr::program_artifact_id(program, registry = registry),
    error = function(error) {
      tempest_program_set_abort(
        "Could not verify the dsprrr program for stage {.val {stage}}.",
        parent = error,
        class = "tempest_program_set_verification_error"
      )
    }
  )
  if (
    !is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !grepl("^sha256:[a-f0-9]{64}$", id)
  ) {
    tempest_program_set_abort(
      "Stage {.val {stage}} did not produce a valid dsprrr artifact ID.",
      class = "tempest_program_set_verification_error"
    )
  }
  id
}

tempest_program_set_reference <- function(stage, type = c("builtin", "file")) {
  type <- match.arg(type)
  if (identical(type, "builtin")) {
    return(list(type = "builtin", id = paste0("tempest::", stage)))
  }
  list(type = "file", path = paste0("programs/", stage, ".rds"))
}

tempest_program_set_entries_from_programs <- function(
  programs,
  contract_versions,
  evaluators,
  governed_revisions,
  reference_type,
  registry = list()
) {
  stages <- tempest_program_set_stages()
  stats::setNames(
    lapply(
      stages,
      \(stage) {
        list(
          stage = stage,
          contract_version = contract_versions[[stage]],
          program_artifact_id = tempest_program_set_program_id(
            programs[[stage]],
            stage,
            registry = registry
          ),
          artifact_reference = tempest_program_set_reference(
            stage,
            reference_type
          ),
          governed_procedure_revision_id = governed_revisions[[stage]],
          evaluator_id = evaluators[[stage]]$evaluator_id,
          evaluator_version = evaluators[[stage]]$evaluator_version
        )
      }
    ),
    stages
  )
}

tempest_program_set_root <- function(value, must_exist = FALSE) {
  value <- tempest_program_set_string(value, "path")
  value <- as.character(fs::path_abs(value))
  if (must_exist && !fs::dir_exists(value)) {
    tempest_program_set_abort(
      "The ProgramSet bundle does not exist at {.path {value}}.",
      class = "tempest_program_set_persistence_error"
    )
  }
  value
}

tempest_program_set_validate_entries <- function(entries, require_all = TRUE) {
  entries <- tempest_research_manifest_programs(entries)
  stages <- tempest_program_set_stages()
  if (require_all && !setequal(names(entries), stages)) {
    missing <- setdiff(stages, names(entries))
    extra <- setdiff(names(entries), stages)
    tempest_program_set_abort(
      paste0(
        "ProgramSet entries must contain the exact Tempest stages ",
        "(missing: {missing}; unknown: {extra})."
      )
    )
  }
  if (require_all) {
    entries <- entries[stages]
  }
  entries
}

tempest_program_set_s7_validator <- function(self) {
  result <- tryCatch(
    {
      if (!identical(self@schema_version, 1L)) {
        stop("schema_version must be the supported version 1")
      }
      entries <- tempest_program_set_validate_entries(self@entries)
      programs <- tempest_program_set_validate_programs(self@programs)
      if (!identical(names(entries), names(programs))) {
        stop("entries and programs must use the same stage order")
      }
      root <- self@bundle_root
      if (
        !is.character(root) ||
          !(length(root) %in% c(0L, 1L)) ||
          (length(root) == 1L &&
            (is.na(root) ||
              !nzchar(root) ||
              !fs::is_absolute_path(root)))
      ) {
        stop("bundle_root must be empty or one absolute directory path")
      }
      reference_types <- vapply(
        entries,
        \(entry) entry$artifact_reference$type,
        character(1)
      )
      expected_type <- if (length(root) == 0L) "builtin" else "file"
      if (any(reference_types != expected_type)) {
        stop("artifact reference types must agree with bundle_root")
      }
      if (length(root) == 1L) {
        tempest_program_set_validate_inventory(root)
      }
      NULL
    },
    error = conditionMessage
  )
  result
}

#' TempestProgramSet (S7)
#'
#' A validated live collection of the exact dsprrr programs used by Tempest.
#' Durable projections contain only entry metadata; registry-bound modules and
#' the local bundle root remain live process state.
#'
#' @keywords internal
TempestProgramSet <- S7::new_class(
  "TempestProgramSet",
  properties = list(
    schema_version = S7::new_property(S7::class_integer, default = 1L),
    bundle_root = S7::new_property(
      S7::class_character,
      default = character()
    ),
    entries = S7::new_property(S7::class_list),
    programs = S7::new_property(S7::class_list)
  ),
  validator = tempest_program_set_s7_validator
)

tempest_program_set_new <- function(entries, programs, bundle_root = NULL) {
  entries <- tempest_program_set_validate_entries(entries)
  programs <- tempest_program_set_validate_programs(programs)
  root <- if (is.null(bundle_root)) character() else bundle_root
  tryCatch(
    TempestProgramSet(
      schema_version = 1L,
      bundle_root = root,
      entries = entries,
      programs = programs
    ),
    error = function(error) {
      if (inherits(error, "tempest_program_set_error")) {
        stop(error)
      }
      tempest_program_set_abort(
        "Could not construct a valid TempestProgramSet.",
        parent = error
      )
    }
  )
}

tempest_program_set_assert <- function(value) {
  if (!S7::S7_inherits(value, TempestProgramSet)) {
    tempest_program_set_abort(
      "{.arg program_set} must be a TempestProgramSet."
    )
  }
  tryCatch(
    S7::validate(value),
    error = function(error) {
      tempest_program_set_abort(
        "The TempestProgramSet failed live validation.",
        parent = error,
        class = "tempest_program_set_verification_error"
      )
    }
  )
  value
}

tempest_program_set_metadata <- function(program_set) {
  entries <- program_set@entries
  stages <- tempest_program_set_stages()
  list(
    contract_versions = stats::setNames(
      vapply(entries, \(entry) entry$contract_version, integer(1)),
      stages
    ),
    evaluators = stats::setNames(
      lapply(
        entries,
        \(entry) {
          list(
            evaluator_id = entry$evaluator_id,
            evaluator_version = entry$evaluator_version
          )
        }
      ),
      stages
    ),
    governed_revisions = stats::setNames(
      lapply(entries, \(entry) entry$governed_procedure_revision_id),
      stages
    )
  )
}

tempest_program_set_bundle_record <- function(entries) {
  list(
    format = "tempest-program-set",
    schema_version = 1L,
    entries = entries
  )
}

tempest_program_set_expected_files <- function() {
  c(
    "program-set.json",
    paste0("programs/", tempest_program_set_stages(), ".rds")
  )
}

tempest_program_set_write_bundle <- function(
  programs,
  path,
  contract_versions,
  evaluators,
  governed_revisions,
  registry = list()
) {
  root <- tempest_program_set_root(path)
  if (fs::file_exists(root) || fs::dir_exists(root)) {
    tempest_program_set_abort(
      "Refusing to replace the existing path {.path {root}}.",
      class = "tempest_program_set_persistence_error"
    )
  }
  parent <- dirname(root)
  fs::dir_create(parent, recurse = TRUE)
  staging <- tempfile(
    pattern = paste0(".", basename(root), "-"),
    tmpdir = parent
  )
  if (!dir.create(staging, recursive = FALSE)) {
    tempest_program_set_abort(
      "Could not create a ProgramSet staging directory.",
      class = "tempest_program_set_persistence_error"
    )
  }
  complete <- FALSE
  on.exit(
    {
      if (!complete && fs::dir_exists(staging)) {
        unlink(staging, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  programs_dir <- file.path(staging, "programs")
  fs::dir_create(programs_dir)
  entries <- tempest_program_set_entries_from_programs(
    programs,
    contract_versions,
    evaluators,
    governed_revisions,
    reference_type = "file",
    registry = registry
  )
  restored <- stats::setNames(vector("list", length(programs)), names(programs))
  for (stage in tempest_program_set_stages()) {
    artifact_path <- file.path(programs_dir, paste0(stage, ".rds"))
    tryCatch(
      dsprrr::save_program(
        programs[[stage]],
        artifact_path,
        registry = registry
      ),
      error = function(error) {
        tempest_program_set_abort(
          "Could not save the dsprrr program for stage {.val {stage}}.",
          parent = error,
          class = "tempest_program_set_persistence_error"
        )
      }
    )
    restored[[stage]] <- tryCatch(
      dsprrr::load_program(artifact_path, registry = registry),
      error = function(error) {
        tempest_program_set_abort(
          "Could not reload the dsprrr program for stage {.val {stage}}.",
          parent = error,
          class = "tempest_program_set_persistence_error"
        )
      }
    )
    actual <- tempest_program_set_program_id(restored[[stage]], stage)
    if (!identical(actual, entries[[stage]]$program_artifact_id)) {
      tempest_program_set_abort(
        "Saved program identity changed for stage {.val {stage}}.",
        class = "tempest_program_set_verification_error"
      )
    }
  }
  manifest_path <- file.path(staging, "program-set.json")
  tryCatch(
    jsonlite::write_json(
      tempest_program_set_bundle_record(entries),
      manifest_path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null",
      digits = NA
    ),
    error = function(error) {
      tempest_program_set_abort(
        "Could not write the ProgramSet manifest.",
        parent = error,
        class = "tempest_program_set_persistence_error"
      )
    }
  )
  if (!file.rename(staging, root)) {
    tempest_program_set_abort(
      "Could not publish the complete ProgramSet bundle.",
      class = "tempest_program_set_persistence_error"
    )
  }
  complete <- TRUE
  tempest_program_set_new(entries, restored, bundle_root = root)
}

#' Create a validated Tempest program set
#'
#' `tempest_program_set()` resolves the exact ten dsprrr programs used by the
#' STORM and Co-STORM product stages. With `programs = NULL` and `path = NULL`,
#' it creates the package's addressable builtin programs without writing files.
#' Custom programs require `path` and are persisted immediately as verified
#' dsprrr program artifacts.
#'
#' The returned live value retains executable modules. Its manifest projection
#' contains only portable identifiers, evaluator metadata, governed-procedure
#' references, and builtin or bundle-relative artifact references.
#'
#' Resume compares program identity independently of physical location, so a
#' relocated verified bundle is accepted. A custom-program run must resume with
#' a ProgramSet carrying the same identities. Omitting `program_set` at a run
#' boundary resolves the package's current builtins and can resume only when
#' those builtin artifact IDs match the persisted run.
#'
#' @param programs `NULL` for the exact package builtin programs, or an exact
#'   named list of ten dsprrr Module objects.
#' @param path Optional directory in which to create a file-backed ProgramSet.
#'   It must not already exist. Custom `programs` require this argument.
#' @param contract_versions Contract version `1`, or an exact named integer
#'   vector containing version `1` for every stage.
#' @param evaluators `NULL` for Tempest's named stage output-contract
#'   evaluators, or an exact named list whose records contain `evaluator_id`
#'   and `evaluator_version`. These identify how stage output is judged and are
#'   distinct from an optimization teleprompter or metric.
#' @param governed_procedure_revision_ids Optional named list or character
#'   vector of governed procedure revision identifiers by stage.
#' @param registry Named runtime-binding registry passed to dsprrr artifact
#'   operations. It is never stored in ProgramSet metadata.
#' @return A validated `TempestProgramSet` S7 object.
#' @export
tempest_program_set <- function(
  programs = NULL,
  path = NULL,
  contract_versions = 1L,
  evaluators = NULL,
  governed_procedure_revision_ids = list(),
  registry = list()
) {
  builtin <- is.null(programs)
  if (builtin) {
    programs <- tempest_make_dsprrr_modules(NULL)
    if (is.null(programs)) {
      tempest_program_set_abort(
        "Could not construct the builtin Tempest dsprrr programs."
      )
    }
  } else if (is.null(path)) {
    tempest_program_set_abort(
      "{.arg path} is required when supplying custom programs."
    )
  }
  programs <- tempest_program_set_validate_programs(programs)
  contract_versions <- tempest_program_set_contract_versions(contract_versions)
  evaluators <- tempest_program_set_evaluators(evaluators)
  governed_revisions <- tempest_program_set_governed_revisions(
    governed_procedure_revision_ids
  )
  if (!is.null(path)) {
    return(tempest_program_set_write_bundle(
      programs,
      path,
      contract_versions,
      evaluators,
      governed_revisions,
      registry = registry
    ))
  }
  entries <- tempest_program_set_entries_from_programs(
    programs,
    contract_versions,
    evaluators,
    governed_revisions,
    reference_type = "builtin",
    registry = registry
  )
  tempest_program_set_new(entries, programs)
}

tempest_program_set_entries <- function(program_set) {
  tempest_program_set_programs(program_set)
  program_set@entries
}

tempest_program_set_entry <- function(program_set, stage) {
  tempest_program_set_assert(program_set)
  stage <- tempest_program_set_string(stage, "stage")
  if (!stage %in% tempest_program_set_stages()) {
    tempest_program_set_abort("Unknown Tempest program stage {.val {stage}}.")
  }
  program_set@entries[[stage]]
}

tempest_program_set_verify_program <- function(program_set, stage) {
  entry <- program_set@entries[[stage]]
  program <- program_set@programs[[stage]]
  actual <- tempest_program_set_program_id(program, stage)
  if (!identical(actual, entry$program_artifact_id)) {
    tempest_program_set_abort(
      "Live program identity changed for stage {.val {stage}}.",
      class = "tempest_program_set_verification_error"
    )
  }
  program
}

tempest_program_set_program <- function(program_set, stage) {
  tempest_program_set_assert(program_set)
  stage <- tempest_program_set_string(stage, "stage")
  if (!stage %in% tempest_program_set_stages()) {
    tempest_program_set_abort("Unknown Tempest program stage {.val {stage}}.")
  }
  tempest_program_set_verify_program(program_set, stage)
}

tempest_program_set_programs <- function(program_set) {
  tempest_program_set_assert(program_set)
  stages <- tempest_program_set_stages()
  stats::setNames(
    lapply(
      stages,
      \(stage) tempest_program_set_verify_program(program_set, stage)
    ),
    stages
  )
}

tempest_program_set_manifest_programs <- function(program_set) {
  tempest_program_set_programs(program_set)
  tempest_research_manifest_programs(program_set@entries)
}

tempest_program_set_identity_references <- function(x) {
  entries <- if (S7::S7_inherits(x, TempestProgramSet)) {
    tempest_program_set_manifest_programs(x)
  } else {
    x
  }
  tempest_research_manifest_program_identity_records(entries)
}

tempest_program_set_identity_equal <- function(x, y) {
  tempest_research_manifest_programs_same_identity(
    if (S7::S7_inherits(x, TempestProgramSet)) {
      tempest_program_set_manifest_programs(x)
    } else {
      x
    },
    if (S7::S7_inherits(y, TempestProgramSet)) {
      tempest_program_set_manifest_programs(y)
    } else {
      y
    }
  )
}

#' Save a Tempest program set
#'
#' Materializes every live program through [dsprrr::save_program()], reloads it
#' through [dsprrr::load_program()], verifies its artifact identity, and writes
#' the manifest last. The destination must not already exist.
#'
#' @param program_set A `TempestProgramSet`.
#' @param path New directory for the ProgramSet bundle.
#' @param registry Named runtime-binding registry passed to dsprrr. It is not
#'   persisted.
#' @return A file-backed `TempestProgramSet` resolved from the new bundle.
#' @export
tempest_save_program_set <- function(program_set, path, registry = list()) {
  programs <- tempest_program_set_programs(program_set)
  metadata <- tempest_program_set_metadata(program_set)
  tempest_program_set_write_bundle(
    programs,
    path,
    metadata$contract_versions,
    metadata$evaluators,
    metadata$governed_revisions,
    registry = registry
  )
}

tempest_program_set_validate_inventory <- function(root) {
  inventory_abort <- function(message) {
    tempest_program_set_abort(
      message,
      class = "tempest_program_set_persistence_error"
    )
  }
  top_level <- list.files(
    root,
    all.files = TRUE,
    recursive = FALSE,
    no.. = TRUE
  )
  if (!identical(sort(top_level), c("program-set.json", "programs"))) {
    inventory_abort(
      "The ProgramSet bundle inventory is incomplete or contains undeclared paths."
    )
  }

  manifest_path <- file.path(root, "program-set.json")
  programs_path <- file.path(root, "programs")
  top_links <- unname(fs::is_link(c(manifest_path, programs_path)))
  if (anyNA(top_links) || any(top_links)) {
    inventory_abort(
      "ProgramSet bundle paths cannot be symbolic links."
    )
  }
  manifest_type <- as.character(fs::file_info(manifest_path)$type[[1]])
  programs_type <- as.character(fs::file_info(programs_path)$type[[1]])
  if (!identical(manifest_type, "file")) {
    inventory_abort("The ProgramSet manifest must be a regular file.")
  }
  if (!identical(programs_type, "directory")) {
    inventory_abort("The ProgramSet programs path must be a directory.")
  }

  artifact_names <- paste0(tempest_program_set_stages(), ".rds")
  program_inventory <- list.files(
    programs_path,
    all.files = TRUE,
    recursive = FALSE,
    no.. = TRUE
  )
  if (!identical(sort(program_inventory), sort(artifact_names))) {
    inventory_abort(
      "The ProgramSet bundle inventory is incomplete or contains undeclared paths."
    )
  }
  artifact_paths <- file.path(programs_path, artifact_names)
  artifact_links <- unname(fs::is_link(artifact_paths))
  if (anyNA(artifact_links) || any(artifact_links)) {
    inventory_abort("ProgramSet bundle paths cannot be symbolic links.")
  }
  artifact_types <- as.character(fs::file_info(artifact_paths)$type)
  if (any(artifact_types != "file")) {
    inventory_abort("Every ProgramSet artifact must be a regular file.")
  }

  root_real <- normalizePath(root, winslash = "/", mustWork = TRUE)
  manifest_real <- normalizePath(
    manifest_path,
    winslash = "/",
    mustWork = TRUE
  )
  programs_real <- normalizePath(
    programs_path,
    winslash = "/",
    mustWork = TRUE
  )
  artifact_real <- normalizePath(
    artifact_paths,
    winslash = "/",
    mustWork = TRUE
  )
  if (
    !identical(dirname(manifest_real), root_real) ||
      !identical(dirname(programs_real), root_real) ||
      any(dirname(artifact_real) != programs_real)
  ) {
    inventory_abort(
      "Every declared ProgramSet path must resolve inside its bundle."
    )
  }
}

tempest_program_set_read_manifest <- function(root) {
  manifest <- tryCatch(
    jsonlite::read_json(
      file.path(root, "program-set.json"),
      simplifyVector = FALSE
    ),
    error = function(error) {
      tempest_program_set_abort(
        "Could not read the ProgramSet manifest.",
        parent = error,
        class = "tempest_program_set_persistence_error"
      )
    }
  )
  fields <- c("format", "schema_version", "entries")
  manifest_names <- names(manifest)
  if (
    !is.list(manifest) ||
      length(manifest) != length(fields) ||
      is.null(manifest_names) ||
      anyNA(manifest_names) ||
      any(!nzchar(manifest_names)) ||
      anyDuplicated(manifest_names) ||
      !setequal(manifest_names, fields)
  ) {
    tempest_program_set_abort(
      "The ProgramSet manifest must contain exactly format, schema_version, and entries.",
      class = "tempest_program_set_persistence_error"
    )
  }
  manifest <- manifest[fields]
  if (!identical(manifest$format, "tempest-program-set")) {
    tempest_program_set_abort(
      "The ProgramSet manifest format is unsupported.",
      class = "tempest_program_set_persistence_error"
    )
  }
  if (!identical(manifest$schema_version, 1L)) {
    tempest_program_set_abort(
      "The ProgramSet manifest schema version is unsupported.",
      class = "tempest_program_set_persistence_error"
    )
  }
  entries <- tryCatch(
    tempest_program_set_validate_entries(manifest$entries),
    error = function(error) {
      tempest_program_set_abort(
        "The ProgramSet manifest entries are invalid.",
        parent = error,
        class = "tempest_program_set_persistence_error"
      )
    }
  )
  reference_types <- vapply(
    entries,
    \(entry) entry$artifact_reference$type,
    character(1)
  )
  if (any(reference_types != "file")) {
    tempest_program_set_abort(
      "A file bundle may contain only bundle-relative file references.",
      class = "tempest_program_set_persistence_error"
    )
  }
  entries
}

#' Load and verify a Tempest program set
#'
#' Loads a closed-inventory ProgramSet bundle through
#' [dsprrr::load_program()] and recomputes every dsprrr artifact ID before the
#' set can be used.
#'
#' @param path Directory containing a ProgramSet bundle.
#' @param registry Named runtime-binding registry required by custom dsprrr
#'   artifacts. The resolved modules retain bindings, while the registry is not
#'   included in durable ProgramSet metadata.
#' @return A verified, file-backed `TempestProgramSet`.
#' @export
tempest_load_program_set <- function(path, registry = list()) {
  root <- tempest_program_set_root(path, must_exist = TRUE)
  tempest_program_set_validate_inventory(root)
  entries <- tempest_program_set_read_manifest(root)
  stages <- tempest_program_set_stages()
  programs <- stats::setNames(vector("list", length(stages)), stages)
  for (stage in stages) {
    artifact_path <- file.path(root, "programs", paste0(stage, ".rds"))
    programs[[stage]] <- tryCatch(
      dsprrr::load_program(artifact_path, registry = registry),
      error = function(error) {
        tempest_program_set_abort(
          "Could not load the dsprrr program for stage {.val {stage}}.",
          parent = error,
          class = "tempest_program_set_persistence_error"
        )
      }
    )
    actual <- tempest_program_set_program_id(programs[[stage]], stage)
    if (!identical(actual, entries[[stage]]$program_artifact_id)) {
      tempest_program_set_abort(
        "Program artifact identity mismatch for stage {.val {stage}}.",
        class = "tempest_program_set_verification_error"
      )
    }
  }
  tempest_program_set_new(entries, programs, bundle_root = root)
}

tempest_program_set_compile_inputs <- function(value, arg, stages) {
  value <- tempest_program_set_named_list(value, arg)
  if (
    length(value) == 0L ||
      any(!names(value) %in% stages)
  ) {
    tempest_program_set_abort(
      "{.arg {arg}} must name at least one known Tempest stage."
    )
  }
  value
}

tempest_program_set_stage_values <- function(
  value,
  arg,
  selected,
  allow_single = FALSE,
  default = NULL
) {
  if (is.null(value)) {
    return(stats::setNames(rep(list(default), length(selected)), selected))
  }
  if (allow_single && inherits(value, "dsprrr::Teleprompter")) {
    return(stats::setNames(rep(list(value), length(selected)), selected))
  }
  value <- tempest_program_set_named_list(value, arg)
  if (!setequal(names(value), selected)) {
    tempest_program_set_abort(
      "{.arg {arg}} must contain exactly the selected compile stages."
    )
  }
  value[selected]
}

#' Compile programs in a Tempest program set
#'
#' Compiles the selected stages through [dsprrr::compile_module()] and only
#' publishes a new complete ProgramSet after every requested compilation and
#' artifact verification succeeds. Compilation errors are never replaced with
#' the original uncompiled program.
#'
#' @param program_set A `TempestProgramSet`.
#' @param trainsets Named list of training data by stage. Its names select the
#'   stages to compile.
#' @param teleprompters One dsprrr Teleprompter used for every selected stage,
#'   or an exact named list of Teleprompters.
#' @param path New directory for the compiled ProgramSet bundle.
#' @param valsets Optional exact named list of validation data for the selected
#'   stages. Omitted stages receive `NULL` only when the whole argument is
#'   omitted.
#' @param registry Named runtime-binding registry passed to dsprrr artifact
#'   operations. It is not persisted.
#' @param .llm Optional language-model runtime passed to dsprrr compilation.
#' @param compile_args Optional exact named list of argument lists passed to
#'   `dsprrr::compile_module()` for each selected stage.
#' @return A new verified, file-backed `TempestProgramSet`.
#' @export
tempest_compile_programs <- function(
  program_set,
  trainsets,
  teleprompters,
  path,
  valsets = NULL,
  registry = list(),
  .llm = NULL,
  compile_args = NULL
) {
  tempest_program_set_assert(program_set)
  stages <- tempest_program_set_stages()
  trainsets <- tempest_program_set_compile_inputs(
    trainsets,
    "trainsets",
    stages
  )
  selected <- names(trainsets)
  teleprompters <- tempest_program_set_stage_values(
    teleprompters,
    "teleprompters",
    selected,
    allow_single = TRUE
  )
  if (
    any(
      !vapply(
        teleprompters,
        inherits,
        logical(1),
        what = "dsprrr::Teleprompter"
      )
    )
  ) {
    tempest_program_set_abort(
      "{.arg teleprompters} must contain only dsprrr Teleprompter objects."
    )
  }
  valsets <- tempest_program_set_stage_values(
    valsets,
    "valsets",
    selected,
    default = NULL
  )
  compile_args <- tempest_program_set_stage_values(
    compile_args,
    "compile_args",
    selected,
    default = list()
  )
  valid_compile_args <- vapply(
    compile_args,
    function(args) {
      is.list(args) &&
        (length(args) == 0L ||
          (!is.null(names(args)) &&
            !anyNA(names(args)) &&
            all(nzchar(names(args))) &&
            !anyDuplicated(names(args))))
    },
    logical(1)
  )
  if (any(!valid_compile_args)) {
    tempest_program_set_abort(
      paste0(
        "{.arg compile_args} must contain empty or uniquely named ",
        "argument lists."
      )
    )
  }
  programs <- tempest_program_set_programs(program_set)
  compiled <- programs
  for (stage in selected) {
    managed <- c("program", "teleprompter", "trainset", "valset", ".llm")
    duplicates <- intersect(names(compile_args[[stage]]), managed)
    if (length(duplicates) > 0L) {
      tempest_program_set_abort(
        paste0(
          "{.arg compile_args} duplicates managed arguments for stage ",
          "{.val {stage}}: {duplicates}."
        )
      )
    }
    args <- c(
      list(
        program = programs[[stage]],
        teleprompter = teleprompters[[stage]],
        trainset = trainsets[[stage]],
        valset = valsets[[stage]],
        .llm = .llm
      ),
      compile_args[[stage]]
    )
    compiled[[stage]] <- tryCatch(
      do.call(dsprrr::compile_module, args),
      error = function(error) {
        tempest_program_set_abort(
          "Compilation failed for stage {.val {stage}}.",
          parent = error,
          class = "tempest_program_set_compile_error"
        )
      }
    )
    if (!inherits(compiled[[stage]], "Module")) {
      tempest_program_set_abort(
        "Compilation did not return a dsprrr Module for stage {.val {stage}}.",
        class = "tempest_program_set_compile_error"
      )
    }
  }
  metadata <- tempest_program_set_metadata(program_set)
  tempest_program_set_write_bundle(
    compiled,
    path,
    metadata$contract_versions,
    metadata$evaluators,
    metadata$governed_revisions,
    registry = registry
  )
}
