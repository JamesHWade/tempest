test_program_set_programs <- function() {
  stages <- tempest:::tempest_program_set_stages()
  stats::setNames(
    lapply(
      stages,
      \(stage) {
        dsprrr::module(
          dsprrr::signature(
            "input -> output",
            instructions = paste("Deterministic fixture for", stage)
          )
        )
      }
    ),
    stages
  )
}

test_program_set <- function(path = NULL, ...) {
  if (is.null(path)) {
    return(tempest_program_set(...))
  }
  tempest_program_set(
    programs = test_program_set_programs(),
    path = path,
    ...
  )
}

test_program_set_manifest <- function(path) {
  jsonlite::read_json(
    file.path(path, "program-set.json"),
    simplifyVector = FALSE
  )
}

test_write_program_set_manifest <- function(path, manifest) {
  jsonlite::write_json(
    manifest,
    file.path(path, "program-set.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
}

test_governed_procedure_ref <- function(
  stage,
  program_artifact_id,
  revision_id = paste0("revision:", stage),
  snapshot_id = "snapshot:test",
  evaluator_id = paste0("tempest::evaluator/", stage),
  evaluator_version = "1"
) {
  tempest:::tempest_governed_procedure_ref_new(
    stage = stage,
    tempest_governed_procedure_id = paste0("tempest-procedure:", stage),
    record_id = paste0("procedure:", stage),
    revision_id = revision_id,
    program_artifact_id = program_artifact_id,
    evaluator_id = evaluator_id,
    evaluator_version = evaluator_version,
    store_id = "store:test",
    snapshot_id = snapshot_id,
    schema_build_digest = "schema:test",
    commit_order = 1
  )
}

test_contains_runtime_value <- function(value) {
  if (
    is.function(value) ||
      is.environment(value) ||
      inherits(value, "R6") ||
      inherits(value, "S7_object") ||
      inherits(value, "connection") ||
      typeof(value) == "externalptr"
  ) {
    return(TRUE)
  }
  if (!is.list(value)) {
    return(FALSE)
  }
  any(vapply(value, test_contains_runtime_value, logical(1)))
}
