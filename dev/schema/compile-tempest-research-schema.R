#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
source <- file.path(root, "dev", "schema", "tempest-research.linkml.yaml")
output_dir <- file.path(root, "inst", "schema")
output <- file.path(output_dir, "tempest-research.graft.json")
sys.source(file.path(root, "R", "graft-schema.R"), envir = environment())
required_contract <- tempest_graft_contract_version
required_store_format <- tempest_graft_store_format_version

if (!requireNamespace("graft", quietly = TRUE)) {
  stop("Install Graft with consumer contract ", required_contract, " first.")
}

contract <- tryCatch(graft::graft_contract_version(), error = function(e) NULL)
pin_valid <- tryCatch(
  tempest_graft_pin_valid(contract) &&
    identical(contract$store_format, required_store_format),
  error = function(error) FALSE
)
if (!pin_valid) {
  stop(
    "The installed Graft package does not satisfy consumer contract ",
    required_contract,
    " with store format ",
    required_store_format,
    "."
  )
}

core <- system.file(
  "schema",
  "graft-core.linkml.yaml",
  package = "graft",
  mustWork = TRUE
)
stage <- tempfile("tempest-research-schema-")
dir.create(stage)
on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
source_staged <- file.copy(
  source,
  file.path(stage, basename(source)),
  overwrite = TRUE
)
core_staged <- file.copy(
  core,
  file.path(stage, basename(core)),
  overwrite = TRUE
)
if (!source_staged || !core_staged) {
  stop("Could not stage the Tempest and Graft LinkML sources.")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

schema <- graft::graft_schema(
  file.path(stage, basename(source)),
  output = output
)
message(
  "Compiled Tempest research schema for Graft contract ",
  required_contract,
  ": ",
  schema@build_digest
)
