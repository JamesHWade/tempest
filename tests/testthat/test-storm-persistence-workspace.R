test_that("schema 7 STORM bundles round-trip the complete workspace", {
  skip_if_not_installed("graft")
  dir <- withr::local_tempdir()
  cfg <- tempest_config()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  knowledge <- test_knowledge_view()
  workspace <- tempest_research_workspace(
    graft_snapshot = knowledge$snapshot,
    max_sources = 4L,
    accepted_graft_references = list(
      list(record_id = "claim.accepted", revision_id = "revision-7")
    )
  )
  source <- tempest_resource(
    resource_kind = "web",
    locator = "https://example.com/current-study",
    title = "Current study",
    media_type = "text/html",
    content = "The study reports a reproducible result.",
    metadata = list(snippet = "The study reports a reproducible result.")
  )
  resource <- tempest_resource(
    resource_kind = "scientific.document",
    locator = "protocols/reviewed-assay",
    title = "Reviewed assay protocol",
    media_type = "text/plain",
    resource_id = "resource.reviewed-assay",
    content = "The assay was reviewed before use.",
    retrieved_at = "2026-08-15T13:00:00Z",
    metadata = list(revision_id = "protocol-9")
  )
  workspace$upsert_retrieved_resource(source)
  workspace$upsert_retrieved_resource(resource)
  span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-reviewed-assay",
    source_id = resource@resource_id,
    quote = "The assay was reviewed before use.",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  source_span_id <- workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-current-study",
    source_id = source@resource_id,
    quote = "The study reports a reproducible result.",
    extracted_by = program_references$extract_claims$program_artifact_id
  ))
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-complete-workspace",
    claim_text = "The result used a reviewed assay.",
    source_ids = c(resource@resource_id, source@resource_id),
    evidence_span_ids = c(span_id, source_span_id),
    supporting_quotes = list(
      "The assay was reviewed before use.",
      "The study reports a reproducible result."
    ),
    confidence = "high"
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-complete-workspace",
    topic = "Assay review",
    claim_ids = claim_id,
    evidence_balance = "agreement"
  ))
  workspace$verify_proposed_claims_batch(
    lapply(
      c(span_id, source_span_id),
      function(evidence_span_id) {
        span <- workspace$get_evidence_span(evidence_span_id)
        tempest_claim_support(
          claim_id = claim_id,
          evidence_span_id = evidence_span_id,
          source_id = span@source_id,
          verification_status = "supported",
          support_score = 0.92,
          rationale = "The exact span directly supports the claim."
        )
      }
    ),
    verified_at = "2026-08-16T00:00:00Z"
  )
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)

  state <- tempest:::tempest_storm_state(
    "Complete workspace",
    completed_stages = "research"
  )
  manifest <- tempest_research_manifest(
    "complete-workspace",
    config = cfg,
    programs = program_references,
    knowledge_snapshot = tempest:::tempest_snapshot_reference(
      knowledge$snapshot
    )
  )
  bound <- test_persistence_bind_storm_records(state, workspace, manifest)
  state <- bound$state
  manifest <- bound$manifest

  tempest:::tempest_storm_save_artifacts(
    dir,
    workspace,
    state,
    manifest,
    program_set = program_set,
    config = cfg,
    steps = "research"
  )

  metadata <- tempest:::tempest_product_read_json(
    file.path(dir, "run_config.json")
  )
  declared <- unlist(metadata$files, use.names = FALSE)
  expect_contains(declared, "workspace.json")
  expect_equal(
    any(c("sources.json", "claims.json", "claim_supports.json") %in% declared),
    FALSE
  )
  expect_equal(file.exists(file.path(dir, "workspace.json")), TRUE)
  expect_equal(file.exists(file.path(dir, "sources.json")), FALSE)
  expect_equal(file.exists(file.path(dir, "claims.json")), FALSE)
  expect_equal(file.exists(file.path(dir, "claim_supports.json")), FALSE)

  loaded <- tempest:::tempest_storm_load_artifacts(
    dir,
    config = cfg,
    program_set = program_set,
    run_id = "complete-workspace"
  )

  expect_identical(
    tempest:::tempest_research_workspace_snapshot(loaded$workspace),
    snapshot
  )

  workspace_path <- file.path(dir, "workspace.json")
  downgraded <- tempest:::tempest_product_read_json(workspace_path)
  downgraded$schema_version <- 4L
  tempest:::tempest_product_write_json(workspace_path, downgraded)
  metadata$checksums[["workspace.json"]] <-
    tempest:::tempest_product_bundle_checksum(dir, "workspace.json")
  tempest:::tempest_product_write_json(
    file.path(dir, "run_config.json"),
    metadata
  )
  expect_error(
    tempest:::tempest_storm_load_artifacts(
      dir,
      config = cfg,
      program_set = program_set,
      run_id = "complete-workspace"
    ),
    class = "tempest_run_restore_error"
  )
})

test_that("STORM workspace files match the exact manifest identity", {
  skip_if_not_installed("graft")
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- tempfile("tempest-workspace-identity-")
    dir.create(dir)
    cfg <- tempest_config()
    knowledge <- test_knowledge_view()
    workspace <- tempest_research_workspace(
      graft_snapshot = knowledge$snapshot,
      max_sources = 4L,
      accepted_graft_references = list(list(
        record_id = "accepted.identity",
        revision_id = "revision-1"
      ))
    )
    tempest:::tempest_storm_save_artifacts(
      dir,
      workspace,
      tempest:::tempest_storm_state("Workspace identity"),
      tempest_research_manifest(
        "workspace-identity",
        config = cfg,
        programs = program_references,
        knowledge_snapshot = tempest:::tempest_snapshot_reference(
          knowledge$snapshot
        )
      ),
      program_set = program_set,
      config = cfg,
      steps = "research"
    )
    list(dir = dir, config = cfg)
  }
  mutate_workspace <- function(bundle, mutate) {
    workspace_path <- file.path(bundle$dir, "workspace.json")
    workspace <- tempest:::tempest_product_read_json(workspace_path)
    workspace <- mutate(workspace)
    tempest:::tempest_product_write_json(workspace_path, workspace)
    manifest_path <- file.path(bundle$dir, "run_config.json")
    manifest <- tempest:::tempest_product_read_json(manifest_path)
    manifest$checksums[["workspace.json"]] <-
      tempest:::tempest_product_bundle_checksum(
        bundle$dir,
        "workspace.json"
      )
    tempest:::tempest_product_write_json(manifest_path, manifest)
  }
  expect_rejected <- function(bundle) {
    expect_error(
      tempest:::tempest_storm_load_artifacts(
        bundle$dir,
        config = bundle$config,
        program_set = program_set,
        run_id = "workspace-identity"
      ),
      class = "tempest_run_restore_error"
    )
  }

  bundle <- make_bundle()
  mutate_workspace(bundle, function(workspace) {
    workspace$max_sources <- 5L
    workspace
  })
  expect_rejected(bundle)

  bundle <- make_bundle()
  mutate_workspace(bundle, function(workspace) {
    workspace$accepted_graft_references[[2]] <- list(
      record_id = "accepted.extra",
      revision_id = "revision-2"
    )
    workspace
  })
  expect_rejected(bundle)

  bundle <- make_bundle()
  manifest <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "run_config.json")
  )
  names(manifest$workspace) <- c(
    "base_snapshot_id",
    "max_sources",
    "max_sources"
  )
  expect_error(
    tempest:::tempest_storm_restore_workspace(
      manifest
    ),
    class = "tempest_run_restore_error"
  )
})
