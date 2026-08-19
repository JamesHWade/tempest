test_that("completed STORM product state fails closed when artifacts drift", {
  test_env <- environment()
  program_set <- tempest_program_set()
  program_references <-
    tempest:::tempest_program_set_manifest_programs(program_set)
  make_bundle <- function() {
    dir <- withr::local_tempdir(
      pattern = "tempest-completed-state-",
      .local_envir = test_env
    )
    cfg <- tempest_config()
    outline <- list(
      title = "Durable state",
      sections = list(list(
        title = "Findings",
        summary = "Summary",
        subsections = list(list(
          title = "Evidence",
          bullets = "Durable evidence",
          needed = "What is authoritative?"
        ))
      ))
    )
    workspace <- tempest_research_workspace()
    source <- tempest:::tempest_source(
      "https://example.com/durable-state",
      title = "Durable source",
      content_text = "Durable evidence supports the durable claim."
    )
    workspace$upsert_retrieved_resource(source)
    span_id <- workspace$add_evidence_span(tempest_evidence_span(
      evidence_span_id = "span-durable-state",
      source_id = source$id,
      quote = "Durable evidence supports the durable claim.",
      extracted_by = program_references$extract_claims$program_artifact_id
    ))
    claim_id <- workspace$add_proposed_claim(tempest_claim(
      claim_id = "claim-durable-state",
      claim_text = "Durable evidence supports the durable claim.",
      source_ids = source$id,
      evidence_span_ids = span_id,
      supporting_quotes = list("Durable evidence supports the durable claim."),
      verification_status = "supported",
      support_score = 0.9
    ))
    workspace$verify_proposed_claims_batch(
      list(tempest_claim_support(
        claim_id = claim_id,
        evidence_span_id = span_id,
        source_id = source$id,
        verification_status = "supported",
        support_score = 0.9,
        rationale = "The durable source directly supports the claim."
      )),
      verified_at = "2026-08-16T00:00:00Z"
    )
    report_md <- tempest_report_md(
      title = "Durable state",
      body = paste0(
        "Durable evidence supports the durable claim. [",
        source$id,
        "]"
      ),
      workspace = workspace,
      citation_policy = cfg@citation_policy,
      on_unsupported_claim = cfg@on_unsupported_claim,
      min_support_score = cfg@min_support_score
    )
    state <- tempest:::tempest_storm_state(
      "Durable state",
      perspectives = list(list(
        name = "Overview",
        description = "General overview",
        key_questions = "What is authoritative?"
      )),
      experts = list(tempest_expert(
        expert_id = "expert.durable-state",
        name = "Durable State Expert",
        title = "Persistence analyst",
        description = "Checks completed product state.",
        instructions = "Reject incomplete persisted stages."
      )),
      draft_outline = outline,
      outline = outline,
      lead_section = "Durable evidence supports the durable claim.",
      draft_md = paste0(
        "Durable evidence supports the durable claim.\n\n",
        "## Findings\n\n",
        "Durable evidence supports the durable claim."
      ),
      report_md = report_md,
      completed_stages = c(
        "perspectives",
        "research",
        "outline",
        "write",
        "polish"
      )
    )
    manifest <- tempest_research_manifest(
      "completed-state",
      config = cfg,
      programs = program_references
    )
    bound <- test_persistence_bind_storm_records(state, workspace, manifest)
    state <- bound$state
    manifest <- bound$manifest
    manifest <- tempest_research_manifest_update(
      manifest,
      status = "succeeded"
    )
    tempest:::tempest_storm_save_artifacts(
      dir,
      workspace,
      state,
      manifest,
      program_set = program_set,
      config = cfg,
      steps = state$completed_stages
    )
    list(dir = dir, config = cfg)
  }
  clone_bundle <- function(bundle) {
    root <- withr::local_tempdir(
      pattern = "tempest-completed-state-clone-",
      .local_envir = test_env
    )
    dir <- file.path(root, "bundle")
    fs::dir_copy(bundle$dir, dir)
    list(dir = dir, config = bundle$config)
  }
  rewrite_checked <- function(bundle, file, value, json = TRUE) {
    path <- file.path(bundle$dir, file)
    if (json) {
      tempest:::tempest_product_write_json(path, value)
    } else {
      writeLines(value, path)
    }
    manifest_path <- file.path(bundle$dir, "run_config.json")
    manifest <- tempest:::tempest_product_read_json(manifest_path)
    manifest$checksums[[file]] <-
      tempest:::tempest_product_bundle_checksum(bundle$dir, file)
    tempest:::tempest_product_write_json(manifest_path, manifest)
  }
  expect_rejected <- function(bundle) {
    expect_error(
      tempest:::tempest_storm_load_artifacts(
        bundle$dir,
        config = bundle$config,
        program_set = program_set,
        run_id = "completed-state"
      ),
      class = "tempest_run_restore_error"
    )
  }

  pristine <- make_bundle()
  bundle <- pristine
  loaded <- tempest:::tempest_storm_load_artifacts(
    bundle$dir,
    config = bundle$config,
    program_set = program_set,
    run_id = "completed-state"
  )
  expect_identical(
    names(loaded$state$perspectives[[1]]),
    tempest:::tempest_storm_perspective_fields()
  )
  expect_identical(
    names(loaded$state$outline),
    tempest:::tempest_storm_outline_fields()
  )
  expect_identical(
    names(loaded$state$outline$sections[[1]]),
    tempest:::tempest_storm_outline_section_fields()
  )
  expect_identical(
    names(loaded$state$outline$sections[[1]]$subsections[[1]]),
    tempest:::tempest_storm_outline_subsection_fields()
  )

  bundle <- clone_bundle(pristine)
  perspectives <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "perspectives.json")
  )
  perspectives[[1]] <- perspectives[[1]][rev(names(perspectives[[1]]))]
  rewrite_checked(bundle, "perspectives.json", perspectives)
  expect_rejected(bundle)

  bundle <- clone_bundle(pristine)
  outline <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "direct_gen_outline.json")
  )
  outline <- outline[rev(names(outline))]
  rewrite_checked(bundle, "direct_gen_outline.json", outline)
  expect_rejected(bundle)

  bundle <- clone_bundle(pristine)
  outline <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "storm_gen_outline.json")
  )
  outline$sections[[1]] <- outline$sections[[1]][
    rev(names(outline$sections[[1]]))
  ]
  rewrite_checked(bundle, "storm_gen_outline.json", outline)
  expect_rejected(bundle)

  bundle <- clone_bundle(pristine)
  outline <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "storm_gen_outline.json")
  )
  outline$sections[[1]]$subsections[[1]] <-
    outline$sections[[1]]$subsections[[1]][
      rev(names(outline$sections[[1]]$subsections[[1]]))
    ]
  rewrite_checked(bundle, "storm_gen_outline.json", outline)
  expect_rejected(bundle)

  bundle <- clone_bundle(pristine)
  rewrite_checked(bundle, "perspectives.json", list())
  expect_rejected(bundle)

  bundle <- clone_bundle(pristine)
  perspectives <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "perspectives.json")
  )
  perspectives[[1]]$key_questions <- list(list("What is authoritative?"))
  rewrite_checked(bundle, "perspectives.json", perspectives)
  expect_rejected(bundle)

  bundle <- clone_bundle(pristine)
  perspectives <- tempest:::tempest_product_read_json(
    file.path(bundle$dir, "perspectives.json")
  )
  second <- perspectives[[1]]
  second$name <- "Second"
  rewrite_checked(bundle, "perspectives.json", c(perspectives, list(second)))
  expect_rejected(bundle)

  for (file in c("direct_gen_outline.json", "storm_gen_outline.json")) {
    bundle <- clone_bundle(pristine)
    rewrite_checked(bundle, file, list())
    expect_rejected(bundle)

    bundle <- clone_bundle(pristine)
    outline <- tempest:::tempest_product_read_json(
      file.path(bundle$dir, file)
    )
    outline$sections[[1]]$subsections <- list(list(
      title = "Nested list",
      bullets = list(list("Nested bullet")),
      needed = list("Flat question")
    ))
    rewrite_checked(bundle, file, outline)
    expect_rejected(bundle)
  }

  for (file in c(
    "storm_gen_article.md",
    "storm_gen_article_polished.md"
  )) {
    bundle <- clone_bundle(pristine)
    rewrite_checked(bundle, file, "", json = FALSE)
    expect_rejected(bundle)
  }
})
