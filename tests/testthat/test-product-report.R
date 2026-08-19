test_that("product reports bind exact content references", {
  workspace <- tempest_research_workspace()

  report <- tempest_report_md(
    "Fixture",
    "Body.",
    workspace,
    citation_policy = "none"
  )
  reference <- tempest:::tempest_product_report_reference(
    "# Report\n\nBody."
  )

  expect_identical(report, "# Fixture\n\nBody.\n")
  expect_identical(
    reference,
    list(
      report_id = "report_md",
      sha256 = paste0(
        "sha256:",
        "00341683b8128b3f18a2335210553e5afc1c6600187a2dd93e4fbc9c539d04a7"
      )
    )
  )
  expect_no_error(tempest:::tempest_product_report_reference_validate(
    reference,
    "# Report\n\nBody."
  ))
})

test_that("product report references reject content substitution", {
  reference <- tempest:::tempest_product_report_reference("Original")

  expect_error(
    tempest:::tempest_product_report_reference_validate(
      reference,
      "Substituted"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("Co-STORM report preflight rejects invalid live state", {
  skip_if_not_installed("ellmer")
  chat <- fake_chat()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) chat
  )
  session <- tempest_session(
    "Report preflight",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.report-preflight",
      name = "Report Preflight Expert"
    ))
  )

  expect_error(
    session$report(include_references = NA),
    class = "tempest_product_validation_error"
  )
  expect_length(chat$.calls(), 0L)
})

test_that("Co-STORM report accessor reads only the exact committed artifact", {
  skip_if_not_installed("ellmer")
  config <- tempest_config(chat_fn = function(...) fake_chat())
  session <- tempest_session(
    "Committed report",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.committed-report",
      name = "Committed Report Expert"
    )),
    session_id = "costorm-committed-report"
  )
  evidence <- test_persistence_add_costorm_evidence(
    session,
    key = "committed-report"
  )
  report_md <- paste0(
    "# Committed report\n\n",
    evidence$claim@claim_text,
    " [",
    evidence$source@resource_id,
    "]."
  )
  report_md <- test_persistence_commit_costorm_report(session, report_md)

  expect_identical(tempest_session_report_md(session), report_md)
  expect_error(
    tempest_session_report_md(list()),
    class = "tempest_product_report_error"
  )

  original_manifest <- session$manifest
  manifest_with <- function(
    research_run_id = original_manifest@research_run_id,
    mode = original_manifest@mode,
    config_digest = original_manifest@config_digest,
    deliverables = original_manifest@deliverables
  ) {
    tempest_research_manifest(
      research_run_id = research_run_id,
      mode = mode,
      config_digest = config_digest,
      programs = original_manifest@programs,
      knowledge_snapshot = original_manifest@knowledge_snapshot,
      runtime = original_manifest@runtime,
      traces = original_manifest@traces,
      deliverables = deliverables,
      status = original_manifest@status
    )
  }
  private <- session$.__enclos_env__$private
  ephemeral <- rlang::duplicate(
    original_manifest@deliverables,
    shallow = FALSE
  )
  ephemeral$report_md$status <- "ephemeral"
  invalid_manifests <- list(
    manifest_with(deliverables = ephemeral),
    manifest_with(mode = "storm"),
    manifest_with(research_run_id = "costorm-wrong-report"),
    manifest_with(config_digest = paste0("sha256:", strrep("0", 64L)))
  )
  for (manifest in invalid_manifests) {
    private$manifest_value <- manifest
    expect_error(
      tempest_session_report_md(session),
      class = "tempest_product_report_error"
    )
  }
  private$manifest_value <- original_manifest

  running <- tempest_session(
    "Running report",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.running-report",
      name = "Running Report Expert"
    ))
  )
  expect_error(
    tempest_session_report_md(running),
    class = "tempest_product_report_error"
  )

  private$report_md_value <- NULL
  expect_error(
    tempest_session_report_md(session),
    class = "tempest_product_report_error"
  )
  private$report_md_value <- 1L
  expect_error(
    tempest_session_report_md(session),
    class = "tempest_product_report_error"
  )
  private$report_md_value <- paste0(report_md, "\n")
  expect_error(
    tempest_session_report_md(session),
    class = "tempest_product_report_error"
  )
  private$report_md_value <- report_md
})
