test_that("built-in workflow specifications resolve executable operations", {
  storm <- tempest_storm_workflow_spec()
  costorm <- tempest_costorm_workflow_spec()
  registry <- tempest_builtin_workflow_operation_registry()

  expect_equal(
    names(storm@steps),
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_equal(
    names(costorm@steps),
    c("warmup", "dialogue", "report")
  )
  expect_equal(
    unname(vapply(
      storm@steps,
      \(step) step@operation_id,
      character(1)
    )),
    paste0(
      "tempest.step.storm.",
      c("perspectives", "research", "outline", "write", "polish")
    )
  )
  expect_equal(
    unname(vapply(
      costorm@steps,
      \(step) step@operation_id,
      character(1)
    )),
    paste0(
      "tempest.step.costorm.",
      c("warmup", "dialogue", "report")
    )
  )
  expect_equal(costorm@steps$dialogue@approval_checkpoint, TRUE)
  expect_equal(storm@steps$research@assignment_rule$type, "all")
  expect_equal(
    costorm@steps$report@required_input_artifact_ids,
    "costorm.dialogue"
  )
  operations <- c(storm@steps, costorm@steps)
  expect_equal(
    unname(vapply(
      operations,
      function(step) {
        registry$has(
          step@operation_id,
          version = step@operation_version,
          kind = "step"
        )
      },
      logical(1)
    )),
    rep(TRUE, length(operations))
  )
})

test_that("built-in STORM operations execute through TempestRun", {
  stages <- character()
  report <- tempest_deliverable_spec(
    deliverable_id = "storm-report",
    title = "Test report",
    purpose = "Test the built-in workflow adapter.",
    instructions = "Return deterministic content.",
    evidence_policy = "none",
    generator_id = "tempest.generator.provided_content",
    renderer_ids = "tempest.renderer.markdown"
  )
  adapter <- function(stage, context, run) {
    stages <<- c(stages, stage)
    if (!identical(stage, "polish")) {
      return(NULL)
    }
    tempest_artifact(
      deliverable = report,
      content = "# Test report",
      artifact_id = "report_md",
      producer_operation_id = context$step@operation_id,
      run_id = run$run_id,
      step_id = stage,
      status = "valid"
    )
  }
  run <- tempest_run_workflow(
    objective = tempest_objective(
      "Test the STORM workflow",
      deliverable_ids = "storm-report"
    ),
    workflow = tempest_storm_workflow_spec(),
    runtime = tempest_builtin_workflow_operation_registry(
      storm_adapter = adapter
    ),
    deliverables = list(
      tempest:::tempest_workflow_checkpoint_spec(),
      report
    ),
    source_store = tempest_research_workspace()
  )

  expect_equal(run$status, "succeeded")
  expect_equal(
    stages,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_equal(
    names(run$artifact_catalog$list()),
    c(
      "report_md",
      "storm.draft",
      "storm.outline",
      "storm.perspectives",
      "storm.research"
    )
  )
  expect_equal(
    run$artifact("storm.research")@content$step_id,
    "research"
  )
  expect_equal(run$artifact("report_md")@content, "# Test report")
})

test_that("generic STORM defaults to one product ResearchWorkspace", {
  local_mocked_bindings(
    tempest_run_workflow = function(...) list(...)
  )

  arguments <- tempest_storm_workflow_run(
    "Workspace-backed STORM",
    config = tempest_config(cache_dir = withr::local_tempdir()),
    verbose = FALSE
  )

  workspace <- arguments$source_store
  retriever <- arguments$runtime_context$retriever
  expect_r6_class(workspace, "ResearchWorkspace")
  expect_identical(inherits(workspace, "SourceStore"), FALSE)
  expect_identical(retriever$workspace, workspace)
  expect_equal("store" %in% names(retriever), FALSE)
})

test_that("generic STORM run reuses existing stages and shared state", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()
  capability_calls <- 0L
  connection <- tempest_connection_ref(
    "connection.selected-documents",
    provider_id = "test.host",
    connection_type = "document-index",
    title = "Selected documents",
    description = "Documents approved for the selected expert"
  )
  capability <- tempest_capability_spec(
    "host.documents.read",
    purpose = "Read approved documents",
    instructions = "Use only the selected document connection.",
    operation_id = "host.capability.documents.read",
    connection_ref_ids = connection@connection_id,
    model_roles = "expert"
  )
  runtime <- tempest_runtime(
    capability_specs = list(capability),
    capability_implementations = list(
      "host.documents.read" = function(
        capability_spec,
        connections,
        context
      ) {
        capability_calls <<- capability_calls + 1L
        list(
          tools = list(),
          registrars = list(),
          metadata = list(connection_ids = names(connections))
        )
      }
    ),
    connection_refs = list(connection),
    connection_bindings = list(
      "connection.selected-documents" = list(index = "test")
    )
  )
  expert <- test_expert(
    expert_id = "expert.selected",
    name = "Dr. Selected",
    title = "Selected workflow analyst",
    required_capability_ids = "host.documents.read"
  )

  run <- tempest_storm_workflow_run(
    "Progress workflow",
    config = fixture$config,
    retriever = fixture$retriever,
    experts = list(expert),
    runtime = runtime,
    connection_permissions = list(
      "expert.selected" = "connection.selected-documents"
    ),
    program_set = tempest_program_set(),
    verbose = FALSE
  )

  expect_equal(run$status, "succeeded")
  expect_identical(run$source_store, fixture$store)
  expect_equal(names(run$experts), "expert.selected")
  expect_gte(capability_calls, 1L)
  expect_equal(
    run$connection_permissions$expert.selected,
    "connection.selected-documents"
  )
  expect_equal(run$assignments$research, "expert.selected")
  expect_contains(
    vapply(
      run$deliverables,
      \(deliverable) deliverable@deliverable_id,
      character(1)
    ),
    "storm-report"
  )
  grants <- tempest_run_capability_grants(run)
  expect_equal(
    grants$research$experts$expert.selected$host.documents.read$status,
    "granted"
  )
  expect_equal(
    grants$research$experts$expert.selected$host.documents.read$connection_ref_ids,
    "connection.selected-documents"
  )
  expect_equal(
    grants$research$experts$expert.selected$host.documents.read$metadata$connection_ids,
    "connection.selected-documents"
  )
  expect_equal(
    unname(vapply(run$step_states, `[[`, character(1), "status")),
    rep("succeeded", 5L)
  )
  expect_contains(
    names(run$artifact_catalog$list()),
    c(
      "storm.perspectives",
      "storm.research",
      "storm.outline",
      "storm.draft",
      "report_md"
    )
  )
  product <- run$step_states$polish$result$value
  expect_equal(product$report_md, run$artifact("report_md")@content)
  expect_named(
    product,
    c(
      "title",
      "perspectives",
      "experts",
      "outline",
      "draft_md",
      "report_md",
      "manifest",
      "state",
      "workspace",
      "retriever",
      "output_dir",
      "checkpoint"
    )
  )
  expect_equal(
    run$artifact("storm.perspectives")@content$state$expert_ids,
    "expert.selected"
  )
  expect_equal(
    run$artifact("storm.outline")@content$state$store_artifact_ids,
    "outline"
  )
  outline_state <- tempest:::tempest_storm_state_from_record(
    run$artifact("storm.outline")@content$state$storm_state
  )
  expect_identical(
    outline_state$completed_stages,
    c("perspectives", "research", "outline")
  )
  expect_identical(
    tempest:::tempest_storm_state_from_record(
      run$artifact("storm.draft")@content$state$storm_state
    )$outline,
    outline_state$outline
  )
})

test_that("generic STORM recomputes research assignments after expert generation", {
  skip_if_not_installed("ellmer")
  fixture <- storm_progress_fixture()

  run <- tempest_storm_workflow_run(
    "Generated expert workflow",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    program_set = tempest_program_set(),
    verbose = FALSE
  )

  expect_equal(run$status, "succeeded")
  expect_length(run$experts, 1L)
  expect_equal(run$assignments$research, names(run$experts))
  expect_equal(
    names(tempest_run_capability_grants(run)$research$experts),
    names(run$experts)
  )
})

test_that("Co-STORM run owns an approval-gated interactive session", {
  skip_if_not_installed("ellmer")
  fake_session_chat <- function(text = "") {
    list(
      chat = function(prompt, ...) text,
      chat_structured = function(...) list(),
      register_tools = function(...) invisible(NULL)
    )
  }
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      text <- if (identical(role, "writer")) {
        "A deterministic Co-STORM report."
      } else {
        ""
      }
      fake_session_chat(text)
    }
  )
  session <- tempest_session(
    "Generic Co-STORM",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.workflow",
      name = "Dr. Workflow",
      title = "Workflow specialist"
    ))
  )

  run <- tempest_costorm_workflow_run(
    session,
    include_references = FALSE,
    reorganize = FALSE,
    verbose = FALSE
  )

  expect_equal(run$status, "awaiting_approval")
  expect_identical(test_session_workflow_run(session), run)
  expect_identical(run$source_store, session$workspace)
  expect_identical(session$retriever$workspace, session$workspace)
  expect_equal("store" %in% names(session), FALSE)
  expect_identical(inherits(run$source_store, "SourceStore"), FALSE)
  expect_identical(run$artifact_catalog, test_session_artifact_catalog(session))
  expect_identical(
    run$experts,
    tempest:::tempest_run_expert_map(session$experts)
  )
  expect_identical(
    run$connection_permissions,
    session$connection_permissions
  )
  expect_identical(run$runtime$skills, session$runtime$skills)
  expect_identical(run$runtime$capabilities, session$runtime$capabilities)
  expect_identical(run$runtime$connections, session$runtime$connections)
  expect_equal(run$step_states$warmup$status, "succeeded")
  expect_equal(run$step_states$dialogue$status, "awaiting_approval")
  expect_equal(run$step_states$report$status, "pending")
  expect_equal(
    run$artifact("costorm.warmup")@content$state$expert_ids,
    "expert.workflow"
  )

  session$add_turn(
    speaker = "User",
    role = "user",
    text = "What did the panel learn?"
  )
  pending <- Filter(
    \(approval) identical(approval$status, "pending"),
    run$approvals
  )
  approval_id <- pending[[1]]$approval_id
  tempest_run_record_approval(run, approval_id)

  expect_equal(run$status, "succeeded")
  expect_equal(
    run$artifact("costorm.dialogue")@content$state$transcript_turns,
    1L
  )
  expect_equal(
    run$artifact("report_md")@content,
    "A deterministic Co-STORM report."
  )
  expect_identical(test_session_workflow_run(session), run)

  snapshot <- tempest_session_snapshot(session)
  expect_equal(
    intersect(names(snapshot), c("artifact_catalog", "workflow_run")),
    character()
  )
  expect_identical(snapshot$report_md, "A deterministic Co-STORM report.")
  bundle <- file.path(withr::local_tempdir(), "costorm-session")
  tempest_session_save(session, bundle)
  expect_equal(file.exists(file.path(bundle, "workflow_run.json")), FALSE)
  session <- tempest_session_resume(bundle, config = config)
  expect_null(test_session_workflow_run(session))
  expect_identical(
    tempest_session_report_md(session),
    "A deterministic Co-STORM report."
  )
})

test_that("Co-STORM adapter rejects session and run scope mismatches", {
  skip_if_not_installed("ellmer")
  fake_session_chat <- function() {
    list(
      chat = function(prompt, ...) "",
      chat_structured = function(...) list(),
      register_tools = function(...) invisible(NULL)
    )
  }
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_session_chat()
    }
  )
  session <- tempest_session(
    "Scoped Co-STORM",
    config = config,
    experts = list(test_expert(expert_id = "expert.session"))
  )
  run <- tempest_costorm_workflow_run(session, verbose = FALSE)
  adapter <- tempest_costorm_workflow_adapter(session, verbose = FALSE)
  context <- list(
    source_store = session$workspace,
    artifact_catalog = test_session_artifact_catalog(session)
  )

  original_experts <- run$experts
  run$experts <- tempest:::tempest_run_expert_map(list(
    test_expert(expert_id = "expert.other")
  ))
  expect_error(
    adapter("dialogue", context, run),
    "expert pool must match",
    class = "tempest_builtin_workflow_error"
  )
  run$experts <- original_experts

  original_runtime <- run$runtime
  run$runtime <- tempest_runtime()
  expect_error(
    adapter("dialogue", context, run),
    "runtime must match",
    class = "tempest_builtin_workflow_error"
  )
  run$runtime <- original_runtime

  run$connection_permissions <- list(other = character())
  expect_error(
    adapter("dialogue", context, run),
    "connection permissions must match",
    class = "tempest_builtin_workflow_error"
  )
})
