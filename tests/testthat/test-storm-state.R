test_that("STORM product state starts with an exact empty stage schema", {
  state <- tempest:::tempest_storm_state("Lithium batteries")

  expect_identical(
    names(state),
    c(
      "schema_version",
      "topic",
      "title",
      "requested_steps",
      "perspectives",
      "experts",
      "draft_outline",
      "outline",
      "lead_section",
      "draft_md",
      "report_md",
      "references",
      "stage_records",
      "completed_stages"
    )
  )
  expect_identical(state$schema_version, 4L)
  expect_identical(state$title, state$topic)
  expect_identical(
    state$requested_steps,
    c("perspectives", "research", "outline", "write", "polish")
  )
  expect_length(state$perspectives, 0L)
  expect_length(state$experts, 0L)
  expect_null(state$outline)
  expect_null(state$report_md)
  expect_length(state$references, 0L)
  expect_length(state$stage_records, 0L)
  expect_length(state$completed_stages, 0L)
})

test_that("STORM product state permits legitimate partial stage results", {
  expert <- tempest_expert(
    expert_id = "expert.technical",
    name = "Dr. Tech",
    title = "Engineer",
    description = "Battery engineering.",
    instructions = "Explain technical tradeoffs."
  )
  state <- tempest:::tempest_storm_state(
    topic = "Lithium batteries",
    title = "Lithium battery systems",
    perspectives = list(list(
      name = "Technical",
      description = "Technology",
      key_questions = c("How do cells work?", "What limits lifetime?")
    )),
    experts = list(expert),
    completed_stages = "perspectives"
  )
  state$draft_outline <- list(
    title = state$title,
    sections = list(list(title = "Overview", summary = "Summary"))
  )
  state$completed_stages <- c("perspectives", "research")

  validated <- tempest:::tempest_storm_state_validate(state)

  expect_identical(validated$title, "Lithium battery systems")
  expect_identical(validated$experts[[1]], expert)
  expect_identical(
    validated$completed_stages,
    c("perspectives", "research")
  )
  expect_null(validated$outline)
})

test_that("STORM product state records experts without runtime objects", {
  expert <- tempest_expert(
    expert_id = "expert.policy",
    name = "Dr. Policy",
    title = "Policy analyst",
    description = "Battery policy.",
    instructions = "Compare policy mechanisms.",
    focus_areas = c("standards", "incentives")
  )
  state <- tempest:::tempest_storm_state(
    "Battery policy",
    experts = list(expert),
    completed_stages = "perspectives"
  )

  record <- tempest:::tempest_storm_state_record(state)
  restored <- tempest:::tempest_storm_state_from_record(record)
  path <- withr::local_tempfile()
  tempest:::tempest_write_json(path, record)
  json_record <- tempest:::tempest_read_json_strict(path)
  json_restored <- tempest:::tempest_storm_state_from_record(json_record)

  expect_identical(names(record), names(state))
  expect_type(record$experts[[1]], "list")
  expect_identical(
    names(json_record$experts[[1]]$selection_metadata),
    character()
  )
  expect_identical(names(json_record$experts[[1]]$metadata), character())
  expect_s7_class(restored$experts[[1]], TempestExpertProfile)
  expect_identical(restored$experts[[1]]@expert_id, expert@expert_id)
  expect_identical(
    tempest:::tempest_storm_state_record(restored),
    record
  )
  expect_identical(
    tempest:::tempest_storm_state_record(json_restored),
    record
  )
})

test_that("STORM authority distinguishes explicit and generated experts", {
  program_id <- paste0("sha256:", strrep("a", 64L))
  perspectives <- list(list(
    name = "Technical",
    description = "Technology",
    key_questions = "How do cells work?"
  ))
  perspective_record <- tempest:::tempest_stage_record_succeed(
    tempest:::tempest_stage_record_start(
      "perspectives",
      program_id,
      attempt_id = "attempt-perspectives-explicit",
      started_at = "2026-08-16T00:00:00Z"
    ),
    tempest:::tempest_stage_output_reference(
      "state_field",
      c("title", "perspectives"),
      content_digest = tempest:::tempest_stage_state_output_digest(
        "perspectives",
        list(
          title = "Lithium batteries",
          perspectives = perspectives
        )
      )
    ),
    support_status = "unknown",
    completed_at = "2026-08-16T00:01:00Z"
  )
  explicit <- tempest_expert(
    expert_id = "expert.explicit",
    name = "Dr. Explicit",
    title = "Engineer",
    description = "Supplied by the caller.",
    instructions = "Review technical evidence."
  )
  explicit_state <- tempest:::tempest_storm_state(
    topic = "Lithium batteries",
    perspectives = perspectives,
    experts = list(explicit),
    stage_records = list(perspective_record),
    completed_stages = "perspectives"
  )

  expect_no_error(tempest:::tempest_stage_records_validate_storm_coverage(
    explicit_state$stage_records,
    explicit_state
  ))
  expect_no_error(tempest:::tempest_stage_records_validate_generated_experts(
    explicit_state$stage_records,
    explicit_state$experts
  ))

  generated <- tempest_expert(
    expert_id = "expert.generated-technical",
    name = "Dr. Generated",
    title = "Engineer",
    description = "Generated by the personas stage.",
    instructions = "Review technical evidence."
  )
  expect_error(
    tempest:::tempest_stage_records_validate_generated_experts(
      list(perspective_record),
      list(generated)
    ),
    class = "tempest_stage_record_error"
  )
  persona_record <- tempest:::tempest_stage_record_succeed(
    tempest:::tempest_stage_record_start(
      "personas",
      program_id,
      attempt_id = "attempt-personas-generated",
      started_at = "2026-08-16T00:01:00Z"
    ),
    tempest:::tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = tempest:::tempest_stage_state_output_digest(
        "personas",
        list(generated)
      )
    ),
    support_status = "unknown",
    completed_at = "2026-08-16T00:02:00Z"
  )
  expect_no_error(tempest:::tempest_stage_records_validate_generated_experts(
    list(perspective_record, persona_record),
    list(generated)
  ))
})

test_that("STORM product-state records cancel running durable copies", {
  program_id <- paste0("sha256:", strrep("a", 64L))
  running <- tempest:::tempest_stage_record_start(
    "perspectives",
    program_id,
    attempt_id = "attempt-running-state",
    started_at = "2026-08-16T00:00:00Z"
  )
  state <- tempest:::tempest_storm_state(
    "Running stage",
    stage_records = list(running)
  )
  live_data <- tempest:::tempest_stage_record_data(running)

  durable <- tempest:::tempest_storm_state_record(state)

  expect_identical(state$stage_records[[1]]@status, "running")
  expect_identical(
    tempest:::tempest_stage_record_data(state$stage_records[[1]]),
    live_data
  )
  expect_identical(durable$stage_records[[1]]$status, "cancelled")
  expect_identical(
    durable$stage_records[[1]]$failure_class,
    "tempest_stage_cancelled"
  )

  durable$stage_records <- tempest:::tempest_stage_records_data(list(running))
  expect_error(
    tempest:::tempest_storm_state_from_record(durable),
    class = "tempest_storm_state_restore_error"
  )
})

test_that("STORM stage records bind exact non-NULL state fields", {
  program_id <- paste0("sha256:", strrep("b", 64L))
  outline <- list(title = "Present stage output")
  started <- tempest:::tempest_stage_record_start(
    "draft_outline",
    program_id,
    attempt_id = "attempt-draft-outline",
    started_at = "2026-08-16T00:00:00Z"
  )
  succeeded <- tempest:::tempest_stage_record_succeed(
    started,
    tempest:::tempest_stage_output_reference(
      "state_field",
      "draft_outline",
      content_digest = tempest:::tempest_stage_state_output_digest(
        "draft_outline",
        outline
      )
    ),
    support_status = "unknown",
    completed_at = "2026-08-16T00:01:00Z"
  )

  expect_error(
    tempest:::tempest_storm_state(
      "Missing stage output",
      stage_records = list(succeeded)
    ),
    class = "tempest_storm_state_error"
  )
  expect_no_error(tempest:::tempest_storm_state(
    "Present stage output",
    draft_outline = outline,
    stage_records = list(succeeded)
  ))

  expect_error(
    tempest:::tempest_stage_record_succeed(
      started,
      tempest:::tempest_stage_output_reference(
        "content_digest",
        paste0("sha256:", strrep("c", 64L))
      ),
      support_status = "unknown",
      completed_at = "2026-08-16T00:01:00Z"
    ),
    class = "tempest_stage_record_error"
  )
})

test_that("STORM requested steps are immutable canonical workflow identity", {
  state <- tempest:::tempest_storm_state(
    "Partial research",
    requested_steps = c("polish", "research", "write")
  )

  expect_identical(state$requested_steps, c("research", "write", "polish"))
  expect_error(
    tempest:::tempest_storm_state(
      "Duplicate request",
      requested_steps = c("research", "research")
    ),
    class = "tempest_storm_state_error"
  )
  expect_error(
    tempest:::tempest_storm_state(
      "Unrequested completion",
      requested_steps = "research",
      report_md = "# Unrequested completion",
      completed_stages = "polish"
    ),
    class = "tempest_storm_state_error"
  )
})

test_that("STORM full completion is distinct from requested-step completion", {
  partial <- tempest:::tempest_storm_state(
    "Partial research",
    requested_steps = "research",
    completed_stages = "research"
  )
  full <- tempest:::tempest_storm_state(
    "Full research",
    draft_outline = list(title = "Full research"),
    outline = list(title = "Full research"),
    draft_md = "# Full research",
    report_md = "# Full research\n\nComplete.\n",
    completed_stages = c(
      "perspectives",
      "research",
      "outline",
      "write",
      "polish"
    )
  )

  expect_identical(tempest:::tempest_storm_state_is_complete(partial), FALSE)
  expect_identical(tempest:::tempest_storm_state_is_complete(full), TRUE)
})

test_that("STORM product state survives a JSON record round trip", {
  skip_if_not_installed("jsonlite")
  state <- tempest:::tempest_storm_state(
    topic = "Grid storage",
    perspectives = list(list(
      name = "Operations",
      description = "Grid operations",
      key_questions = c("How is storage dispatched?", "Who operates it?")
    )),
    draft_outline = list(
      title = "Grid storage",
      sections = list(list(title = "Dispatch", summary = "Operations"))
    ),
    outline = list(
      title = "Grid storage",
      sections = list(list(title = "Dispatch", summary = "Operations"))
    ),
    draft_md = "# Grid storage",
    references = list(list(id = "S123", url = "https://example.org/grid")),
    completed_stages = c("perspectives", "research", "outline", "write")
  )
  record <- tempest:::tempest_storm_state_record(state)
  json <- jsonlite::toJSON(
    record,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  decoded <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  restored <- tempest:::tempest_storm_state_from_record(decoded)

  expect_identical(restored, state)
  expect_identical(restored$references[[1]]$id, "S123")
  expect_identical(
    as.character(jsonlite::toJSON(
      tempest:::tempest_storm_state_record(restored),
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )),
    as.character(json)
  )
})

test_that("STORM product-state records canonicalize missing source fields", {
  skip_if_not_installed("jsonlite")
  state <- tempest:::tempest_storm_state(
    topic = "Source metadata",
    references = list(list(
      id = "S123",
      url = "https://example.org/source",
      title = "Source",
      snippet = NA_character_,
      content_text = NA_character_,
      fetched_at = NA_character_,
      content_hash = NA_character_,
      meta = list()
    ))
  )
  record <- tempest:::tempest_storm_state_record(state)
  json <- jsonlite::toJSON(
    record,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  restored <- tempest:::tempest_storm_state_from_record(
    jsonlite::fromJSON(json, simplifyVector = FALSE)
  )
  restored_json <- jsonlite::toJSON(
    tempest:::tempest_storm_state_record(restored),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )

  expect_null(record$references[[1]]$snippet)
  expect_identical(restored, state)
  expect_identical(as.character(restored_json), as.character(json))
})

test_that("STORM product state enforces ordered coherent subset stages", {
  outline <- list(
    title = "Battery safety",
    sections = list(list(title = "Hazards", summary = "Failure modes"))
  )
  valid <- tempest:::tempest_storm_state(
    "Battery safety",
    draft_outline = outline,
    outline = outline,
    draft_md = "# Battery safety",
    report_md = "# Battery safety\n\nComplete.\n",
    completed_stages = c("outline", "write", "polish")
  )

  expect_identical(
    valid$completed_stages,
    c("outline", "write", "polish")
  )
  expect_no_error(tempest:::tempest_storm_state(
    "Battery safety",
    outline = outline,
    draft_md = "# Battery safety",
    completed_stages = "write"
  ))
  expect_error(
    tempest:::tempest_storm_state(
      "Battery safety",
      draft_outline = outline,
      outline = outline,
      draft_md = "# Battery safety",
      report_md = "# Battery safety report",
      completed_stages = c("polish", "write")
    ),
    class = "tempest_storm_state_error"
  )
  expect_error(
    tempest:::tempest_storm_state(
      "Battery safety",
      draft_outline = outline,
      completed_stages = "outline"
    ),
    class = "tempest_storm_state_error"
  )
  expect_error(
    tempest:::tempest_storm_state(
      "Battery safety",
      draft_md = "# Battery safety",
      completed_stages = "write"
    ),
    class = "tempest_storm_state_error"
  )
  expect_error(
    tempest:::tempest_storm_state(
      "Battery safety",
      report_md = "# Battery safety report",
      completed_stages = "polish"
    ),
    class = "tempest_storm_state_error"
  )
})

test_that("STORM product state rejects schema drift and runtime values", {
  state <- tempest:::tempest_storm_state("Battery safety")
  state$claims <- list()
  expect_error(
    tempest:::tempest_storm_state_validate(state),
    class = "tempest_storm_state_error"
  )

  state <- tempest:::tempest_storm_state("Battery safety")
  state <- state[names(state) != "references"]
  expect_error(
    tempest:::tempest_storm_state_validate(state),
    class = "tempest_storm_state_error"
  )

  state <- tempest:::tempest_storm_state("Battery safety")
  state$title <- 42
  expect_error(
    tempest:::tempest_storm_state_validate(state),
    class = "tempest_storm_state_error"
  )

  state <- tempest:::tempest_storm_state("Battery safety")
  state$artifacts <- new.env(parent = emptyenv())
  expect_error(
    tempest:::tempest_storm_state_record(state),
    class = "tempest_storm_state_error"
  )

  state <- tempest:::tempest_storm_state("Battery safety")
  state$perspectives <- list(list(tool = function() NULL))
  expect_error(
    tempest:::tempest_storm_state_validate(state),
    class = "tempest_storm_state_error"
  )

  state <- tempest:::tempest_storm_state("Battery safety")
  state$completed_stages <- c("research", "research")
  expect_error(
    tempest:::tempest_storm_state_validate(state),
    class = "tempest_storm_state_error"
  )

  record <- tempest:::tempest_storm_state_record(
    tempest:::tempest_storm_state("Battery safety")
  )
  whole_double_schema <- record
  whole_double_schema$schema_version <- 4
  expect_error(
    tempest:::tempest_storm_state_from_record(whole_double_schema),
    class = "tempest_storm_state_restore_error"
  )

  record$schema_version <- 1L
  record$stage_records <- NULL
  expect_error(
    tempest:::tempest_storm_state_from_record(record),
    class = "tempest_unsupported_format_error"
  )
})

test_that("STORM product state excludes evidence and arbitrary artifacts", {
  fields <- names(tempest:::tempest_storm_state("Battery safety"))
  contracts <- tempest:::tempest_stage_durable_output_contracts()

  expect_setequal(
    intersect(fields, c("claims", "claim_supports", "artifacts")),
    character()
  )
  expect_identical(
    contracts$verify_claim_support$kind,
    "claim_supports"
  )
})

test_that("returned STORM manifest equals the output-dir manifest", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  fixture <- storm_progress_fixture()
  output_root <- withr::local_tempdir()
  program_set <- tempest_program_set()

  result <- tempest_run(
    "T7 manifest persistence",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    program_set = program_set,
    steps = c("perspectives", "research"),
    output_dir = output_root,
    run_id = "t7-manifest-persistence",
    verbose = FALSE
  )
  returned <- tempest_research_manifest_record(result$manifest)
  persisted <- tempest:::tempest_read_json_strict(file.path(
    result$output_dir,
    "run_config.json"
  ))$research_manifest
  restored <- tempest:::tempest_load_run_artifacts(
    result$output_dir,
    config = fixture$config,
    program_set = program_set,
    run_id = "t7-manifest-persistence"
  )
  deputy_traces <- Filter(
    \(trace) identical(trace$trace_type, "deputy_run"),
    returned$traces
  )

  expect_identical(returned, persisted)
  expect_identical(returned$status, "running")
  expect_null(returned$deliverables$report_md)
  expect_null(result$state$report_md)
  expect_identical(
    tempest_research_manifest_record(restored$research_manifest),
    returned
  )
  expect_length(deputy_traces, 1L)
  if (length(deputy_traces) == 1L) {
    expect_identical(
      unlist(returned$runtime$deputy_run_ids, use.names = FALSE),
      deputy_traces[[1L]]$deputy_run_id
    )
    expect_identical(
      unlist(returned$runtime$deputy_session_ids, use.names = FALSE),
      deputy_traces[[1L]]$deputy_session_id
    )
  }
})

test_that("no-output STORM returns succeeded publication authority", {
  skip_if_not_installed("deputy")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("graft")
  fixture <- storm_progress_fixture()
  seal_calls <- 0L
  original_seal <- tempest:::tempest_research_workspace_seal
  local_mocked_bindings(
    tempest_research_workspace_seal = function(workspace, owner = NULL) {
      seal_calls <<- seal_calls + 1L
      original_seal(workspace, owner)
    }
  )

  result <- tempest_run(
    "T7 in-memory publication",
    config = fixture$config,
    retriever = fixture$retriever,
    n_experts = 1,
    max_questions_per_perspective = 1,
    program_set = tempest_program_set(),
    verbose = FALSE
  )
  authority <- tempest:::tempest_product_authority_validate(
    result$manifest,
    result$state$stage_records,
    result$workspace,
    report_md = result$report_md,
    report_reference = tempest:::tempest_persistence_report_reference(
      result$report_md
    ),
    config = fixture$config,
    experts = result$experts,
    product_state = result$state,
    require_publishable = TRUE
  )

  expect_null(result$output_dir)
  expect_identical(result$manifest@status, "succeeded")
  expect_identical(
    result$manifest@deliverables$report_md$status,
    "durable"
  )
  expect_identical(authority$publishable, TRUE)
  expect_identical(authority$status, "succeeded")
  expect_identical(seal_calls, 1L)
  expect_identical(
    tempest:::tempest_research_workspace_mutation_state(result$workspace),
    "sealed"
  )
  expect_error(
    result$workspace$upsert_retrieved_resource(fake_source(
      url = "https://example.org/sealed-storm",
      title = "Sealed STORM workspace",
      content_text = "Succeeded research workspaces are immutable."
    )),
    class = "tempest_research_workspace_error"
  )
})
