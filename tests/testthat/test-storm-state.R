test_that("STORM product state starts with an exact empty stage schema", {
  state <- tempest:::tempest_storm_state("Lithium batteries")

  expect_identical(
    names(state),
    c(
      "schema_version",
      "topic",
      "title",
      "perspectives",
      "experts",
      "draft_outline",
      "outline",
      "lead_section",
      "draft_md",
      "report_md",
      "references",
      "completed_stages"
    )
  )
  expect_identical(state$schema_version, 1L)
  expect_identical(state$title, state$topic)
  expect_length(state$perspectives, 0L)
  expect_length(state$experts, 0L)
  expect_null(state$outline)
  expect_null(state$report_md)
  expect_length(state$references, 0L)
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

  expect_identical(names(record), names(state))
  expect_type(record$experts[[1]], "list")
  expect_s7_class(restored$experts[[1]], TempestExpertProfile)
  expect_identical(restored$experts[[1]]@expert_id, expert@expert_id)
  expect_equal(
    tempest:::tempest_storm_state_record(restored),
    record
  )
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
    report_md = "# Battery safety report",
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
  record$schema_version <- 2L
  expect_error(
    tempest:::tempest_storm_state_from_record(record),
    class = "tempest_storm_state_restore_error"
  )
})

test_that("STORM product state excludes evidence and arbitrary artifacts", {
  fields <- names(tempest:::tempest_storm_state("Battery safety"))

  expect_setequal(
    intersect(fields, c("claims", "citation_audit", "artifacts")),
    character()
  )
})
