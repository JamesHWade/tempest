test_that("research experts roundtrip through their exact product record", {
  expert <- tempest_expert(
    name = "Expert",
    title = "Analyst",
    description = "Analyzes evidence.",
    instructions = "Be precise.",
    focus_areas = c("evidence", "policy"),
    initial_questions = "Where is the uncertainty?"
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  restored <- tempest:::tempest_expert_profile_from_data(record)

  expect_identical(restored, expert)
  expect_identical(
    tempest:::tempest_product_record_hash(record[-length(record)]),
    record$fingerprint
  )
  expect_identical(record$focus_areas, list("evidence", "policy"))
  expect_identical(
    record$initial_questions,
    list("Where is the uncertainty?")
  )
})

test_that("expert row decoding rejects noncanonical raw variants", {
  expert <- tempest_expert(
    name = "Strict expert",
    title = "Researcher",
    description = "Reviews evidence.",
    instructions = "Preserve uncertainty.",
    focus_areas = c("evidence", "policy")
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  variants <- list(
    within(record, name <- " Strict expert "),
    within(record, focus_areas <- list("evidence", "evidence")),
    within(record, focus_areas <- c("evidence", "policy")),
    record[rev(seq_along(record))]
  )

  for (variant in variants) {
    if (identical(names(variant), names(record))) {
      variant$fingerprint <- tempest:::tempest_product_record_hash(
        variant[-length(variant)]
      )
    }
    expect_error(
      tempest:::tempest_expert_profile_from_data(variant),
      class = "tempest_research_expert_error"
    )
  }
})

test_that("expert updates accept authored fields only", {
  expert <- tempest_expert(
    name = "Original expert",
    title = "Researcher",
    description = "Reviews original evidence.",
    instructions = "Preserve uncertainty."
  )

  updated <- tempest:::tempest_expert_update(
    expert,
    name = "Updated expert",
    focus_areas = "policy"
  )

  expect_identical(updated@name, "Updated expert")
  expect_identical(updated@focus_areas, "policy")
  expect_identical(updated@schema_version, 2L)
  expect_identical(identical(updated@expert_id, expert@expert_id), FALSE)

  invalid <- list(
    list(),
    list(expert_id = expert@expert_id),
    list(version = expert@version),
    list(schema_version = 2L),
    list(unknown = "value")
  )
  for (changes in invalid) {
    expect_error(
      do.call(tempest:::tempest_expert_update, c(list(expert), changes)),
      class = "tempest_research_expert_error"
    )
  }
})
