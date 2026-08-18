test_that("research experts roundtrip through their product record", {
  expert <- tempest_expert(
    "expert.one",
    "Expert",
    "Analyst",
    "Analyzes evidence.",
    "Be precise."
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  restored <- tempest:::tempest_expert_profile_from_data(record)

  expect_identical(restored@expert_id, expert@expert_id)
  expect_identical(
    record$fingerprint,
    "bea459473197dde550a654806d2e09c969179752d95f8658295f651d27fb9b71"
  )
})

test_that("expert rows use one exact fingerprint-bound writer shape", {
  expert <- tempest_expert(
    "expert.writer",
    "Writer expert",
    "Researcher",
    "Reviews evidence.",
    "Preserve uncertainty.",
    focus_areas = c("evidence", "policy"),
    selection_metadata = list(region = "US"),
    initial_work_items = "Map the evidence",
    metadata = list(owner = "research")
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  array_fields <- c(
    "focus_areas",
    "skill_ids",
    "skill_versions",
    "required_capability_ids",
    "optional_capability_ids",
    "initial_work_items",
    "initial_questions"
  )

  expect_identical(
    names(record),
    c(tempest:::tempest_research_expert_fields(), "fingerprint")
  )
  expect_identical(
    vapply(record[array_fields], is.list, logical(1)),
    stats::setNames(rep(TRUE, length(array_fields)), array_fields)
  )
  expect_identical(
    unname(lapply(record[array_fields], attributes)),
    rep(list(NULL), length(array_fields))
  )
  expect_null(record$model_policy_ref)
  expect_identical(names(record$selection_metadata), "region")
  expect_identical(names(record$metadata), "owner")
  expect_identical(
    tempest:::tempest_product_record_hash(record[-length(record)]),
    record$fingerprint
  )

  empty_record <- tempest:::tempest_expert_profile_record(tempest_expert(
    "expert.empty-maps",
    "Empty maps",
    "Researcher",
    "Reviews evidence.",
    "Preserve uncertainty."
  ))
  expect_identical(names(empty_record$selection_metadata), character())
  expect_identical(names(empty_record$metadata), character())
})

test_that("expert row decoding rejects noncanonical raw variants", {
  expert <- tempest_expert(
    "expert.strict-wire",
    "Strict expert",
    "Researcher",
    "Reviews evidence.",
    "Preserve uncertainty.",
    focus_areas = c("evidence", "policy")
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  variants <- list(
    model_policy_na = within(record, model_policy_ref <- NA_character_),
    padded_scalar = within(record, name <- " Strict expert "),
    duplicated_array = within(
      record,
      focus_areas <- list("evidence", "evidence")
    ),
    padded_array = within(record, focus_areas <- list(" evidence", "policy")),
    classed_scalar = within(
      record,
      name <- structure("Strict expert", class = "test_string")
    ),
    classed_array = within(
      record,
      focus_areas <- list(
        structure("evidence", class = "test_string"),
        "policy"
      )
    )
  )

  for (variant in variants) {
    expect_error(
      tempest:::tempest_expert_profile_from_data(variant),
      class = "tempest_research_expert_error"
    )
  }

  reversed <- record[rev(seq_along(record))]
  expect_error(
    tempest:::tempest_expert_profile_from_data(reversed),
    class = "tempest_research_expert_error"
  )

  padded <- record
  padded$name <- " Strict expert "
  padded$fingerprint <- tempest:::tempest_product_record_hash(
    padded[-length(padded)]
  )
  duplicated <- record
  duplicated$focus_areas <- list("evidence", "evidence")
  duplicated$fingerprint <- tempest:::tempest_product_record_hash(
    duplicated[-length(duplicated)]
  )
  padded_array <- record
  padded_array$focus_areas <- list(" evidence", "policy")
  padded_array$fingerprint <- tempest:::tempest_product_record_hash(
    padded_array[-length(padded_array)]
  )
  atomic_array <- record
  atomic_array$focus_areas <- c("evidence", "policy")
  atomic_array$fingerprint <- tempest:::tempest_product_record_hash(
    atomic_array[-length(atomic_array)]
  )
  for (variant in list(padded, duplicated, padded_array, atomic_array)) {
    expect_error(
      tempest:::tempest_expert_profile_from_data(variant),
      class = "tempest_research_expert_error"
    )
  }
})

test_that("expert row arrays reject non-current atomic coercions", {
  expert <- tempest_expert(
    "expert.strict-row",
    "Strict expert",
    "Researcher",
    "Reviews evidence.",
    "Preserve uncertainty."
  )
  record <- tempest:::tempest_expert_profile_record(expert)
  record$focus_areas <- numeric()
  record$fingerprint <- tempest:::tempest_expert_profile_fingerprint(record)

  expect_error(
    tempest:::tempest_expert_profile_from_data(record),
    class = "tempest_research_expert_error"
  )
})
