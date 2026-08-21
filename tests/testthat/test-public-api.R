test_that("the current public API contract is exact", {
  lines <- readLines(
    test_path("fixtures", "public-api-current.txt"),
    warn = FALSE
  )
  section_rows <- grep("^\\[[^]]+\\]$", lines)
  section_names <- gsub("^\\[|\\]$", "", lines[section_rows])
  section_ends <- c(section_rows[-1L] - 1L, length(lines))
  contract <- stats::setNames(
    lapply(seq_along(section_rows), function(index) {
      start <- section_rows[[index]] + 1L
      end <- section_ends[[index]]
      if (start > end) {
        return(character())
      }
      lines[start:end]
    }),
    section_names
  )

  expected_sections <- c(
    "exports",
    "s3_methods",
    "formals.tempest_run",
    "formals.tempest_session_resume",
    "formals.tempest_expert",
    "formals.tempest_research_manifest",
    "formals.tempest_resource",
    "tempest_session.public_methods",
    "tempest_session.active_bindings"
  )
  expect_named(contract, expected_sections)
  expect_length(contract$exports, 56L)
  expect_identical(
    contract$exports,
    sort(unique(contract$exports), method = "radix")
  )
  expect_identical(
    sort(getNamespaceExports("tempest"), method = "radix"),
    contract$exports
  )

  methods <- getNamespaceInfo(asNamespace("tempest"), "S3methods")
  registrations <- if (nrow(methods) == 0L) {
    character()
  } else {
    sort(paste(methods[, 1L], methods[, 2L], sep = "."), method = "radix")
  }
  expect_identical(registrations, contract$s3_methods)

  formal_contracts <- grep("^formals[.]", names(contract), value = TRUE)
  for (section in formal_contracts) {
    function_name <- sub("^formals[.]", "", section)
    expect_identical(
      names(formals(getExportedValue("tempest", function_name))),
      contract[[section]],
      info = function_name
    )
  }

  session_generator <- get(
    "TempestSession",
    envir = asNamespace("tempest"),
    inherits = FALSE
  )
  expect_identical(
    sort(names(session_generator$public_methods), method = "radix"),
    contract[["tempest_session.public_methods"]]
  )
  expect_identical(
    sort(names(session_generator$active), method = "radix"),
    contract[["tempest_session.active_bindings"]]
  )

  retired_exports <- c(
    "tempest_agent_skills",
    "tempest_install_agent_skills",
    "tempest_okf_concepts",
    "tempest_okf_context",
    "tempest_okf_resources",
    "tempest_read_okf",
    "tempest_suggest_questions"
  )
  expect_identical(
    intersect(getNamespaceExports("tempest"), retired_exports),
    character()
  )
})

test_that("the 0.3 transition inventory maps the former surface exactly once", {
  transition <- utils::read.csv(
    test_path("fixtures", "public-api-transition-0.3.csv"),
    stringsAsFactors = FALSE
  )

  expect_named(
    transition,
    c("current_export", "disposition", "target_export")
  )
  expect_equal(nrow(transition), 63L)
  expect_identical(
    transition$current_export,
    sort(unique(transition$current_export), method = "radix")
  )
  expect_setequal(
    unique(transition$disposition),
    c("retain", "replace", "internalize", "delete")
  )

  retained <- transition$disposition == "retain"
  replaced <- transition$disposition == "replace"
  removed <- transition$disposition %in% c("internalize", "delete")
  expect_identical(
    transition$target_export[retained],
    transition$current_export[retained]
  )
  expect_equal(
    nzchar(transition$target_export[replaced]),
    rep(TRUE, sum(replaced))
  )
  expect_equal(transition$target_export[removed], rep("", sum(removed)))

  target <- sort(
    unique(transition$target_export[nzchar(transition$target_export)]),
    method = "radix"
  )
  expect_identical(
    target,
    c(
      "tempest_app",
      "tempest_claim_supports",
      "tempest_claims",
      "tempest_config",
      "tempest_expert",
      "tempest_graft_plan",
      "tempest_graft_schema",
      "tempest_knowledge",
      "tempest_promotion_bundle",
      "tempest_promotion_receipt",
      "tempest_read_promotion_bundle",
      "tempest_report",
      "tempest_run",
      "tempest_save_promotion_bundle",
      "tempest_session",
      "tempest_session_resume",
      "tempest_session_save",
      "tempest_sources",
      "tempest_trajectory_review"
    )
  )
})
