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
    "tempest_expert.properties",
    "tempest_session.public_methods",
    "tempest_session.active_bindings"
  )
  expect_named(contract, expected_sections)
  expect_length(contract$exports, 23L)
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

  expected_expert_properties <- c(
    "expert_id",
    "version",
    "name",
    "title",
    "description",
    "instructions",
    "focus_areas",
    "initial_questions",
    "schema_version"
  )
  expect_identical(
    contract[["tempest_expert.properties"]],
    expected_expert_properties
  )
  expert <- tempest_expert(
    name = "Contract Expert",
    title = "Contract specialist",
    description = "Exercises the installed expert profile contract.",
    instructions = "Preserve the exact authored profile."
  )
  expect_identical(S7::prop_names(expert), expected_expert_properties)

  session_generator <- get(
    "TempestSession",
    envir = asNamespace("tempest"),
    inherits = FALSE
  )
  expect_identical(
    sort(
      setdiff(
        names(session_generator$public_methods),
        c("initialize", "clone")
      ),
      method = "radix"
    ),
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
