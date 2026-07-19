test_that("Tempest ships a complete Agent Skill suite", {
  skills <- tempest_agent_skills()

  expect_named(
    skills,
    c(
      "build-tempest-workflow",
      "conduct-storm-research",
      "design-tempest-workflow",
      "use-tempest-research",
      "verify-tempest-workflow"
    )
  )
  expect_equal(
    file.exists(file.path(skills, "SKILL.md")),
    rep(TRUE, length(skills))
  )
  expect_equal(
    file.exists(file.path(skills, "agents", "openai.yaml")),
    rep(TRUE, length(skills))
  )
  expect_equal(
    unname(lengths(lapply(skills, list.files, recursive = TRUE)) >= 3L),
    rep(TRUE, length(skills))
  )
})

test_that("portable research skill includes host and mode contracts", {
  skill <- tempest_agent_skills()[["conduct-storm-research"]]
  references <- file.path(
    skill,
    "references",
    c(
      "storm-protocol.md",
      "costorm-protocol.md",
      "host-contract.md",
      "shinychat.md"
    )
  )

  expect_equal(file.exists(references), rep(TRUE, length(references)))
  host_contract <- paste(readLines(references[[3]]), collapse = "\n")
  expect_match(host_contract, 'btw_tools\\("skills"\\)')
  expect_match(host_contract, "Shinychat")
  shinychat_contract <- paste(readLines(references[[4]]), collapse = "\n")
  expect_match(shinychat_contract, 'btw::btw_tools\\("skills"\\)')
  expect_match(shinychat_contract, "chat_mod_server")
  instructions <- paste(
    readLines(file.path(skill, "SKILL.md")),
    collapse = "\n"
  )
  expect_no_match(instructions, "tempest_[a-z_]+\\(")
})

test_that("bundled Agent Skill references resolve", {
  skills <- tempest_agent_skills()

  for (skill in skills) {
    instructions <- paste(
      readLines(file.path(skill, "SKILL.md")),
      collapse = "\n"
    )
    matches <- gregexpr(
      "references/[a-z0-9-]+[.]md",
      instructions,
      perl = TRUE
    )
    references <- unique(regmatches(instructions, matches)[[1]])
    expect_equal(
      file.exists(file.path(skill, references)),
      rep(TRUE, length(references))
    )
  }
})

test_that("tempest_install_agent_skills installs selected skills", {
  destination <- file.path(withr::local_tempdir(), "skills")

  installed <- tempest_install_agent_skills(
    destination,
    skills = "use-tempest-research"
  )

  expect_named(installed, "use-tempest-research")
  expect_equal(file.exists(file.path(installed, "SKILL.md")), TRUE)
  expect_equal(
    file.exists(file.path(installed, "references", "host-integration.md")),
    TRUE
  )
})

test_that("tempest_install_agent_skills installs the complete suite", {
  destination <- file.path(withr::local_tempdir(), "skills")

  installed <- tempest_install_agent_skills(destination)

  expect_named(installed, names(tempest_agent_skills()))
  expect_equal(
    file.exists(file.path(installed, "SKILL.md")),
    rep(TRUE, length(installed))
  )
})

test_that("tempest_install_agent_skills refuses unknown or existing skills", {
  destination <- file.path(withr::local_tempdir(), "skills")

  expect_snapshot(
    tempest_install_agent_skills(destination, skills = "missing-skill"),
    error = TRUE
  )
  expect_error(
    tempest_install_agent_skills(destination, skills = "missing-skill"),
    class = "tempest_agent_skill_error"
  )

  tempest_install_agent_skills(
    destination,
    skills = "verify-tempest-workflow"
  )
  expect_snapshot(
    tempest_install_agent_skills(
      destination,
      skills = "verify-tempest-workflow"
    ),
    error = TRUE
  )
})

test_that("tempest_install_agent_skills replaces only requested skills", {
  destination <- file.path(withr::local_tempdir(), "skills")
  installed <- tempest_install_agent_skills(
    destination,
    skills = "build-tempest-workflow"
  )
  stale <- file.path(installed, "stale.txt")
  writeLines("stale", stale)

  replaced <- tempest_install_agent_skills(
    destination,
    skills = "build-tempest-workflow",
    overwrite = TRUE
  )

  expect_equal(file.exists(stale), FALSE)
  expect_equal(file.exists(file.path(replaced, "SKILL.md")), TRUE)
})
