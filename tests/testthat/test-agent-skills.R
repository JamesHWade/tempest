test_that("Tempest ships a complete custom-workflow Agent Skill suite", {
  skills <- tempest_agent_skills()

  expect_named(
    skills,
    c(
      "build-tempest-workflow",
      "design-tempest-workflow",
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

test_that("tempest_install_agent_skills installs selected skills", {
  destination <- file.path(withr::local_tempdir(), "skills")

  installed <- tempest_install_agent_skills(
    destination,
    skills = "design-tempest-workflow"
  )

  expect_named(installed, "design-tempest-workflow")
  expect_equal(file.exists(file.path(installed, "SKILL.md")), TRUE)
  expect_equal(
    file.exists(file.path(installed, "references", "workflow-contracts.md")),
    TRUE
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
