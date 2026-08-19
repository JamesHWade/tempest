# Bundled Agent Skill discovery and installation

tempest_agent_skill_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_agent_skill_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_supported_agent_skills <- c(
  "conduct-storm-research",
  "use-tempest-research"
)

#' Discover and install bundled Agent Skills
#'
#' `tempest_agent_skills()` lists the two supported research Agent Skill
#' directories shipped with Tempest. These `SKILL.md` bundles guide agents
#' through using the STORM and Co-STORM product APIs or conducting the portable
#' STORM protocol.
#'
#' @return `tempest_agent_skills()` returns a named character vector of bundled
#'   skill directories.
#' @examples
#' tempest_agent_skills()
#' @export
tempest_agent_skills <- function() {
  root <- tempest_pkg_file("skills")
  if (!nzchar(root) || !dir.exists(root)) {
    tempest_agent_skill_abort(
      "The installed {.pkg tempest} package does not contain bundled Agent Skills."
    )
  }

  paths <- file.path(root, tempest_supported_agent_skills)
  valid <- dir.exists(paths) & file.exists(file.path(paths, "SKILL.md"))
  if (!all(valid)) {
    tempest_agent_skill_abort(
      "The installed {.pkg tempest} package is missing a supported Agent Skill."
    )
  }

  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  stats::setNames(paths, tempest_supported_agent_skills)
}

#' @param path Destination Agent Skill directory. For example,
#'   `~/.codex/skills` for a user-level Codex installation or a project-local
#'   directory supported by the active agent.
#' @param skills Supported research skill names to install. `NULL` installs
#'   both supported skills.
#' @param overwrite Whether to replace existing directories with the same
#'   skill names.
#' @return `tempest_install_agent_skills()` invisibly returns a named character
#'   vector of installed skill directories.
#' @examples
#' \dontrun{
#' tempest_install_agent_skills("~/.codex/skills")
#' }
#' @rdname tempest_agent_skills
#' @export
tempest_install_agent_skills <- function(
  path,
  skills = NULL,
  overwrite = FALSE
) {
  if (!rlang::is_string(path) || is.na(path)) {
    tempest_agent_skill_abort(
      "{.arg path} must be a single non-missing string."
    )
  }
  path <- tempest_trim(path)
  if (!nzchar(path)) {
    tempest_agent_skill_abort("{.arg path} must not be empty.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    tempest_agent_skill_abort("{.arg overwrite} must be `TRUE` or `FALSE`.")
  }

  available <- tempest_agent_skills()
  if (is.null(skills)) {
    skills <- names(available)
  }
  if (!is.character(skills) || length(skills) == 0L || anyNA(skills)) {
    tempest_agent_skill_abort(
      "{.arg skills} must be a non-empty character vector without missing values."
    )
  }
  skills <- unique(tempest_trim(skills))
  if (any(!nzchar(skills))) {
    tempest_agent_skill_abort("{.arg skills} cannot contain empty names.")
  }
  unknown <- setdiff(skills, names(available))
  if (length(unknown) > 0L) {
    tempest_agent_skill_abort(c(
      "Unknown bundled Agent Skill: {.val {unknown[[1]]}}.",
      i = "Available skills: {.val {names(available)}}."
    ))
  }

  destination <- path.expand(path)
  if (file.exists(destination) && !dir.exists(destination)) {
    tempest_agent_skill_abort(
      "{.arg path} must be a directory, not an existing file: {.path {destination}}."
    )
  }
  targets <- file.path(destination, skills)
  existing <- file.exists(targets)
  if (any(existing) && !overwrite) {
    existing_skill <- skills[[which(existing)[[1]]]]
    tempest_agent_skill_abort(c(
      "Refusing to replace existing Agent Skill {.val {existing_skill}}.",
      i = "Use {.code overwrite = TRUE} to replace requested skills."
    ))
  }

  if (!dir.exists(destination)) {
    created <- dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(destination)) {
      tempest_agent_skill_abort(
        "Could not create Agent Skill directory: {.path {destination}}."
      )
    }
  }
  destination <- normalizePath(
    destination,
    winslash = "/",
    mustWork = TRUE
  )
  targets <- file.path(destination, skills)

  staging <- tempfile(".tempest-agent-skills-", tmpdir = destination)
  if (!dir.create(staging, showWarnings = FALSE)) {
    tempest_agent_skill_abort(
      "Could not create an Agent Skill staging directory in {.path {destination}}."
    )
  }
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  for (skill in skills) {
    tryCatch(
      fs::dir_copy(available[[skill]], file.path(staging, skill)),
      error = function(error) {
        tempest_agent_skill_abort(
          "Could not stage bundled Agent Skill {.val {skill}}.",
          parent = error
        )
      }
    )
  }

  for (skill in skills) {
    target <- file.path(destination, skill)
    staged <- file.path(staging, skill)
    if (file.exists(target)) {
      unlink(target, recursive = TRUE, force = TRUE)
      if (file.exists(target)) {
        tempest_agent_skill_abort(
          "Could not replace existing Agent Skill: {.path {target}}."
        )
      }
    }
    if (!file.rename(staged, target)) {
      tryCatch(
        fs::dir_copy(staged, target),
        error = function(error) {
          tempest_agent_skill_abort(
            "Could not install bundled Agent Skill {.val {skill}}.",
            parent = error
          )
        }
      )
      unlink(staged, recursive = TRUE, force = TRUE)
    }
  }

  installed <- normalizePath(targets, winslash = "/", mustWork = TRUE)
  invisible(stats::setNames(installed, skills))
}
