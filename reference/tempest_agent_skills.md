# Discover and install bundled Agent Skills

`tempest_agent_skills()` lists the portable Agent Skill directories
shipped with Tempest. These `SKILL.md` bundles guide agents through
using the built-in research workflows, conducting the portable STORM
protocol, and designing, building, or verifying a custom Tempest
workflow. They are distinct from
[`tempest_skill()`](https://jameshwade.github.io/tempest/reference/tempest_skill.md),
which creates a serializable procedure assigned to a Tempest expert
inside a workflow.

## Usage

``` r
tempest_agent_skills()

tempest_install_agent_skills(path, skills = NULL, overwrite = FALSE)
```

## Arguments

- path:

  Destination Agent Skill directory. For example, `~/.codex/skills` for
  a user-level Codex installation or a project-local directory supported
  by the active agent.

- skills:

  Bundled skill names to install. `NULL` installs all bundled skills.

- overwrite:

  Whether to replace existing directories with the same skill names.

## Value

`tempest_agent_skills()` returns a named character vector of bundled
skill directories.

`tempest_install_agent_skills()` invisibly returns a named character
vector of installed skill directories.

## Examples

``` r
tempest_agent_skills()
#>                                                   build-tempest-workflow 
#>  "/home/runner/work/_temp/Library/tempest/skills/build-tempest-workflow" 
#>                                                   conduct-storm-research 
#>  "/home/runner/work/_temp/Library/tempest/skills/conduct-storm-research" 
#>                                                  design-tempest-workflow 
#> "/home/runner/work/_temp/Library/tempest/skills/design-tempest-workflow" 
#>                                                     use-tempest-research 
#>    "/home/runner/work/_temp/Library/tempest/skills/use-tempest-research" 
#>                                                  verify-tempest-workflow 
#> "/home/runner/work/_temp/Library/tempest/skills/verify-tempest-workflow" 
if (FALSE) { # \dontrun{
tempest_install_agent_skills("~/.codex/skills")
} # }
```
