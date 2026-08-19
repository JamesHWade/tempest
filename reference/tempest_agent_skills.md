# Discover and install bundled Agent Skills

`tempest_agent_skills()` lists the two supported research Agent Skill
directories shipped with Tempest. These `SKILL.md` bundles guide agents
through using the STORM and Co-STORM product APIs or conducting the
portable STORM protocol.

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

  Supported research skill names to install. `NULL` installs both
  supported skills.

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
#>                                                  conduct-storm-research 
#> "/home/runner/work/_temp/Library/tempest/skills/conduct-storm-research" 
#>                                                    use-tempest-research 
#>   "/home/runner/work/_temp/Library/tempest/skills/use-tempest-research" 
if (FALSE) { # \dontrun{
tempest_install_agent_skills("~/.codex/skills")
} # }
```
