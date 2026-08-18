# tempest_install_agent_skills refuses unknown or existing skills

    Code
      tempest_install_agent_skills(destination, skills = "missing-skill")
    Condition
      Error in `tempest_agent_skill_abort()`:
      ! Unknown bundled Agent Skill: "missing-skill".
      i Available skills: "conduct-storm-research" and "use-tempest-research".

---

    Code
      tempest_install_agent_skills(destination, skills = "use-tempest-research")
    Condition
      Error in `tempest_agent_skill_abort()`:
      ! Refusing to replace existing Agent Skill "use-tempest-research".
      i Use `overwrite = TRUE` to replace requested skills.

