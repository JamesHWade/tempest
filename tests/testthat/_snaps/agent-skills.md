# tempest_install_agent_skills refuses unknown or existing skills

    Code
      tempest_install_agent_skills(destination, skills = "missing-skill")
    Condition
      Error in `tempest_agent_skill_abort()`:
      ! Unknown bundled Agent Skill: "missing-skill".
      i Available skills: "build-tempest-workflow", "conduct-storm-research", "design-tempest-workflow", "use-tempest-research", and "verify-tempest-workflow".

---

    Code
      tempest_install_agent_skills(destination, skills = "verify-tempest-workflow")
    Condition
      Error in `tempest_agent_skill_abort()`:
      ! Refusing to replace existing Agent Skill "verify-tempest-workflow".
      i Use `overwrite = TRUE` to replace requested skills.

