# Create an Expert Subagent Tool

Creates an ellmer tool for a specific expert persona that can be called
by the moderator to delegate questions.

## Usage

``` r
tempest_create_expert_tool(persona, session_manager, topic)
```

## Arguments

- persona:

  A persona object from
  [`tempest_generate_personas()`](tempest_generate_personas.md).

- session_manager:

  An `ExpertSessionManager` instance.

- topic:

  The research topic (for context).

## Value

An ellmer tool.
