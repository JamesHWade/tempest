# Create the expert delegation tool

Creates one generic tool that resolves the manager's live roster by
exact stable expert id.

## Usage

``` r
tempest_create_expert_delegation_tool(session_manager, topic, experts = NULL)
```

## Arguments

- session_manager:

  An `ExpertSessionManager` instance.

- topic:

  The research topic (for context).

- experts:

  Optional selected expert profiles. These are validated for runtime
  composition, while calls resolve the manager's live roster.

## Value

An ellmer tool.
