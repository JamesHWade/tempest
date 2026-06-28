# Create a chat object for a given role.

Create a chat object for a given role.

## Usage

``` r
tempest_make_chat(config, role, system_prompt = NULL, echo = "none")
```

## Arguments

- config:

  A `TempestConfig` object.

- role:

  Role name (e.g., "coordinator", "writer", "expert").

- system_prompt:

  Optional system prompt override.

- echo:

  Echo setting for the chat.

## Value

An ellmer-compatible Chat object.
