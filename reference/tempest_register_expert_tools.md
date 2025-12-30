# Register Expert Tools on a Chat

Registers expert subagent tools on a moderator chat, allowing it to
delegate questions to specialized experts.

## Usage

``` r
tempest_register_expert_tools(chat, personas, session_manager, topic)
```

## Arguments

- chat:

  An ellmer chat object (the moderator).

- personas:

  List of persona objects.

- session_manager:

  An `ExpertSessionManager` instance.

- topic:

  The research topic.

## Value

The chat object (invisibly).
