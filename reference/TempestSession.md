# TempestSession

TempestSession

TempestSession

## Details

Maintains state for a Co-STORM session: multi-agent dialog, mind map,
sources, and report artifacts.

## Public fields

- `topic`:

  The research topic.

- `title`:

  The report title.

- `config`:

  A `TempestConfig` object.

- `store`:

  A `SourceStore` object.

- `retriever`:

  A `TempestRetriever` object.

- `personas`:

  List of expert personas.

- `expert_session_manager`:

  Manages expert chat sessions.

- `chats`:

  List of chat objects for each role.

- `transcript`:

  List of dialog turns.

- `mindmap`:

  The mind map data structure.

- `artifacts`:

  Environment of report artifacts.

## Methods

### Public methods

- [`TempestSession$new()`](#method-TempestSession-new)

- [`TempestSession$add_turn()`](#method-TempestSession-add_turn)

- [`TempestSession$transcript_markdown()`](#method-TempestSession-transcript_markdown)

- [`TempestSession$get_persona_names()`](#method-TempestSession-get_persona_names)

- [`TempestSession$get_persona_descriptions()`](#method-TempestSession-get_persona_descriptions)

- [`TempestSession$decide_next()`](#method-TempestSession-decide_next)

- [`TempestSession$update_mindmap()`](#method-TempestSession-update_mindmap)

- [`TempestSession$mindmap_markdown()`](#method-TempestSession-mindmap_markdown)

- [`TempestSession$extract_facts()`](#method-TempestSession-extract_facts)

- [`TempestSession$find_expert_index()`](#method-TempestSession-find_expert_index)

- [`TempestSession$step()`](#method-TempestSession-step)

- [`TempestSession$warmup()`](#method-TempestSession-warmup)

- [`TempestSession$report()`](#method-TempestSession-report)

- [`TempestSession$clone()`](#method-TempestSession-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TempestSession.

#### Usage

    TempestSession$new(
      topic,
      config = tempest_config(),
      n_experts = 3,
      personas = NULL
    )

#### Arguments

- `topic`:

  The research topic.

- `config`:

  A `TempestConfig` object.

- `n_experts`:

  Number of expert agents.

- `personas`:

  Optional list of pre-generated personas. If NULL, personas are
  generated automatically using
  [`tempest_generate_personas()`](tempest_generate_personas.md).

------------------------------------------------------------------------

### Method `add_turn()`

Add a turn to the transcript.

#### Usage

    TempestSession$add_turn(speaker, role = c("user", "assistant"), text)

#### Arguments

- `speaker`:

  Speaker name.

- `role`:

  Role: "user" or "assistant".

- `text`:

  The text content.

------------------------------------------------------------------------

### Method `transcript_markdown()`

Get the transcript as markdown.

#### Usage

    TempestSession$transcript_markdown(max_turns = 50)

#### Arguments

- `max_turns`:

  Maximum turns to include.

#### Returns

Markdown string.

------------------------------------------------------------------------

### Method `get_persona_names()`

Get persona names for agent routing.

#### Usage

    TempestSession$get_persona_names()

#### Returns

Character vector of persona names.

------------------------------------------------------------------------

### Method `get_persona_descriptions()`

Build persona descriptions for moderator context.

#### Usage

    TempestSession$get_persona_descriptions()

#### Returns

A formatted string describing all personas.

------------------------------------------------------------------------

### Method `decide_next()`

Decide which agent should respond next.

#### Usage

    TempestSession$decide_next(user_input)

#### Arguments

- `user_input`:

  The user's input.

#### Returns

A decision object with next_agent and instruction.

------------------------------------------------------------------------

### Method `update_mindmap()`

Update the mind map based on new exchange.

#### Usage

    TempestSession$update_mindmap(last_exchange)

#### Arguments

- `last_exchange`:

  The latest exchange text.

------------------------------------------------------------------------

### Method `mindmap_markdown()`

Get the mind map as markdown.

#### Usage

    TempestSession$mindmap_markdown()

#### Returns

Markdown string.

------------------------------------------------------------------------

### Method `extract_facts()`

Extract facts from text into the store.

#### Usage

    TempestSession$extract_facts(text)

#### Arguments

- `text`:

  Text containing factual claims.

------------------------------------------------------------------------

### Method `find_expert_index()`

Find expert index by persona name.

#### Usage

    TempestSession$find_expert_index(name)

#### Arguments

- `name`:

  The agent name to look up.

#### Returns

Index of the expert, or NULL if not found.

------------------------------------------------------------------------

### Method [`step()`](https://rdrr.io/r/stats/step.html)

Process one step of the conversation.

#### Usage

    TempestSession$step(user_input)

#### Arguments

- `user_input`:

  User's input message.

#### Returns

A list with speaker, answer, tool_calls, and mindmap_md.

------------------------------------------------------------------------

### Method `warmup()`

Run a warmup phase where each expert researches their initial questions.
This primes the knowledge base with foundational research before
interactive Q&A.

#### Usage

    TempestSession$warmup(verbose = TRUE)

#### Arguments

- `verbose`:

  If TRUE, prints progress messages.

#### Returns

A list with results from each expert's warmup.

------------------------------------------------------------------------

### Method `report()`

Generate a report from the session.

#### Usage

    TempestSession$report(
      style = c("technical", "executive"),
      include_references = TRUE
    )

#### Arguments

- `style`:

  Report style: "technical" or "executive".

- `include_references`:

  Include references section.

#### Returns

Markdown report string.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TempestSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
