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

- `discourse_manager`:

  A `DiscourseManager` object (NULL when disabled).

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

- [`TempestSession$add_expert()`](#method-TempestSession-add_expert)

- [`TempestSession$retire_expert()`](#method-TempestSession-retire_expert)

- [`TempestSession$get_active_personas()`](#method-TempestSession-get_active_personas)

- [`TempestSession$check_and_expand_nodes()`](#method-TempestSession-check_and_expand_nodes)

- [`TempestSession$get_discussed_source_ids()`](#method-TempestSession-get_discussed_source_ids)

- [`TempestSession$find_undiscussed_sources()`](#method-TempestSession-find_undiscussed_sources)

- [`TempestSession$surface_unseen_information()`](#method-TempestSession-surface_unseen_information)

- [`TempestSession$reorganize_mindmap()`](#method-TempestSession-reorganize_mindmap)

- [`TempestSession$execute_turn_decision()`](#method-TempestSession-execute_turn_decision)

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

    TempestSession$step(user_input = NULL, auto = FALSE)

#### Arguments

- `user_input`:

  User's input message.

- `auto`:

  If TRUE and discourse manager is enabled, let the discourse manager
  decide.

#### Returns

A list with speaker, answer, and mindmap_md.

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
      include_references = TRUE,
      reorganize = TRUE
    )

#### Arguments

- `style`:

  Report style: "technical" or "executive".

- `include_references`:

  Include references section.

- `reorganize`:

  Whether to reorganize mind map before generating.

#### Returns

Markdown report string.

------------------------------------------------------------------------

### Method `add_expert()`

Add a new expert to the panel dynamically.

#### Usage

    TempestSession$add_expert(area, name = NULL)

#### Arguments

- `area`:

  The area of expertise needed.

- `name`:

  Optional name for the new expert.

#### Returns

The new persona (invisibly).

------------------------------------------------------------------------

### Method `retire_expert()`

Retire an expert from the panel.

#### Usage

    TempestSession$retire_expert(name)

#### Arguments

- `name`:

  The name of the expert to retire.

#### Returns

Logical indicating success.

------------------------------------------------------------------------

### Method `get_active_personas()`

Get active (non-retired) personas.

#### Usage

    TempestSession$get_active_personas()

#### Returns

List of active persona objects.

------------------------------------------------------------------------

### Method `check_and_expand_nodes()`

Check and expand oversized mind map nodes.

#### Usage

    TempestSession$check_and_expand_nodes()

------------------------------------------------------------------------

### Method `get_discussed_source_ids()`

Get source IDs that have been discussed in the transcript.

#### Usage

    TempestSession$get_discussed_source_ids()

#### Returns

Character vector of discussed source IDs.

------------------------------------------------------------------------

### Method `find_undiscussed_sources()`

Find sources that haven't been discussed yet.

#### Usage

    TempestSession$find_undiscussed_sources()

#### Returns

Character vector of undiscussed source IDs.

------------------------------------------------------------------------

### Method `surface_unseen_information()`

Generate questions about undiscussed sources.

#### Usage

    TempestSession$surface_unseen_information(max_questions = 3)

#### Arguments

- `max_questions`:

  Maximum questions to generate.

#### Returns

Character vector of questions, or NULL if none.

------------------------------------------------------------------------

### Method `reorganize_mindmap()`

Reorganize the mind map for clarity.

#### Usage

    TempestSession$reorganize_mindmap()

------------------------------------------------------------------------

### Method `execute_turn_decision()`

Execute a discourse manager turn decision.

#### Usage

    TempestSession$execute_turn_decision(decision)

#### Arguments

- `decision`:

  A turn decision from the discourse manager.

#### Returns

A list with speaker, answer, and mindmap_md.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TempestSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
