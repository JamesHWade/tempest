# TempestSession

Maintains state for a Co-STORM session: multi-agent dialog, mind map,
sources, and report artifacts.

## Public fields

- `topic`:

  The research topic.

- `title`:

  The report title.

- `config`:

  A `TempestConfig` object.

- `session_id`:

  Stable identifier shared by progress events for the session.

- `progress`:

  Optional progress callback.

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

- [`TempestSession$new()`](#method-TempestSession-initialize)

- [`TempestSession$emit_progress()`](#method-TempestSession-emit_progress)

- [`TempestSession$add_turn()`](#method-TempestSession-add_turn)

- [`TempestSession$transcript_markdown()`](#method-TempestSession-transcript_markdown)

- [`TempestSession$get_persona_names()`](#method-TempestSession-get_persona_names)

- [`TempestSession$get_persona_descriptions()`](#method-TempestSession-get_persona_descriptions)

- [`TempestSession$update_mindmap()`](#method-TempestSession-update_mindmap)

- [`TempestSession$mindmap_markdown()`](#method-TempestSession-mindmap_markdown)

- [`TempestSession$extract_facts()`](#method-TempestSession-extract_facts)

- [`TempestSession$harvest_native_sources()`](#method-TempestSession-harvest_native_sources)

- [`TempestSession$suggest_questions()`](#method-TempestSession-suggest_questions)

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

### `TempestSession$new()`

Create a new TempestSession.

#### Usage

    TempestSession$new(
      topic,
      config = tempest_config(),
      n_experts = 3,
      personas = NULL,
      retriever = NULL,
      progress = NULL
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
  [`tempest_generate_personas()`](https://jameshwade.github.io/tempest/reference/tempest_generate_personas.md).

- `retriever`:

  Optional `TempestRetriever` or compatible retriever object with a
  `SourceStore` at `$store`.

- `progress`:

  Optional function called with `tempest_progress_event` objects as the
  session makes progress.

------------------------------------------------------------------------

### `TempestSession$emit_progress()`

Emit a Co-STORM progress event.

#### Usage

    TempestSession$emit_progress(
      event_type,
      status,
      stage = NA_character_,
      step = NA_character_,
      message = NA_character_,
      payload = list(),
      parent_event_id = NA_character_,
      correlation_id = NA_character_
    )

#### Arguments

- `event_type`:

  Progress event type.

- `status`:

  Progress event status.

- `stage`:

  Optional workflow stage.

- `step`:

  Optional workflow step.

- `message`:

  Optional progress message.

- `payload`:

  Optional progress metadata.

- `parent_event_id`:

  Optional parent event id.

- `correlation_id`:

  Optional correlation id.

------------------------------------------------------------------------

### `TempestSession$add_turn()`

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

### `TempestSession$transcript_markdown()`

Get the transcript as markdown.

#### Usage

    TempestSession$transcript_markdown(max_turns = 50)

#### Arguments

- `max_turns`:

  Maximum turns to include.

#### Returns

Markdown string.

------------------------------------------------------------------------

### `TempestSession$get_persona_names()`

Get persona names for agent routing.

#### Usage

    TempestSession$get_persona_names()

#### Returns

Character vector of persona names.

------------------------------------------------------------------------

### `TempestSession$get_persona_descriptions()`

Build persona descriptions for moderator context.

#### Usage

    TempestSession$get_persona_descriptions()

#### Returns

A formatted string describing all personas.

------------------------------------------------------------------------

### `TempestSession$update_mindmap()`

Update the mind map based on new exchange.

#### Usage

    TempestSession$update_mindmap(last_exchange)

#### Arguments

- `last_exchange`:

  The latest exchange text.

------------------------------------------------------------------------

### `TempestSession$mindmap_markdown()`

Get the mind map as markdown.

#### Usage

    TempestSession$mindmap_markdown()

#### Returns

Markdown string.

------------------------------------------------------------------------

### `TempestSession$extract_facts()`

Extract facts from text into the store.

#### Usage

    TempestSession$extract_facts(text)

#### Arguments

- `text`:

  Text containing factual claims.

------------------------------------------------------------------------

### `TempestSession$harvest_native_sources()`

Experimental helper for harvesting source metadata from provider-native
web tool responses.

#### Usage

    TempestSession$harvest_native_sources(chat = NULL, turn = NULL)

#### Arguments

- `chat`:

  Optional chat whose last turn should be inspected.

- `turn`:

  Optional explicit ellmer turn.

#### Returns

Character vector of source ids added or updated.

------------------------------------------------------------------------

### `TempestSession$suggest_questions()`

Suggest follow-up questions for the user based on the conversation so
far.

#### Usage

    TempestSession$suggest_questions(n = 4)

#### Arguments

- `n`:

  Maximum number of questions to return.

#### Returns

A character vector of questions (possibly empty).

------------------------------------------------------------------------

### `TempestSession$find_expert_index()`

Find expert index by persona name.

#### Usage

    TempestSession$find_expert_index(name)

#### Arguments

- `name`:

  The agent name to look up.

#### Returns

Index of the expert, or NULL if not found.

------------------------------------------------------------------------

### `TempestSession$step()`

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

### `TempestSession$warmup()`

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

### `TempestSession$report()`

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

### `TempestSession$add_expert()`

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

### `TempestSession$retire_expert()`

Retire an expert from the panel.

#### Usage

    TempestSession$retire_expert(name)

#### Arguments

- `name`:

  The name of the expert to retire.

#### Returns

Logical indicating success.

------------------------------------------------------------------------

### `TempestSession$get_active_personas()`

Get active (non-retired) personas.

#### Usage

    TempestSession$get_active_personas()

#### Returns

List of active persona objects.

------------------------------------------------------------------------

### `TempestSession$check_and_expand_nodes()`

Check and expand oversized mind map nodes.

#### Usage

    TempestSession$check_and_expand_nodes()

------------------------------------------------------------------------

### `TempestSession$get_discussed_source_ids()`

Get source IDs that have been discussed in the transcript.

#### Usage

    TempestSession$get_discussed_source_ids()

#### Returns

Character vector of discussed source IDs.

------------------------------------------------------------------------

### `TempestSession$find_undiscussed_sources()`

Find sources that haven't been discussed yet.

#### Usage

    TempestSession$find_undiscussed_sources()

#### Returns

Character vector of undiscussed source IDs.

------------------------------------------------------------------------

### `TempestSession$surface_unseen_information()`

Generate questions about undiscussed sources.

#### Usage

    TempestSession$surface_unseen_information(max_questions = 3)

#### Arguments

- `max_questions`:

  Maximum questions to generate.

#### Returns

Character vector of questions, or NULL if none.

------------------------------------------------------------------------

### `TempestSession$reorganize_mindmap()`

Reorganize the mind map for clarity.

#### Usage

    TempestSession$reorganize_mindmap()

------------------------------------------------------------------------

### `TempestSession$execute_turn_decision()`

Execute a discourse manager turn decision.

#### Usage

    TempestSession$execute_turn_decision(decision)

#### Arguments

- `decision`:

  A turn decision from the discourse manager.

#### Returns

A list with speaker, answer, and mindmap_md.

------------------------------------------------------------------------

### `TempestSession$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TempestSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
