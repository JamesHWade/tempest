# TempestSession

Maintains state for a Co-STORM session: multi-agent dialog, mind map,
sources, auxiliary session state, and typed deliverable artifacts.

## Public fields

- `topic`:

  The research topic.

- `title`:

  The report title.

- `config`:

  A `TempestConfig` object.

- `runtime`:

  A `TempestRuntime` containing process-local adapters.

- `connection_permissions`:

  Named per-role or per-expert connection allow-lists.

- `session_id`:

  Stable identifier shared by progress events for the session.

- `progress`:

  Optional progress callback.

- `store`:

  A `SourceStore` object.

- `retriever`:

  A `TempestRetriever` object.

- `experts`:

  List of validated `tempest_expert` profiles.

- `expert_session_manager`:

  Manages expert chat sessions.

- `chats`:

  List of chat objects for each role.

- `transcript`:

  List of dialog turns.

- `mindmap`:

  The mind map data structure.

- `events`:

  Ordered normalized progress-event history.

- `artifacts`:

  Environment of auxiliary and legacy-compatible session state.

- `artifact_catalog`:

  Typed deliverable specifications and artifacts produced by the
  session.

- `workflow_run`:

  Optional generic `TempestRun` that owns the session workflow
  lifecycle.

- `capability_grants`:

  Serializable capability decisions by execution context.

- `discourse_manager`:

  A `DiscourseManager` object (NULL when disabled).

## Methods

### Public methods

- [`TempestSession$new()`](#method-TempestSession-initialize)

- [`TempestSession$record_progress_event()`](#method-TempestSession-record_progress_event)

- [`TempestSession$emit_progress()`](#method-TempestSession-emit_progress)

- [`TempestSession$add_turn()`](#method-TempestSession-add_turn)

- [`TempestSession$transcript_markdown()`](#method-TempestSession-transcript_markdown)

- [`TempestSession$get_expert_names()`](#method-TempestSession-get_expert_names)

- [`TempestSession$get_expert_descriptions()`](#method-TempestSession-get_expert_descriptions)

- [`TempestSession$update_mindmap()`](#method-TempestSession-update_mindmap)

- [`TempestSession$mindmap_markdown()`](#method-TempestSession-mindmap_markdown)

- [`TempestSession$extract_facts()`](#method-TempestSession-extract_facts)

- [`TempestSession$harvest_native_sources()`](#method-TempestSession-harvest_native_sources)

- [`TempestSession$suggest_questions()`](#method-TempestSession-suggest_questions)

- [`TempestSession$find_expert()`](#method-TempestSession-find_expert)

- [`TempestSession$step()`](#method-TempestSession-step)

- [`TempestSession$warmup()`](#method-TempestSession-warmup)

- [`TempestSession$report()`](#method-TempestSession-report)

- [`TempestSession$add_expert()`](#method-TempestSession-add_expert)

- [`TempestSession$retire_expert()`](#method-TempestSession-retire_expert)

- [`TempestSession$get_active_experts()`](#method-TempestSession-get_active_experts)

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
      runtime = tempest_runtime(),
      n_experts = 3,
      experts = NULL,
      connection_permissions = list(),
      retriever = NULL,
      progress = NULL,
      session_id = NULL
    )

#### Arguments

- `topic`:

  The research topic.

- `config`:

  A `TempestConfig` object.

- `runtime`:

  A
  [`tempest_runtime()`](https://jameshwade.github.io/tempest/reference/tempest_runtime.md)
  containing process-local adapters.

- `n_experts`:

  Number of expert agents.

- `experts`:

  Optional list of validated expert profiles. If `NULL`, experts are
  generated automatically using
  [`tempest_generate_experts()`](https://jameshwade.github.io/tempest/reference/tempest_generate_experts.md).

- `connection_permissions`:

  Named list mapping role or expert ids to opaque connection ids allowed
  for this session.

- `retriever`:

  Optional `TempestRetriever` or compatible retriever object with a
  `SourceStore` at `$store`.

- `progress`:

  Optional function called with `tempest_progress_event` objects as the
  session makes progress.

- `session_id`:

  Optional stable session identifier. If `NULL`, a new identifier is
  generated.

------------------------------------------------------------------------

### `TempestSession$record_progress_event()`

Record a progress event emitted by a session-owned collaborator.

#### Usage

    TempestSession$record_progress_event(event)

#### Arguments

- `event`:

  A `tempest_progress_event` object.

#### Returns

The event, invisibly.

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

### `TempestSession$get_expert_names()`

Get expert names for agent routing.

#### Usage

    TempestSession$get_expert_names()

#### Returns

Character vector of expert names.

------------------------------------------------------------------------

### `TempestSession$get_expert_descriptions()`

Build expert descriptions for moderator context.

#### Usage

    TempestSession$get_expert_descriptions()

#### Returns

A formatted string describing all experts.

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

    TempestSession$extract_facts(
      text,
      turn = NULL,
      source_ids = NULL,
      session_id = self$session_id,
      expert_id = NA_character_,
      correlation_id = NA_character_
    )

#### Arguments

- `text`:

  Text containing factual claims.

- `turn`:

  Optional ellmer turn to inspect for provider-native sources.

- `source_ids`:

  Optional source ids already harvested for the turn.

- `session_id`:

  Optional Co-STORM or expert session id.

- `expert_id`:

  Optional expert id.

- `correlation_id`:

  Optional progress correlation id for the turn.

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

### `TempestSession$find_expert()`

Find an expert index by stable id.

#### Usage

    TempestSession$find_expert(expert_id)

#### Arguments

- `expert_id`:

  The stable expert id to look up.

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

The new expert profile (invisibly).

------------------------------------------------------------------------

### `TempestSession$retire_expert()`

Retire an expert from the panel.

#### Usage

    TempestSession$retire_expert(expert_id)

#### Arguments

- `expert_id`:

  The stable id of the expert to retire.

#### Returns

Logical indicating success.

------------------------------------------------------------------------

### `TempestSession$get_active_experts()`

Get active expert profiles.

#### Usage

    TempestSession$get_active_experts()

#### Returns

List of active `tempest_expert` profiles.

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
