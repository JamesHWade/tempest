# Expert Session Manager

Manages expert chat sessions with session IDs for conversation
continuity. Each expert persona gets their own chat session that can be
resumed using the session ID returned from previous interactions.

## Public fields

- `sessions`:

  Environment storing active chat sessions keyed by session ID.

- `config`:

  A `TempestConfig` object for creating chats.

- `retriever`:

  A `TempestRetriever` for registering tools.

- `extractor`:

  Chat object for fact extraction (optional).

- `store`:

  A `SourceStore` for storing extracted facts (optional).

- `progress`:

  Optional progress callback.

- `run_id`:

  Shared Co-STORM session id for progress events.

- `session_provenance`:

  Environments keyed by expert session id for claim-write provenance.

## Methods

### Public methods

- [`ExpertSessionManager$new()`](#method-ExpertSessionManager-initialize)

- [`ExpertSessionManager$emit_progress()`](#method-ExpertSessionManager-emit_progress)

- [`ExpertSessionManager$extract_facts()`](#method-ExpertSessionManager-extract_facts)

- [`ExpertSessionManager$generate_session_id()`](#method-ExpertSessionManager-generate_session_id)

- [`ExpertSessionManager$get_or_create()`](#method-ExpertSessionManager-get_or_create)

- [`ExpertSessionManager$list_sessions()`](#method-ExpertSessionManager-list_sessions)

- [`ExpertSessionManager$clone()`](#method-ExpertSessionManager-clone)

------------------------------------------------------------------------

### `ExpertSessionManager$new()`

Create a new ExpertSessionManager.

#### Usage

    ExpertSessionManager$new(
      config,
      retriever,
      extractor = NULL,
      store = NULL,
      progress = NULL,
      run_id = NULL
    )

#### Arguments

- `config`:

  A `TempestConfig` object.

- `retriever`:

  A `TempestRetriever` object.

- `extractor`:

  Optional chat object for fact extraction.

- `store`:

  Optional `SourceStore` for storing extracted facts.

- `progress`:

  Optional progress callback.

- `run_id`:

  Shared Co-STORM session id for progress events.

------------------------------------------------------------------------

### `ExpertSessionManager$emit_progress()`

Emit a Co-STORM expert progress event.

#### Usage

    ExpertSessionManager$emit_progress(
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

### `ExpertSessionManager$extract_facts()`

Extract facts from an expert response.

#### Usage

    ExpertSessionManager$extract_facts(
      response,
      turn = NULL,
      source_ids = NULL,
      session_id = NA_character_,
      persona_id = NA_character_,
      correlation_id = NA_character_
    )

#### Arguments

- `response`:

  Character string response from expert.

- `turn`:

  Optional ellmer turn to inspect for provider-native sources.

- `source_ids`:

  Optional source ids already harvested for the turn.

- `session_id`:

  Optional expert session id.

- `persona_id`:

  Optional persona id.

- `correlation_id`:

  Optional progress correlation id for the turn.

#### Returns

Invisibly returns NULL.

------------------------------------------------------------------------

### `ExpertSessionManager$generate_session_id()`

Generate a human-readable session ID from a persona name.

#### Usage

    ExpertSessionManager$generate_session_id(persona_name)

#### Arguments

- `persona_name`:

  Name of the persona.

#### Returns

Character string session ID like "dr-sarah-chen-abc123".

------------------------------------------------------------------------

### `ExpertSessionManager$get_or_create()`

Get an existing session or create a new one.

#### Usage

    ExpertSessionManager$get_or_create(persona, session_id = NULL)

#### Arguments

- `persona`:

  A persona object from
  [`tempest_generate_personas()`](https://jameshwade.github.io/tempest/reference/tempest_generate_personas.md).

- `session_id`:

  Optional session ID to resume.

#### Returns

List with `chat`, `session_id`, and `is_new` fields.

------------------------------------------------------------------------

### `ExpertSessionManager$list_sessions()`

List all active session IDs.

#### Usage

    ExpertSessionManager$list_sessions()

#### Returns

Character vector of session IDs.

------------------------------------------------------------------------

### `ExpertSessionManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExpertSessionManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
