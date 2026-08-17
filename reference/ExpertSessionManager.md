# Expert Session Manager

Manages capability-scoped chats for validated expert profiles.

## Public fields

- `sessions`:

  Environment storing active chat sessions keyed by session ID.

- `session_profiles`:

  Environment storing serializable session bindings.

- `config`:

  A `TempestConfig` object for creating chats.

- `retriever`:

  A `TempestRetriever` for registering tools.

- `runtime`:

  A `TempestRuntime` used to resolve skills and capabilities.

- `experts`:

  Environment of expert profiles keyed by stable expert id.

- `expert_connection_ref_ids`:

  Environment of allowed connection ids by expert.

- `extractor`:

  Chat object for fact extraction (optional).

- `extract_claims_program`:

  ProgramSet-bound claim-extraction execution.

- `workspace`:

  A
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  for extracted facts (optional).

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

- [`ExpertSessionManager$add_expert()`](#method-ExpertSessionManager-add_expert)

- [`ExpertSessionManager$retire_expert()`](#method-ExpertSessionManager-retire_expert)

- [`ExpertSessionManager$profile()`](#method-ExpertSessionManager-profile)

- [`ExpertSessionManager$list_experts()`](#method-ExpertSessionManager-list_experts)

- [`ExpertSessionManager$get_or_create()`](#method-ExpertSessionManager-get_or_create)

- [`ExpertSessionManager$restore_session()`](#method-ExpertSessionManager-restore_session)

- [`ExpertSessionManager$session_profile()`](#method-ExpertSessionManager-session_profile)

- [`ExpertSessionManager$list_sessions()`](#method-ExpertSessionManager-list_sessions)

- [`ExpertSessionManager$retire_session()`](#method-ExpertSessionManager-retire_session)

- [`ExpertSessionManager$clone()`](#method-ExpertSessionManager-clone)

------------------------------------------------------------------------

### `ExpertSessionManager$new()`

Create a new ExpertSessionManager.

#### Usage

    ExpertSessionManager$new(
      experts,
      runtime,
      config,
      retriever,
      allowed_connection_ref_ids = list(),
      extractor = NULL,
      extract_claims_program = NULL,
      workspace = NULL,
      progress = NULL,
      run_id = NULL,
      stage_recorder = NULL,
      manifest = NULL,
      on_start = function(pending_run) invisible(pending_run),
      on_run = function(trace) invisible(trace)
    )

#### Arguments

- `experts`:

  Validated `tempest_expert` profiles.

- `runtime`:

  A `TempestRuntime`.

- `config`:

  A `TempestConfig` object.

- `retriever`:

  A `TempestRetriever` object.

- `allowed_connection_ref_ids`:

  Named list of allowed connection ids by expert id.

- `extractor`:

  Optional chat object for fact extraction.

- `extract_claims_program`:

  ProgramSet-bound claim-extraction execution. Required when `extractor`
  is supplied.

- `workspace`:

  Optional
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  for extracted facts.

- `progress`:

  Optional progress callback.

- `run_id`:

  Shared Co-STORM session id for progress events.

- `stage_recorder`:

  Optional callback accepting a stage record and its evaluated output.

- `manifest`:

  Research manifest that owns Deputy execution identity.

- `on_start`:

  Callback accepting one pending Deputy run record.

- `on_run`:

  Callback accepting one terminal Deputy run trace.

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
      expert_id = NA_character_,
      correlation_id = NA_character_,
      deputy_execution = NULL
    )

#### Arguments

- `response`:

  Character string response from expert.

- `turn`:

  Optional ellmer turn to inspect for provider-native sources.

- `source_ids`:

  Optional source ids already harvested for the turn.

- `session_id`:

  Optional manager-owned expert session id. This is delegation metadata
  only; extracted claims use the manager's exact research run id.

- `expert_id`:

  Optional stable expert id.

- `correlation_id`:

  Optional progress correlation id for the turn.

- `deputy_execution`:

  Optional terminal Deputy trace for the answer.

#### Returns

Invisibly returns NULL.

------------------------------------------------------------------------

### `ExpertSessionManager$add_expert()`

Add an active expert profile to the live roster.

#### Usage

    ExpertSessionManager$add_expert(
      expert,
      allowed_connection_ref_ids = character(),
      replace = FALSE
    )

#### Arguments

- `expert`:

  A validated `tempest_expert`.

- `allowed_connection_ref_ids`:

  Connection ids granted to this expert.

- `replace`:

  Whether to replace an existing profile with the same id.

#### Returns

The stable expert id, invisibly.

------------------------------------------------------------------------

### `ExpertSessionManager$retire_expert()`

Retire an expert and all chats bound to that profile.

#### Usage

    ExpertSessionManager$retire_expert(expert_id)

#### Arguments

- `expert_id`:

  Stable expert id.

#### Returns

Whether the expert was present.

------------------------------------------------------------------------

### `ExpertSessionManager$profile()`

Look up an expert by exact stable id.

#### Usage

    ExpertSessionManager$profile(expert_id, active_only = TRUE)

#### Arguments

- `expert_id`:

  Stable expert id.

- `active_only`:

  Whether retired profiles should be rejected.

#### Returns

A validated expert profile.

------------------------------------------------------------------------

### `ExpertSessionManager$list_experts()`

List expert profiles in stable-id order.

#### Usage

    ExpertSessionManager$list_experts(active_only = TRUE)

#### Arguments

- `active_only`:

  Whether to omit retired profiles.

#### Returns

A list of validated expert profiles.

------------------------------------------------------------------------

### `ExpertSessionManager$get_or_create()`

Get an expert's existing session or create a scoped chat.

#### Usage

    ExpertSessionManager$get_or_create(expert_id, session_id = NULL)

#### Arguments

- `expert_id`:

  Stable expert id or matching expert profile.

- `session_id`:

  Optional existing, manager-owned session id to resume.

#### Returns

Chat, session binding, grants, provenance, and creation status.

------------------------------------------------------------------------

### `ExpertSessionManager$restore_session()`

Restore a saved session binding through fresh runtime authorization.

#### Usage

    ExpertSessionManager$restore_session(binding)

#### Arguments

- `binding`:

  Serializable session profile containing the opaque session id and
  exact expert identity fields.

#### Returns

The same result shape as `get_or_create()`.

------------------------------------------------------------------------

### `ExpertSessionManager$session_profile()`

Return the serializable binding for an active session.

#### Usage

    ExpertSessionManager$session_profile(session_id)

#### Arguments

- `session_id`:

  Manager-owned expert session id.

#### Returns

Session binding including expert fingerprint and grants.

------------------------------------------------------------------------

### `ExpertSessionManager$list_sessions()`

List all active session IDs.

#### Usage

    ExpertSessionManager$list_sessions()

#### Returns

Character vector of session IDs.

------------------------------------------------------------------------

### `ExpertSessionManager$retire_session()`

Retire a stateful expert chat so it cannot be reused after timeout or
cancellation.

#### Usage

    ExpertSessionManager$retire_session(session_id)

#### Arguments

- `session_id`:

  Session id returned by `get_or_create()`.

#### Returns

A list describing whether the session existed and whether a provider
cancellation method was available.

------------------------------------------------------------------------

### `ExpertSessionManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExpertSessionManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
