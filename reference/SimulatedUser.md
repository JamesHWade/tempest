# SimulatedUser

A simulated curious researcher for automated Co-STORM evaluation.
Generates natural research questions and tracks turn counts.

## Public fields

- `topic`:

  The research topic.

- `chat`:

  An ellmer chat object for generating questions.

- `turn_count`:

  Number of turns completed.

- `max_turns`:

  Maximum turns before stopping.

## Methods

### Public methods

- [`SimulatedUser$new()`](#method-SimulatedUser-initialize)

- [`SimulatedUser$generate_question()`](#method-SimulatedUser-generate_question)

- [`SimulatedUser$run_session()`](#method-SimulatedUser-run_session)

- [`SimulatedUser$clone()`](#method-SimulatedUser-clone)

------------------------------------------------------------------------

### `SimulatedUser$new()`

Create a new SimulatedUser.

#### Usage

    SimulatedUser$new(topic, config = tempest_config(), max_turns = 10L)

#### Arguments

- `topic`:

  The research topic.

- `config`:

  A `TempestConfig` object.

- `max_turns`:

  Maximum number of turns.

------------------------------------------------------------------------

### `SimulatedUser$generate_question()`

Generate the next question based on session state.

#### Usage

    SimulatedUser$generate_question(
      transcript_md = "(no dialog yet)",
      mindmap_md = "(empty)"
    )

#### Arguments

- `transcript_md`:

  Recent transcript as markdown.

- `mindmap_md`:

  Mind map as markdown.

#### Returns

A question string, or NULL if done.

------------------------------------------------------------------------

### `SimulatedUser$run_session()`

Run a full automated session.

#### Usage

    SimulatedUser$run_session(session, warmup = TRUE, verbose = TRUE)

#### Arguments

- `session`:

  A `TempestSession` object.

- `warmup`:

  Whether to run warmup first.

- `verbose`:

  Print progress.

#### Returns

The session object (invisibly).

------------------------------------------------------------------------

### `SimulatedUser$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulatedUser$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
