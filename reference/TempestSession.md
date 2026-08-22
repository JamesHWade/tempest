# TempestSession

Maintains state for a Co-STORM session: multi-agent dialog, mind map,
provisional scientific evidence, and report state.

## Internal implementation

`TempestSession` is an unexported mutable implementation returned by
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).
Its supported product state is the correlated research manifest,
workspace, transcript, mind map, experts, progress events, and canonical
report. Process-local execution members are internal and are not part of
the persistence or public API contract.

## Active bindings

- `session_id`:

  Stable session identifier.

- `topic`:

  Research topic.

- `status`:

  Current product status.

- `experts`:

  Read-only expert roster.

- `transcript`:

  Read-only conversation transcript.

- `mindmap`:

  Read-only mind-map projection.

## Methods

### Public methods

- [`TempestSession$new()`](#method-TempestSession-initialize)

- [`TempestSession$suggest_questions()`](#method-TempestSession-suggest_questions)

- [`TempestSession$step()`](#method-TempestSession-step)

- [`TempestSession$warmup()`](#method-TempestSession-warmup)

- [`TempestSession$publish()`](#method-TempestSession-publish)

- [`TempestSession$add_expert()`](#method-TempestSession-add_expert)

- [`TempestSession$retire_expert()`](#method-TempestSession-retire_expert)

- [`TempestSession$clone()`](#method-TempestSession-clone)

------------------------------------------------------------------------

### `TempestSession$new()`

Internal constructor. Use
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
for the supported API.

#### Usage

    TempestSession$new(
      topic,
      config = tempest_config(),
      n_experts = 3,
      experts = NULL,
      retriever = NULL,
      progress = NULL,
      session_id = NULL,
      program_set = NULL,
      knowledge_view = NULL,
      .restore_manifest = NULL,
      .restore_token = NULL
    )

#### Arguments

- `topic`:

  The research topic.

- `config`:

  A `TempestConfig` object.

- `n_experts`:

  Number of expert agents.

- `experts`:

  Optional list of validated expert profiles. If `NULL`, experts are
  generated automatically using
  [`tempest_generate_experts()`](https://jameshwade.github.io/tempest/reference/tempest_generate_experts.md).

- `retriever`:

  Optional `TempestRetriever` or compatible retriever object with a
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  at `$workspace`.

- `progress`:

  Optional function called with `tempest_progress_event` objects as the
  session makes progress.

- `session_id`:

  Optional stable session identifier. If `NULL`, a new identifier is
  generated.

- `program_set`:

  A
  [TempestProgramSet](https://jameshwade.github.io/tempest/reference/TempestProgramSet.md)
  used for every structured Co-STORM stage.

- `knowledge_view`:

  Optional immutable Graft view. A fresh session requires it whenever
  `program_set` contains governed procedures.

- `.restore_manifest`:

  Internal research manifest supplied only by Tempest's
  bundle-restoration seam.

- `.restore_token`:

  Internal authorization token for bundle restoration.

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

### `TempestSession$step()`

Process one explicit user turn through the Deputy moderator.

#### Usage

    TempestSession$step(user_input = NULL)

#### Arguments

- `user_input`:

  User input.

#### Returns

A list with the moderator answer and exact Deputy identity.

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

### `TempestSession$publish()`

Verify claims, create and commit the canonical report, move the manifest
to `succeeded`, and seal the workspace.

#### Usage

    TempestSession$publish(
      style = c("technical", "executive"),
      include_references = TRUE
    )

#### Arguments

- `style`:

  Report style: "technical" or "executive".

- `include_references`:

  Include references section.

#### Returns

The committed Markdown report. Use
[`tempest_report()`](https://jameshwade.github.io/tempest/reference/tempest_report.md)
to read the exact committed bytes later.

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

### `TempestSession$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TempestSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
