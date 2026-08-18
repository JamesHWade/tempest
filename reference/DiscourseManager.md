# DiscourseManager

Retired generic discourse manager for Co-STORM sessions.

## Public fields

- `config`:

  A `TempestConfig` object.

## Methods

### Public methods

- [`DiscourseManager$new()`](#method-DiscourseManager-initialize)

- [`DiscourseManager$decide_next_turn()`](#method-DiscourseManager-decide_next_turn)

- [`DiscourseManager$clone()`](#method-DiscourseManager-clone)

------------------------------------------------------------------------

### `DiscourseManager$new()`

Create a new DiscourseManager.

#### Usage

    DiscourseManager$new(config)

#### Arguments

- `config`:

  A `TempestConfig` object.

------------------------------------------------------------------------

### `DiscourseManager$decide_next_turn()`

Decide the next turn action.

#### Usage

    DiscourseManager$decide_next_turn(
      topic,
      transcript_md,
      mindmap_md,
      expert_descriptions,
      unseen_sources = character()
    )

#### Arguments

- `topic`:

  The research topic.

- `transcript_md`:

  Recent transcript as markdown.

- `mindmap_md`:

  Mind map as markdown.

- `expert_descriptions`:

  Formatted expert descriptions with stable ids.

- `unseen_sources`:

  Character vector of undiscussed source IDs.

#### Returns

A turn decision list with action, expert_id, instruction, rationale.

------------------------------------------------------------------------

### `DiscourseManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiscourseManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
