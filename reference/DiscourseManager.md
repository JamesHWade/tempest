# DiscourseManager

DiscourseManager

DiscourseManager

## Details

LLM-driven discourse management for Co-STORM sessions. Decides which
agent should speak next and what action to take.

## Public fields

- `chat`:

  An ellmer chat object for making decisions.

- `config`:

  A `TempestConfig` object.

## Methods

### Public methods

- [`DiscourseManager$new()`](#method-DiscourseManager-new)

- [`DiscourseManager$decide_next_turn()`](#method-DiscourseManager-decide_next_turn)

- [`DiscourseManager$clone()`](#method-DiscourseManager-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DiscourseManager.

#### Usage

    DiscourseManager$new(config)

#### Arguments

- `config`:

  A `TempestConfig` object.

------------------------------------------------------------------------

### Method `decide_next_turn()`

Decide the next turn action.

#### Usage

    DiscourseManager$decide_next_turn(
      topic,
      transcript_md,
      mindmap_md,
      persona_descriptions,
      unseen_sources = character()
    )

#### Arguments

- `topic`:

  The research topic.

- `transcript_md`:

  Recent transcript as markdown.

- `mindmap_md`:

  Mind map as markdown.

- `persona_descriptions`:

  Formatted persona descriptions.

- `unseen_sources`:

  Character vector of undiscussed source IDs.

#### Returns

A turn decision list with action, agent_name, instruction, rationale.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiscourseManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
