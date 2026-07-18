# Create a Tempest skill specification

**\[experimental\]**

## Usage

``` r
tempest_skill(
  skill_id,
  purpose,
  instructions,
  version = "1",
  title = skill_id,
  input_schema = list(),
  output_schema = list(),
  required_capability_ids = character(),
  operation_ids = character(),
  operation_versions = character(),
  state = "active",
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- skill_id:

  Stable skill identifier.

- purpose:

  Outcome the skill is intended to accomplish.

- instructions:

  Procedure an expert should follow.

- version:

  Stable skill version.

- title:

  Display title. Defaults to `skill_id`.

- input_schema, output_schema:

  Canonical JSON-compatible contracts.

- required_capability_ids:

  Capability identifiers needed by the skill.

- operation_ids:

  Runtime skill operation identifiers.

- operation_versions:

  Optional named character vector mapping operation identifiers to
  required versions.

- state:

  Definition state, either `"active"` or `"retired"`.

- metadata:

  Canonical JSON-compatible host metadata. Metadata cannot contain
  credentials or executable values.

- schema_version:

  Serializable record schema version.

## Value

A `tempest_skill` S7 object.

## Details

Skills are serializable procedures. They identify required capabilities
and runtime skill operations without storing executable functions.

## Examples

``` r
skill <- tempest_skill(
  "evidence-synthesis",
  purpose = "Synthesize verified evidence",
  instructions = "Compare sources and preserve disagreements.",
  required_capability_ids = "evidence.search"
)
```
