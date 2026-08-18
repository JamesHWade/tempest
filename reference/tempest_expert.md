# Create a Tempest expert profile

**\[experimental\]**

## Usage

``` r
tempest_expert(
  expert_id,
  name,
  title,
  description,
  instructions,
  version = "1",
  focus_areas = character(),
  skill_ids = character(),
  skill_versions = character(),
  required_capability_ids = character(),
  optional_capability_ids = character(),
  model_role = "expert",
  model_policy_ref = NA_character_,
  selection_metadata = list(),
  initial_work_items = character(),
  initial_questions = character(),
  state = "active",
  metadata = list(),
  schema_version = 1L
)
```

## Arguments

- expert_id:

  Stable expert identifier.

- name:

  Expert display name.

- title:

  Short title or area of expertise.

- description:

  Description of the expert's perspective and scope.

- instructions:

  Instructions the expert should follow.

- version:

  Stable expert-profile version.

- focus_areas:

  Character vector of focus areas.

- skill_ids, skill_versions:

  Reserved current-schema fields. They must be empty because scientific
  experts use fixed product roles and tools.

- required_capability_ids, optional_capability_ids:

  Reserved current-schema fields that must be empty.

- model_role:

  One fixed scientific model role: `"coordinator"`, `"expert"`,
  `"writer"`, `"mindmap"`, or `"judge"`.

- model_policy_ref:

  Reserved current-schema field that must be `NA`.

- selection_metadata:

  Serializable metadata for host-side expert selection.

- initial_work_items, initial_questions:

  Optional startup work.

- state:

  Definition state, either `"active"` or `"retired"`.

- metadata:

  Canonical JSON-compatible host metadata. Metadata cannot contain
  credentials or executable values.

- schema_version:

  Serializable record schema version.

## Value

A `tempest_expert` S7 object.

## Details

Expert profiles are serializable definitions of scientific identity and
procedure. Runtime chats, fixed role tools, clients, and credentials are
resolved separately for each execution context.

## Examples

``` r
expert <- tempest_expert(
  expert_id = "expert.battery-policy",
  name = "Dr. Rivera",
  title = "Battery policy analyst",
  description = "Policy and market incentives",
  instructions = "Compare policy mechanisms and preserve uncertainty."
)
```
