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

- skill_ids:

  Skill identifiers assigned to the expert.

- skill_versions:

  Optional named character vector mapping assigned skill identifiers to
  required versions.

- required_capability_ids:

  Capabilities that must be granted before the expert can run.

- optional_capability_ids:

  Capabilities the expert may use when granted.

- model_role:

  Default model role.

- model_policy_ref:

  Optional host model-policy reference.

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

Expert profiles are serializable definitions of identity, procedure, and
permission requirements. Runtime chats, tools, clients, and credentials
are resolved separately for each execution context.

## Examples

``` r
expert <- tempest_expert(
  expert_id = "expert.battery-policy",
  name = "Dr. Rivera",
  title = "Battery policy analyst",
  description = "Policy and market incentives",
  instructions = "Compare policy mechanisms and preserve uncertainty.",
  skill_ids = "evidence-synthesis",
  required_capability_ids = "evidence.search"
)
```
