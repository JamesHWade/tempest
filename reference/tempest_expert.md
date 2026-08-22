# Create a Tempest expert profile

**\[experimental\]**

## Usage

``` r
tempest_expert(
  name,
  title,
  description,
  instructions,
  focus_areas = character(),
  initial_questions = character()
)
```

## Arguments

- name:

  Expert display name.

- title:

  Short title or area of expertise.

- description:

  Description of the expert's perspective and scope.

- instructions:

  Instructions the expert should follow.

- focus_areas:

  Character vector of focus areas.

- initial_questions:

  Optional startup research questions.

## Value

A `tempest_expert` S7 object.

## Details

Expert profiles contain only human-authored scientific identity and
perspective. Tempest derives the exact profile identity and version from
those canonical fields. Runtime chats, the fixed expert model role,
tools, clients, roster state, and credentials are resolved separately.

## Examples

``` r
expert <- tempest_expert(
  name = "Dr. Rivera",
  title = "Battery policy analyst",
  description = "Policy and market incentives",
  instructions = "Compare policy mechanisms and preserve uncertainty."
)
```
