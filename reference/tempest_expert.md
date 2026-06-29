# Create a Co-STORM expert definition

**\[experimental\]**

## Usage

``` r
tempest_expert(
  name,
  title = "Research Specialist",
  perspective = "General research perspective",
  initial_questions = character(),
  id = NULL,
  affiliation = NA_character_,
  background = NA_character_,
  focus_areas = character()
)
```

## Arguments

- name:

  Expert display name.

- title:

  Short title or area of expertise.

- perspective:

  Description of the expert's research perspective.

- initial_questions:

  Character vector of warmup questions for this expert.

- id:

  Optional numeric expert id. Missing ids are assigned by
  [`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

- affiliation:

  Optional affiliation.

- background:

  Optional background text.

- focus_areas:

  Optional character vector of focus areas.

## Value

A list suitable for the `personas` argument of
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md).

## Details

Host applications can pass these definitions to
[`tempest_session()`](https://jameshwade.github.io/tempest/reference/tempest_session.md)
via the `personas` argument to control the expert panel without patching
`TempestSession` internals.

## Examples

``` r
expert <- tempest_expert(
  name = "Dr. Rivera",
  title = "Battery policy analyst",
  perspective = "Policy and market incentives",
  initial_questions = "Which policies affect battery recycling?"
)
```
