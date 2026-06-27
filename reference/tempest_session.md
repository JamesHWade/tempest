# Create a Co-STORM session

Create a Co-STORM session

## Usage

``` r
tempest_session(
  topic,
  config = tempest_config(),
  n_experts = 3,
  personas = NULL
)
```

## Arguments

- topic:

  Topic string.

- config:

  A `TempestConfig`.

- n_experts:

  Number of expert agents.

- personas:

  Optional list of pre-generated personas. If NULL, personas are
  generated automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
session <- tempest_session("History of jazz", config = tempest_config())
session$step("What styles emerged in the 1950s?")
} # }
```
