# Create a Tempest validation result

**\[experimental\]**

## Usage

``` r
tempest_validation_result(
  validator_id,
  status = c("passed", "failed", "warning"),
  message = NA_character_,
  details = list(),
  created_at = NULL
)
```

## Arguments

- validator_id:

  Stable validator operation identifier.

- status:

  One of `"passed"`, `"failed"`, or `"warning"`.

- message:

  Optional human-readable result.

- details:

  Serializable diagnostic details.

- created_at:

  Optional creation timestamp.

## Value

A `tempest_validation_result` S7 object.
