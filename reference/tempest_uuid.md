# Generate a unique identifier

Creates a prefixed identifier using xxhash64. Not cryptographically
secure, but sufficient for local session state tracking.

## Usage

``` r
tempest_uuid(prefix = "id")
```

## Arguments

- prefix:

  Prefix string for the identifier.

## Value

Character string identifier.
