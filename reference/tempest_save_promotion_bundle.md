# Save a Tempest promotion bundle atomically

The destination must not exist. The current format is a closed two-file
directory with an exact inventory, SHA-256 checksums, and a self-bound
manifest. No prior promotion format is read or written.

## Usage

``` r
tempest_save_promotion_bundle(bundle, path)
```

## Arguments

- bundle:

  A
  [`tempest_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_promotion_bundle.md)
  value.

- path:

  New destination directory.

## Value

The normalized bundle directory, invisibly.
