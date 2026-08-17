# Read and validate a current Tempest promotion bundle

Bundle-local digests establish internal consistency, not authenticity.
The caller must retain the original bundle id through a trusted channel
and supply it when reading the persisted directory.

## Usage

``` r
tempest_read_promotion_bundle(path, expected_bundle_id)
```

## Arguments

- path:

  Promotion-bundle directory created by
  [`tempest_save_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_save_promotion_bundle.md).

- expected_bundle_id:

  Exact SHA-256 bundle id retained independently from the directory, for
  example `bundle@bundle_id` from the value passed to
  [`tempest_save_promotion_bundle()`](https://jameshwade.github.io/tempest/reference/tempest_save_promotion_bundle.md).

## Value

A validated `TempestPromotionBundle`.
