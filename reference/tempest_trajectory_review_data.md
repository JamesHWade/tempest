# Extract a validated trajectory review projection

`tempest_trajectory_review_data()` exposes the closed, plain-data
projection from a value returned by
[`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md).
It validates the exact Tempest review class and its content-bound review
identity before returning data. It never reaches back into a product,
session, workspace, provider, or other live object.

## Usage

``` r
tempest_trajectory_review_data(x)
```

## Arguments

- x:

  A value returned by
  [`tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.md).

## Value

A named list containing the validated, schema-versioned trajectory
review projection.
