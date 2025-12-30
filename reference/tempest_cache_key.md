# Generate cache key from arguments

Creates a deterministic hash key from the provided arguments.

## Usage

``` r
tempest_cache_key(..., algo = "xxhash64")
```

## Arguments

- ...:

  Arguments to hash.

- algo:

  Hash algorithm (default: xxhash64).

## Value

Character string cache key.
