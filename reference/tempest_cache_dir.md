# Get or create cache directory

Returns the cache directory path, creating it if necessary. Uses
TEMPEST_CACHE_DIR environment variable, or a temp directory.

## Usage

``` r
tempest_cache_dir(cache_dir = NULL)
```

## Arguments

- cache_dir:

  Optional explicit cache directory path.

## Value

Path to cache directory.
