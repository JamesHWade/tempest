# Retrieve value from cache

Attempts to read a cached value. Returns NULL if not found or on error.
Warns on read errors to help diagnose cache corruption.

## Usage

``` r
tempest_cache_get(cache_dir, key)
```

## Arguments

- cache_dir:

  Cache directory path.

- key:

  Cache key.

## Value

Cached value or NULL.
