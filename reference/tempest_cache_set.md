# Store value in cache

Saves a value to the cache. Creates parent directories if needed. Warns
on write errors but doesn't abort.

## Usage

``` r
tempest_cache_set(cache_dir, key, value)
```

## Arguments

- cache_dir:

  Cache directory path.

- key:

  Cache key.

- value:

  Value to cache.

## Value

Invisibly returns the value.
