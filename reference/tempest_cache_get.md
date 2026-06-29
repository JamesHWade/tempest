# Retrieve value from cache

Attempts to read a cached value. Returns NULL if the entry is missing,
has expired (older than `max_age`), or could not be read. Warns on read
errors to help diagnose cache corruption.

## Usage

``` r
tempest_cache_get(cache_dir, key, max_age = Inf)
```

## Arguments

- cache_dir:

  Cache directory path.

- key:

  Cache key.

- max_age:

  Maximum cache age in seconds. Entries older than this are treated as
  missing. Defaults to `Inf` (no expiry).

## Value

Cached value or NULL.
