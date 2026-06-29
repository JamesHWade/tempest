# Clear the Tempest cache

Removes cached search and fetch results from the specified cache
directory. This is useful when a host app wants to force fresh web
retrieval without changing the configured cache path.

## Usage

``` r
tempest_cache_clear(cache_dir = NULL, max_age = NULL)
```

## Arguments

- cache_dir:

  Cache directory path. If `NULL`, uses `TEMPEST_CACHE_DIR` or the
  default Tempest cache under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- max_age:

  Optional maximum cache age in seconds. When supplied, only files older
  than `max_age` are removed.

## Value

Invisibly returns TRUE.
