# Validate and normalize a URL

Normalizes URLs to HTTPS and validates against security risks including
SSRF attacks (Server-Side Request Forgery).

## Usage

``` r
tempest_normalize_url(url)
```

## Arguments

- url:

  URL string to normalize.

## Value

Normalized URL or `NA_character_` if invalid.
