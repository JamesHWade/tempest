# Extract table of contents from a URL

Fetches a page and extracts h1-h4 headings as a structured ToC.

## Usage

``` r
tempest_extract_toc_from_url(url)
```

## Arguments

- url:

  URL to extract ToC from.

## Value

Character vector of indented headings, or
[`character()`](https://rdrr.io/r/base/character.html) on error.
