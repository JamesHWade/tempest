# Fetch URL as markdown using ragnar

Uses ragnar's read_as_markdown() for high-quality content extraction.
Handles HTML, PDFs, and many other document types.

## Usage

``` r
tempest_fetch_url_markdown(url)
```

## Arguments

- url:

  The URL to fetch.

## Value

A list with `kind`, `text`, `title`, and `error`.
