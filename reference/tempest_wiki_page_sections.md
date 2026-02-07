# Get Wikipedia page sections via Parse API

Uses the Wikipedia Parse API to get structured section headings.

## Usage

``` r
tempest_wiki_page_sections(title)
```

## Arguments

- title:

  Wikipedia page title.

## Value

Character vector of indented section headings, or
[`character()`](https://rdrr.io/r/base/character.html) on error.
