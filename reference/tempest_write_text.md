# Write text to file

Creates parent directories if needed. The write is atomic: content is
written to a temporary file and renamed into place.

## Usage

``` r
tempest_write_text(path, text)
```

## Arguments

- path:

  Path to file.

- text:

  Text content to write.

## Value

Invisibly returns the path.
