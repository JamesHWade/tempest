# Atomically write lines to a file

Writes to a temporary file in the same directory and then renames it
into place. An interrupted or failed write therefore cannot truncate or
corrupt an existing file: the destination is only replaced once the new
content is fully on disk.

## Usage

``` r
tempest_atomic_write_lines(lines, path)
```

## Arguments

- lines:

  Character vector of lines to write.

- path:

  Destination path.

## Value

Invisibly returns the path.
