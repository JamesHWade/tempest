# Count notes and source_ids per mind map node

Count notes and source_ids per mind map node

## Usage

``` r
tempest_mindmap_node_sizes(mindmap)
```

## Arguments

- mindmap:

  A mind map list with `nodes` and `edges`.

## Value

A named list where each key is a node id and value is a list with
`n_notes` (word count of notes) and `n_sources` (count of source_ids).
