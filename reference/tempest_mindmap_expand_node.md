# Expand an oversized mind map node into subtopics

This legacy generic expansion route is unavailable. Co-STORM mind maps
are deterministic projections of committed product evidence and
transcript state.

## Usage

``` r
tempest_mindmap_expand_node(chat, mindmap, node_id)
```

## Arguments

- chat:

  An ellmer chat object (typically the mindmap chat).

- mindmap:

  A mind map list with `nodes` and `edges`.

- node_id:

  The id of the node to expand.

## Value

The updated mind map with the node expanded, or the original mind map if
expansion fails.
