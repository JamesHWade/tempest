# Expand an oversized mind map node into subtopics

Uses an LLM to split a single oversized node into 2-4 child subtopics,
distributing notes and source_ids appropriately.

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
