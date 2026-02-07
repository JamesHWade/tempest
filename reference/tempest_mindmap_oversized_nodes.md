# Find oversized mind map nodes

Identifies nodes whose total content (notes word count + source count)
exceeds the trigger threshold.

## Usage

``` r
tempest_mindmap_oversized_nodes(mindmap, trigger_count)
```

## Arguments

- mindmap:

  A mind map list with `nodes` and `edges`.

- trigger_count:

  Integer threshold. Nodes with total \> trigger_count are oversized.

## Value

Character vector of oversized node ids.
