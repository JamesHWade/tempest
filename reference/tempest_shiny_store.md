# Create a shared Tempest Shiny store

**\[experimental\]**

## Usage

``` r
tempest_shiny_store()
```

## Value

A 13-member product handle containing `peek_costorm_session`,
`costorm_session`, `costorm_workspace`, `set_costorm_session`,
`touch_costorm_session`, `save_costorm_session`,
`resume_costorm_session`, `costorm_persistence_status`, `report_md`,
`report_workspace`, `report_topic`, `publish_costorm_report`, and
`publish_storm_report`.

## Details

`tempest_shiny_store()` creates the small reactive store used by the
embeddable Tempest Shiny adapter. Host apps can pass the returned store
to
[`tempest_shiny_server()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_server.md)
when they want to share state across adapter instances or inspect the
current Co-STORM session and report product.

The returned object should be treated as an adapter handle; prefer its
public methods over relying on its internal representation.
