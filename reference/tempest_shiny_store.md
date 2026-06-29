# Create a shared Tempest Shiny store

**\[experimental\]**

## Usage

``` r
tempest_shiny_store()
```

## Value

A Tempest Shiny store handle.

## Details

`tempest_shiny_store()` creates the small reactive store used by the
embeddable Tempest Shiny adapter. Host apps can pass the returned store
to
[`tempest_shiny_server()`](https://jameshwade.github.io/tempest/reference/tempest_shiny_server.md)
when they want to share state across adapter instances or inspect the
current `TempestSession`.

The returned object should be treated as an adapter handle; prefer its
public methods over relying on its internal representation.
