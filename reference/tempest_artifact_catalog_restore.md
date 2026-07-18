# Restore a typed Tempest artifact catalog

**\[experimental\]**

## Usage

``` r
tempest_artifact_catalog_restore(snapshot, store = NULL, evidence_store = NULL)
```

## Arguments

- snapshot:

  A catalog snapshot from `TempestArtifactCatalog$snapshot()`.

- store:

  Optional runtime artifact-store adapter.

- evidence_store:

  Optional `SourceStore` used to validate resource, claim, and
  evidence-span identifiers.

## Value

A restored `TempestArtifactCatalog`.

## Details

Restoration reconstructs every specification and artifact through its
validated constructor, verifies fingerprints and content checksums, and
optionally verifies evidence lineage against a `SourceStore`. Runtime
store adapters are reattached but are not written during restoration.
