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

  Optional
  [ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
  used to validate resource, claim, and evidence-span identifiers.

## Value

A restored `TempestArtifactCatalog`.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

Restoration reconstructs every specification and artifact through its
validated constructor, verifies fingerprints and content checksums, and
optionally verifies evidence lineage against a
[ResearchWorkspace](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md).
Runtime store adapters are reattached but are not written during
restoration.
