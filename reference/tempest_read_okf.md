# Read an Open Knowledge Format bundle

`tempest_read_okf()` reads a conformant [Open Knowledge
Format](https://github.com/GoogleCloudPlatform/knowledge-catalog/tree/main/okf)
(OKF) directory without executing referenced code or resolving external
resources. Concept files remain evidence inputs: reading a bundle does
not approve its contents, grant capabilities, or publish artifacts.

## Usage

``` r
tempest_read_okf(path, max_concepts = 5000, max_bytes = 20 * 1024^2)
```

## Arguments

- path:

  Directory containing an OKF bundle.

- max_concepts:

  Maximum concept files to read.

- max_bytes:

  Maximum aggregate Markdown bytes to read, including index and log
  files.

## Value

A `tempest_okf_bundle`.

## Details

Tempest enforces the OKF v0.2 conformance boundary: parseable YAML
frontmatter and a non-empty `type` for every concept, while tolerating
unknown types, extension keys, missing indexes, and broken links as the
specification requires. Optional trust, lifecycle, provenance, and
attestation problems are retained as diagnostics in `bundle$issues`.

## Examples

``` r
if (FALSE) { # \dontrun{
bundle <- tempest_read_okf("knowledge/okf")
tempest_okf_concepts(bundle)
} # }
```
