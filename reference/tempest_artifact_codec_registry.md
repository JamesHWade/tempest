# Create a typed artifact codec registry

**\[experimental\]**

## Usage

``` r
tempest_artifact_codec_registry(codecs = list(), include_builtins = TRUE)
```

## Arguments

- codecs:

  Artifact codec definitions to register.

- include_builtins:

  Include Tempest UTF-8 text, canonical JSON, and external-reference
  codecs.

## Value

A `TempestArtifactCodecRegistry`.

## Details

This experimental API is frozen and scheduled for removal in Tempest
0.2.0. No compatibility shim is planned; see
[tempest-generic-kernel-retirement](https://jameshwade.github.io/tempest/reference/tempest-generic-kernel-retirement.md).

Registries resolve codecs by stable id, version, and media type. Their
[`list()`](https://rdrr.io/r/base/list.html) method deliberately
excludes executable functions.
