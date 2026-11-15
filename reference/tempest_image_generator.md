# Create an OpenAI image generation callback

The callback reads `OPENAI_API_KEY` when invoked, requests one PNG image
from the OpenAI Images API, and saves it in the provided task directory.
The returned callback and metadata never retain the API key.

## Usage

``` r
tempest_image_generator(
  model = "gpt-image-2.6-flare",
  size = "1536x1024",
  quality = "medium"
)
```

## Arguments

- model:

  OpenAI image model. The default is GPT-Image-2.6 Flare.

- size:

  Output dimensions: `"1536x1024"`, `"1024x1024"`, `"1024x1536"`, or
  `"auto"`.

- quality:

  Image quality: `"medium"`, `"low"`, `"high"`, `"xhigh"`, `"max"`, or
  `"auto"`.

## Value

A callback with the `function(task, directory)` interface used by
[`tempest_enrich_report()`](https://jameshwade.github.io/tempest/reference/tempest_enrich_report.md).
It returns `file` and `metadata`.
