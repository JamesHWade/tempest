# Create a ragnar store with tempest metadata schema

Creates a ragnar store configured with metadata columns useful for
STORM/Co-STORM workflows: source tracking, citation linking, and content
organization.

## Usage

``` r
tempest_create_ragnar_store(
  embed_fn,
  cache_dir = NULL,
  name = "tempest_knowledge",
  title = "STORM Knowledge Base"
)
```

## Arguments

- embed_fn:

  Embedding function (e.g.,
  [`ragnar::embed_openai()`](https://ragnar.tidyverse.org/reference/embed_ollama.html)).

- cache_dir:

  Optional directory for persistent storage. If NULL, creates an
  in-memory store.

- name:

  Store name for tool registration.

- title:

  Human-readable store title.

## Value

A ragnar store object.

## Details

The store includes these metadata columns:

- source_id:

  tempest source ID (Sxxxxxxxxxxxx) for citation linking

- url:

  Original source URL

- title:

  Document title

- fetched_at:

  ISO timestamp when content was fetched

- content_type:

  Content type (html, pdf, etc.)

- perspective:

  STORM perspective this content relates to (optional)
