# TempestRetriever

Provides web and Wikipedia retrieval with caching, plus helper methods
to register retrieval tools with ellmer chats. Optionally integrates
with ragnar for semantic search capabilities.

## Public fields

- `config`:

  A `TempestConfig` object.

- `store`:

  A `SourceStore` object.

- `ragnar_store`:

  A ragnar store for semantic retrieval (optional).

- `cache_dir`:

  Path to cache directory.

- `cache_enabled`:

  Whether retriever calls use the on-disk cache.

- `cache_ttl`:

  Maximum cache age in seconds.

## Methods

### Public methods

- [`TempestRetriever$new()`](#method-TempestRetriever-initialize)

- [`TempestRetriever$search()`](#method-TempestRetriever-search)

- [`TempestRetriever$fetch()`](#method-TempestRetriever-fetch)

- [`TempestRetriever$ingest_to_ragnar()`](#method-TempestRetriever-ingest_to_ragnar)

- [`TempestRetriever$build_ragnar_index()`](#method-TempestRetriever-build_ragnar_index)

- [`TempestRetriever$retrieve()`](#method-TempestRetriever-retrieve)

- [`TempestRetriever$get_source_text()`](#method-TempestRetriever-get_source_text)

- [`TempestRetriever$list_sources()`](#method-TempestRetriever-list_sources)

- [`TempestRetriever$cache_stats()`](#method-TempestRetriever-cache_stats)

- [`TempestRetriever$clone()`](#method-TempestRetriever-clone)

------------------------------------------------------------------------

### `TempestRetriever$new()`

Create a new TempestRetriever.

#### Usage

    TempestRetriever$new(config = tempest_config(), store = SourceStore$new())

#### Arguments

- `config`:

  A `TempestConfig` object.

- `store`:

  A `SourceStore` object.

------------------------------------------------------------------------

### `TempestRetriever$search()`

Search for sources using the configured provider.

#### Usage

    TempestRetriever$search(query, k = NULL, provider = NULL, force = FALSE)

#### Arguments

- `query`:

  Search query string.

- `k`:

  Maximum number of results.

- `provider`:

  Search provider override.

- `force`:

  If TRUE, bypass any cached result and refresh the cache.

#### Returns

A tibble of search results.

------------------------------------------------------------------------

### `TempestRetriever$fetch()`

Fetch and cache content from a URL.

#### Usage

    TempestRetriever$fetch(url, force = FALSE, perspective = NA_character_)

#### Arguments

- `url`:

  The URL to fetch.

- `force`:

  If TRUE, bypass cache.

- `perspective`:

  Optional perspective name for ragnar metadata.

#### Returns

A source object.

------------------------------------------------------------------------

### `TempestRetriever$ingest_to_ragnar()`

Ingest content into the ragnar store.

#### Usage

    TempestRetriever$ingest_to_ragnar(
      source_id,
      url,
      title,
      text,
      fetched_at,
      content_type,
      perspective = NA_character_
    )

#### Arguments

- `source_id`:

  The tempest source ID.

- `url`:

  The source URL.

- `title`:

  The document title.

- `text`:

  The full text content.

- `fetched_at`:

  Timestamp when fetched.

- `content_type`:

  Content type (html, pdf, etc.).

- `perspective`:

  Optional perspective name.

------------------------------------------------------------------------

### `TempestRetriever$build_ragnar_index()`

Build the ragnar store index for faster retrieval.

#### Usage

    TempestRetriever$build_ragnar_index()

------------------------------------------------------------------------

### `TempestRetriever$retrieve()`

Retrieve relevant chunks from the ragnar store.

#### Usage

    TempestRetriever$retrieve(query, k = 10, method = c("hybrid", "vss", "bm25"))

#### Arguments

- `query`:

  The search query.

- `k`:

  Maximum number of chunks to return.

- `method`:

  Retrieval method: "hybrid", "vss", or "bm25".

#### Returns

A data frame of relevant chunks with metadata.

------------------------------------------------------------------------

### `TempestRetriever$get_source_text()`

Get the text content for a source by id.

#### Usage

    TempestRetriever$get_source_text(source_id)

#### Arguments

- `source_id`:

  The source id.

#### Returns

The content text or NA.

------------------------------------------------------------------------

### `TempestRetriever$list_sources()`

List all sources in the store.

#### Usage

    TempestRetriever$list_sources()

#### Returns

A list of source objects.

------------------------------------------------------------------------

### `TempestRetriever$cache_stats()`

Return cache hit/miss counters for this retriever.

#### Usage

    TempestRetriever$cache_stats(reset = FALSE)

#### Arguments

- `reset`:

  Whether to reset counters after reading them.

#### Returns

A tibble with one row each for search and fetch cache counters.

------------------------------------------------------------------------

### `TempestRetriever$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TempestRetriever$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
