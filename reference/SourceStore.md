# SourceStore

SourceStore

SourceStore

## Details

A lightweight in-memory store for sources, fact notes, and artifacts.
This store is designed for both script usage (STORM) and interactive
usage (Co-STORM).

## Public fields

- `sources`:

  Environment of source objects keyed by id.

- `facts`:

  List of fact objects.

- `artifacts`:

  Environment of arbitrary artifacts.

## Methods

### Public methods

- [`SourceStore$new()`](#method-SourceStore-new)

- [`SourceStore$upsert_source()`](#method-SourceStore-upsert_source)

- [`SourceStore$get_source()`](#method-SourceStore-get_source)

- [`SourceStore$list_sources()`](#method-SourceStore-list_sources)

- [`SourceStore$add_fact()`](#method-SourceStore-add_fact)

- [`SourceStore$list_facts()`](#method-SourceStore-list_facts)

- [`SourceStore$set_artifact()`](#method-SourceStore-set_artifact)

- [`SourceStore$get_artifact()`](#method-SourceStore-get_artifact)

- [`SourceStore$to_tibbles()`](#method-SourceStore-to_tibbles)

- [`SourceStore$clone()`](#method-SourceStore-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SourceStore.

#### Usage

    SourceStore$new()

------------------------------------------------------------------------

### Method `upsert_source()`

Insert or update a source.

#### Usage

    SourceStore$upsert_source(source)

#### Arguments

- `source`:

  A source list with an `id` field.

------------------------------------------------------------------------

### Method `get_source()`

Get a source by id.

#### Usage

    SourceStore$get_source(source_id)

#### Arguments

- `source_id`:

  The source id.

#### Returns

The source list or NULL.

------------------------------------------------------------------------

### Method `list_sources()`

List all sources.

#### Usage

    SourceStore$list_sources()

#### Returns

A list of source objects.

------------------------------------------------------------------------

### Method `add_fact()`

Add a fact to the store.

#### Usage

    SourceStore$add_fact(fact)

#### Arguments

- `fact`:

  A fact list with an `id` field.

------------------------------------------------------------------------

### Method `list_facts()`

List all facts.

#### Usage

    SourceStore$list_facts()

#### Returns

A list of fact objects.

------------------------------------------------------------------------

### Method `set_artifact()`

Store an artifact by name.

#### Usage

    SourceStore$set_artifact(name, value)

#### Arguments

- `name`:

  Artifact name.

- `value`:

  Artifact value.

------------------------------------------------------------------------

### Method `get_artifact()`

Retrieve an artifact by name.

#### Usage

    SourceStore$get_artifact(name)

#### Arguments

- `name`:

  Artifact name.

#### Returns

The artifact value or NULL.

------------------------------------------------------------------------

### Method `to_tibbles()`

Convert sources and facts to tibbles.

#### Usage

    SourceStore$to_tibbles()

#### Returns

A list with `sources` and `facts` tibbles.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SourceStore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
