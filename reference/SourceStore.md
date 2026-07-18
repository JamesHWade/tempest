# SourceStore (evidence ledger)

In-memory store for sources, claims, evidence spans, disputes, and
artifacts. Stays an R6 reference object: it is the mutable accumulator
threaded through every pipeline stage and Co-STORM turn.

## Public fields

- `resources`:

  Environment of typed resources and built-in web-source records keyed
  by resource id.

- `sources`:

  Alias of `resources` retained for built-in web adapters.

- `claims`:

  Environment of claim records keyed by claim_id.

- `evidence_spans`:

  Environment of evidence-span records keyed by id.

- `disputes`:

  Environment of dispute records keyed by id.

- `artifacts`:

  Environment of arbitrary artifacts.

- `max_sources`:

  Maximum number of unique sources admitted by the store.

## Methods

### Public methods

- [`SourceStore$new()`](#method-SourceStore-initialize)

- [`SourceStore$set_max_sources()`](#method-SourceStore-set_max_sources)

- [`SourceStore$upsert_source()`](#method-SourceStore-upsert_source)

- [`SourceStore$get_source()`](#method-SourceStore-get_source)

- [`SourceStore$list_sources()`](#method-SourceStore-list_sources)

- [`SourceStore$upsert_resource()`](#method-SourceStore-upsert_resource)

- [`SourceStore$get_resource()`](#method-SourceStore-get_resource)

- [`SourceStore$list_resources()`](#method-SourceStore-list_resources)

- [`SourceStore$add_claim()`](#method-SourceStore-add_claim)

- [`SourceStore$get_claim()`](#method-SourceStore-get_claim)

- [`SourceStore$list_claims()`](#method-SourceStore-list_claims)

- [`SourceStore$claims_for_source()`](#method-SourceStore-claims_for_source)

- [`SourceStore$add_evidence_span()`](#method-SourceStore-add_evidence_span)

- [`SourceStore$link_evidence()`](#method-SourceStore-link_evidence)

- [`SourceStore$get_evidence_for_claim()`](#method-SourceStore-get_evidence_for_claim)

- [`SourceStore$verify_claim()`](#method-SourceStore-verify_claim)

- [`SourceStore$add_dispute()`](#method-SourceStore-add_dispute)

- [`SourceStore$list_disputes()`](#method-SourceStore-list_disputes)

- [`SourceStore$set_artifact()`](#method-SourceStore-set_artifact)

- [`SourceStore$get_artifact()`](#method-SourceStore-get_artifact)

- [`SourceStore$to_tibbles()`](#method-SourceStore-to_tibbles)

- [`SourceStore$clone()`](#method-SourceStore-clone)

------------------------------------------------------------------------

### `SourceStore$new()`

Create a new SourceStore.

#### Usage

    SourceStore$new(max_sources = Inf)

#### Arguments

- `max_sources`:

  Maximum number of unique sources. New sources are refused once the
  limit is reached.

------------------------------------------------------------------------

### `SourceStore$set_max_sources()`

Set the maximum number of unique sources.

#### Usage

    SourceStore$set_max_sources(max_sources)

#### Arguments

- `max_sources`:

  A positive whole number or `Inf`.

------------------------------------------------------------------------

### `SourceStore$upsert_source()`

Insert or update a source.

#### Usage

    SourceStore$upsert_source(source)

#### Arguments

- `source`:

  A built-in web-source list or a typed resource created by
  [`tempest_resource()`](https://jameshwade.github.io/tempest/reference/tempest_resource.md).

------------------------------------------------------------------------

### `SourceStore$get_source()`

Get a source by id.

#### Usage

    SourceStore$get_source(source_id)

#### Arguments

- `source_id`:

  The source id.

------------------------------------------------------------------------

### `SourceStore$list_sources()`

List all sources.

#### Usage

    SourceStore$list_sources()

------------------------------------------------------------------------

### `SourceStore$upsert_resource()`

Insert or update a typed evidence resource.

#### Usage

    SourceStore$upsert_resource(resource)

#### Arguments

- `resource`:

  A resource created by
  [`tempest_resource()`](https://jameshwade.github.io/tempest/reference/tempest_resource.md).

------------------------------------------------------------------------

### `SourceStore$get_resource()`

Get a typed evidence resource by id.

#### Usage

    SourceStore$get_resource(resource_id)

#### Arguments

- `resource_id`:

  Resource id.

------------------------------------------------------------------------

### `SourceStore$list_resources()`

List all evidence as typed resources.

#### Usage

    SourceStore$list_resources()

------------------------------------------------------------------------

### `SourceStore$add_claim()`

Add a claim record to the ledger.

#### Usage

    SourceStore$add_claim(claim)

#### Arguments

- `claim`:

  A `tempest_claim` S7 record.

------------------------------------------------------------------------

### `SourceStore$get_claim()`

Get a claim by id.

#### Usage

    SourceStore$get_claim(claim_id)

#### Arguments

- `claim_id`:

  The claim id.

------------------------------------------------------------------------

### `SourceStore$list_claims()`

List all claims.

#### Usage

    SourceStore$list_claims()

------------------------------------------------------------------------

### `SourceStore$claims_for_source()`

Claims that cite a given source.

#### Usage

    SourceStore$claims_for_source(source_id)

#### Arguments

- `source_id`:

  Source id.

------------------------------------------------------------------------

### `SourceStore$add_evidence_span()`

Add an evidence span.

#### Usage

    SourceStore$add_evidence_span(span)

#### Arguments

- `span`:

  A `tempest_evidence_span` S7 record.

------------------------------------------------------------------------

### `SourceStore$link_evidence()`

Link an evidence span to a claim.

#### Usage

    SourceStore$link_evidence(claim_id, span_id)

#### Arguments

- `claim_id`:

  Claim id.

- `span_id`:

  Evidence span id.

------------------------------------------------------------------------

### `SourceStore$get_evidence_for_claim()`

Evidence spans linked to a claim.

#### Usage

    SourceStore$get_evidence_for_claim(claim_id)

#### Arguments

- `claim_id`:

  Claim id.

------------------------------------------------------------------------

### `SourceStore$verify_claim()`

Update a claim's verification status.

#### Usage

    SourceStore$verify_claim(
      claim_id,
      status,
      score = NA_real_,
      verifier = NA_character_
    )

#### Arguments

- `claim_id`:

  Claim id.

- `status`:

  One of the verification status labels.

- `score`:

  Support score in `[0, 1]` or NA.

- `verifier`:

  Verifier model id.

------------------------------------------------------------------------

### `SourceStore$add_dispute()`

Add a dispute.

#### Usage

    SourceStore$add_dispute(dispute)

#### Arguments

- `dispute`:

  A `tempest_dispute` S7 record.

------------------------------------------------------------------------

### `SourceStore$list_disputes()`

List all disputes.

#### Usage

    SourceStore$list_disputes()

------------------------------------------------------------------------

### `SourceStore$set_artifact()`

Store an artifact by name.

#### Usage

    SourceStore$set_artifact(name, value)

#### Arguments

- `name`:

  Artifact name.

- `value`:

  Artifact value.

------------------------------------------------------------------------

### `SourceStore$get_artifact()`

Retrieve an artifact by name.

#### Usage

    SourceStore$get_artifact(name)

#### Arguments

- `name`:

  Artifact name.

------------------------------------------------------------------------

### `SourceStore$to_tibbles()`

Convert sources, claims, and disputes to tibbles.

#### Usage

    SourceStore$to_tibbles()

------------------------------------------------------------------------

### `SourceStore$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SourceStore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
