# SourceStore (deprecated compatibility ledger)

**\[deprecated\]**

`SourceStore` was renamed in Tempest 0.2.0. Use
[`tempest_research_workspace()`](https://jameshwade.github.io/tempest/reference/tempest_research_workspace.md)
for new code. This subclass temporarily retains the legacy arbitrary
artifact surface while product callers move to explicit STORM and
Co-STORM state.

## Super class

[`ResearchWorkspace`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.md)
-\> `SourceStore`

## Public fields

- `artifacts`:

  Legacy environment of arbitrary product artifacts.

## Methods

### Public methods

- [`SourceStore$new()`](#method-SourceStore-initialize)

- [`SourceStore$set_artifact()`](#method-SourceStore-set_artifact)

- [`SourceStore$get_artifact()`](#method-SourceStore-get_artifact)

- [`SourceStore$set_citation_audit()`](#method-SourceStore-set_citation_audit)

- [`SourceStore$clone()`](#method-SourceStore-clone)

Inherited methods

- [`ResearchWorkspace$add_claim()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-add_claim)
- [`ResearchWorkspace$add_dispute()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-add_dispute)
- [`ResearchWorkspace$add_evidence_span()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-add_evidence_span)
- [`ResearchWorkspace$add_proposed_claim()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-add_proposed_claim)
- [`ResearchWorkspace$claims_for_source()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-claims_for_source)
- [`ResearchWorkspace$get_claim()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_claim)
- [`ResearchWorkspace$get_evidence_for_claim()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_evidence_for_claim)
- [`ResearchWorkspace$get_evidence_span()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_evidence_span)
- [`ResearchWorkspace$get_proposed_claim()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_proposed_claim)
- [`ResearchWorkspace$get_resource()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_resource)
- [`ResearchWorkspace$get_retrieved_resource()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_retrieved_resource)
- [`ResearchWorkspace$get_source()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-get_source)
- [`ResearchWorkspace$link_evidence()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-link_evidence)
- [`ResearchWorkspace$list_accepted_graft_references()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_accepted_graft_references)
- [`ResearchWorkspace$list_claims()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_claims)
- [`ResearchWorkspace$list_disputes()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_disputes)
- [`ResearchWorkspace$list_evidence_spans()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_evidence_spans)
- [`ResearchWorkspace$list_proposed_claims()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_proposed_claims)
- [`ResearchWorkspace$list_resources()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_resources)
- [`ResearchWorkspace$list_retrieved_resources()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_retrieved_resources)
- [`ResearchWorkspace$list_sources()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-list_sources)
- [`ResearchWorkspace$record_accepted_graft_reference()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-record_accepted_graft_reference)
- [`ResearchWorkspace$set_max_sources()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-set_max_sources)
- [`ResearchWorkspace$to_tibbles()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-to_tibbles)
- [`ResearchWorkspace$upsert_resource()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-upsert_resource)
- [`ResearchWorkspace$upsert_retrieved_resource()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-upsert_retrieved_resource)
- [`ResearchWorkspace$upsert_source()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-upsert_source)
- [`ResearchWorkspace$verify_claim()`](https://jameshwade.github.io/tempest/reference/ResearchWorkspace.html#method-verify_claim)

------------------------------------------------------------------------

### `SourceStore$new()`

Create a deprecated SourceStore compatibility object.

#### Usage

    SourceStore$new(
      max_sources = Inf,
      base_snapshot_id = NULL,
      accepted_graft_references = list()
    )

#### Arguments

- `max_sources`:

  Maximum number of unique resources admitted.

- `base_snapshot_id`:

  Optional opaque identifier for the pinned accepted-knowledge snapshot.

- `accepted_graft_references`:

  Unnamed list of canonical JSON-compatible references to accepted graft
  records.

------------------------------------------------------------------------

### `SourceStore$set_artifact()`

Store a legacy product artifact by name.

#### Usage

    SourceStore$set_artifact(name, value)

#### Arguments

- `name`:

  Artifact name.

- `value`:

  Artifact value.

------------------------------------------------------------------------

### `SourceStore$get_artifact()`

Retrieve a legacy product artifact by name.

#### Usage

    SourceStore$get_artifact(name)

#### Arguments

- `name`:

  Artifact name.

------------------------------------------------------------------------

### `SourceStore$set_citation_audit()`

Record the latest claim-centered citation audit.

#### Usage

    SourceStore$set_citation_audit(citation_audit)

#### Arguments

- `citation_audit`:

  A citation-audit data frame, or `NULL` to clear it.

------------------------------------------------------------------------

### `SourceStore$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SourceStore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
