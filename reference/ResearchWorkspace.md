# ResearchWorkspace (provisional scientific evidence ledger)

A mutable, run-scoped workspace for retrieved resources, proposed
claims, evidence spans, disputes, and references to accepted graft
knowledge. The workspace never grants acceptance to proposed claims;
acceptance remains an explicit graft review and commit.

## Active bindings

- `retrieved_resources`:

  Read-only named-list snapshot of typed resources and built-in
  web-source records keyed by resource id.

- `proposed_claims`:

  Read-only named-list snapshot of provisional claim records keyed by
  claim id.

- `evidence_spans`:

  Read-only named-list snapshot of provisional evidence-span records.

- `disputes`:

  Read-only named-list snapshot of provisional dispute records.

- `accepted_graft_references`:

  Read-only list of opaque references to accepted graft knowledge used
  by the research run.

- `base_snapshot_id`:

  Read-only opaque identifier for the accepted knowledge snapshot on
  which this workspace is based.

- `graft_snapshot`:

  Optional read-only, path-free `graft::GraftSnapshot` used to reopen
  the accepted knowledge boundary.

- `citation_audit`:

  Latest claim-centered citation audit, when available.

- `max_sources`:

  Maximum number of unique resources admitted.

## Methods

### Public methods

- [`ResearchWorkspace$new()`](#method-ResearchWorkspace-initialize)

- [`ResearchWorkspace$set_max_sources()`](#method-ResearchWorkspace-set_max_sources)

- [`ResearchWorkspace$upsert_retrieved_resource()`](#method-ResearchWorkspace-upsert_retrieved_resource)

- [`ResearchWorkspace$get_retrieved_resource()`](#method-ResearchWorkspace-get_retrieved_resource)

- [`ResearchWorkspace$get_retrieved_source()`](#method-ResearchWorkspace-get_retrieved_source)

- [`ResearchWorkspace$list_retrieved_resources()`](#method-ResearchWorkspace-list_retrieved_resources)

- [`ResearchWorkspace$list_retrieved_sources()`](#method-ResearchWorkspace-list_retrieved_sources)

- [`ResearchWorkspace$add_proposed_claim()`](#method-ResearchWorkspace-add_proposed_claim)

- [`ResearchWorkspace$add_proposed_claims()`](#method-ResearchWorkspace-add_proposed_claims)

- [`ResearchWorkspace$add_extracted_claim_batch()`](#method-ResearchWorkspace-add_extracted_claim_batch)

- [`ResearchWorkspace$verify_proposed_claims_batch()`](#method-ResearchWorkspace-verify_proposed_claims_batch)

- [`ResearchWorkspace$get_proposed_claim()`](#method-ResearchWorkspace-get_proposed_claim)

- [`ResearchWorkspace$list_proposed_claims()`](#method-ResearchWorkspace-list_proposed_claims)

- [`ResearchWorkspace$proposed_claims_for_resource()`](#method-ResearchWorkspace-proposed_claims_for_resource)

- [`ResearchWorkspace$add_evidence_span()`](#method-ResearchWorkspace-add_evidence_span)

- [`ResearchWorkspace$get_evidence_span()`](#method-ResearchWorkspace-get_evidence_span)

- [`ResearchWorkspace$list_evidence_spans()`](#method-ResearchWorkspace-list_evidence_spans)

- [`ResearchWorkspace$link_evidence_to_proposed_claim()`](#method-ResearchWorkspace-link_evidence_to_proposed_claim)

- [`ResearchWorkspace$get_evidence_for_proposed_claim()`](#method-ResearchWorkspace-get_evidence_for_proposed_claim)

- [`ResearchWorkspace$verify_proposed_claim()`](#method-ResearchWorkspace-verify_proposed_claim)

- [`ResearchWorkspace$add_dispute()`](#method-ResearchWorkspace-add_dispute)

- [`ResearchWorkspace$list_disputes()`](#method-ResearchWorkspace-list_disputes)

- [`ResearchWorkspace$record_accepted_graft_reference()`](#method-ResearchWorkspace-record_accepted_graft_reference)

- [`ResearchWorkspace$list_accepted_graft_references()`](#method-ResearchWorkspace-list_accepted_graft_references)

- [`ResearchWorkspace$set_citation_audit()`](#method-ResearchWorkspace-set_citation_audit)

- [`ResearchWorkspace$validate_integrity()`](#method-ResearchWorkspace-validate_integrity)

- [`ResearchWorkspace$to_tibbles()`](#method-ResearchWorkspace-to_tibbles)

- [`ResearchWorkspace$clone()`](#method-ResearchWorkspace-clone)

------------------------------------------------------------------------

### `ResearchWorkspace$new()`

Create a new provisional research workspace.

#### Usage

    ResearchWorkspace$new(
      base_snapshot_id = NULL,
      graft_snapshot = NULL,
      max_sources = Inf,
      accepted_graft_references = list()
    )

#### Arguments

- `base_snapshot_id`:

  Optional opaque identifier for the pinned accepted knowledge snapshot.

- `graft_snapshot`:

  Optional real, path-free `graft::GraftSnapshot`.

- `max_sources`:

  Maximum number of unique sources. New sources are refused once the
  limit is reached.

- `accepted_graft_references`:

  Unnamed list of canonical JSON-compatible references to accepted graft
  records.

------------------------------------------------------------------------

### `ResearchWorkspace$set_max_sources()`

Set the maximum number of unique sources.

#### Usage

    ResearchWorkspace$set_max_sources(max_sources)

#### Arguments

- `max_sources`:

  A positive whole number or `Inf`.

------------------------------------------------------------------------

### `ResearchWorkspace$upsert_retrieved_resource()`

Insert or update a retrieved typed evidence resource.

#### Usage

    ResearchWorkspace$upsert_retrieved_resource(resource)

#### Arguments

- `resource`:

  A resource created by
  [`tempest_resource()`](https://jameshwade.github.io/tempest/reference/tempest_resource.md)
  or an internal built-in web-source record.

------------------------------------------------------------------------

### `ResearchWorkspace$get_retrieved_resource()`

Get a retrieved typed evidence resource by id.

#### Usage

    ResearchWorkspace$get_retrieved_resource(resource_id)

#### Arguments

- `resource_id`:

  Resource id.

------------------------------------------------------------------------

### `ResearchWorkspace$get_retrieved_source()`

Get one retrieved resource as a built-in source view.

#### Usage

    ResearchWorkspace$get_retrieved_source(resource_id)

#### Arguments

- `resource_id`:

  Resource id.

------------------------------------------------------------------------

### `ResearchWorkspace$list_retrieved_resources()`

List all retrieved evidence as typed resources.

#### Usage

    ResearchWorkspace$list_retrieved_resources()

------------------------------------------------------------------------

### `ResearchWorkspace$list_retrieved_sources()`

List retrieved resources as built-in source views.

#### Usage

    ResearchWorkspace$list_retrieved_sources()

------------------------------------------------------------------------

### `ResearchWorkspace$add_proposed_claim()`

Add a proposed claim record to the workspace.

#### Usage

    ResearchWorkspace$add_proposed_claim(claim)

#### Arguments

- `claim`:

  A `tempest_claim` S7 record.

------------------------------------------------------------------------

### `ResearchWorkspace$add_proposed_claims()`

Atomically add proposed claim records to the workspace.

#### Usage

    ResearchWorkspace$add_proposed_claims(claims, commit = NULL)

#### Arguments

- `claims`:

  A list of `tempest_claim` S7 records.

- `commit`:

  Optional zero-argument callback committed with the batch.

------------------------------------------------------------------------

### `ResearchWorkspace$add_extracted_claim_batch()`

Atomically add extracted evidence spans and claims.

#### Usage

    ResearchWorkspace$add_extracted_claim_batch(
      claims,
      evidence_spans = list(),
      commit = NULL
    )

#### Arguments

- `claims`:

  A list of `tempest_claim` S7 records.

- `evidence_spans`:

  A list of `tempest_evidence_span` S7 records.

- `commit`:

  Optional zero-argument callback committed with the batch.

------------------------------------------------------------------------

### `ResearchWorkspace$verify_proposed_claims_batch()`

Atomically verify proposed claims and set their audit.

#### Usage

    ResearchWorkspace$verify_proposed_claims_batch(
      verifications,
      citation_audit,
      commit = NULL
    )

#### Arguments

- `verifications`:

  A list of claim-verification update records.

- `citation_audit`:

  The complete claim-centered citation audit.

- `commit`:

  Optional zero-argument callback committed with the batch.

------------------------------------------------------------------------

### `ResearchWorkspace$get_proposed_claim()`

Get a proposed claim by id.

#### Usage

    ResearchWorkspace$get_proposed_claim(claim_id)

#### Arguments

- `claim_id`:

  The claim id.

------------------------------------------------------------------------

### `ResearchWorkspace$list_proposed_claims()`

List all proposed claims.

#### Usage

    ResearchWorkspace$list_proposed_claims()

------------------------------------------------------------------------

### `ResearchWorkspace$proposed_claims_for_resource()`

Proposed claims that cite a retrieved resource.

#### Usage

    ResearchWorkspace$proposed_claims_for_resource(resource_id)

#### Arguments

- `resource_id`:

  Resource id.

------------------------------------------------------------------------

### `ResearchWorkspace$add_evidence_span()`

Add an evidence span.

#### Usage

    ResearchWorkspace$add_evidence_span(span)

#### Arguments

- `span`:

  A `tempest_evidence_span` S7 record.

------------------------------------------------------------------------

### `ResearchWorkspace$get_evidence_span()`

Get an evidence span by id.

#### Usage

    ResearchWorkspace$get_evidence_span(span_id)

#### Arguments

- `span_id`:

  Evidence span id.

------------------------------------------------------------------------

### `ResearchWorkspace$list_evidence_spans()`

List all evidence spans in deterministic id order.

#### Usage

    ResearchWorkspace$list_evidence_spans()

------------------------------------------------------------------------

### `ResearchWorkspace$link_evidence_to_proposed_claim()`

Link an evidence span to a claim.

#### Usage

    ResearchWorkspace$link_evidence_to_proposed_claim(claim_id, span_id)

#### Arguments

- `claim_id`:

  Claim id.

- `span_id`:

  Evidence span id.

------------------------------------------------------------------------

### `ResearchWorkspace$get_evidence_for_proposed_claim()`

Evidence spans linked to a claim.

#### Usage

    ResearchWorkspace$get_evidence_for_proposed_claim(claim_id)

#### Arguments

- `claim_id`:

  Claim id.

------------------------------------------------------------------------

### `ResearchWorkspace$verify_proposed_claim()`

Update a claim's verification status.

#### Usage

    ResearchWorkspace$verify_proposed_claim(
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

### `ResearchWorkspace$add_dispute()`

Add a dispute.

#### Usage

    ResearchWorkspace$add_dispute(dispute)

#### Arguments

- `dispute`:

  A `tempest_dispute` S7 record.

------------------------------------------------------------------------

### `ResearchWorkspace$list_disputes()`

List all disputes.

#### Usage

    ResearchWorkspace$list_disputes()

------------------------------------------------------------------------

### `ResearchWorkspace$record_accepted_graft_reference()`

Record a reference to accepted graft knowledge.

#### Usage

    ResearchWorkspace$record_accepted_graft_reference(reference)

#### Arguments

- `reference`:

  Opaque canonical JSON-compatible graft reference.

------------------------------------------------------------------------

### `ResearchWorkspace$list_accepted_graft_references()`

List accepted graft references deterministically.

#### Usage

    ResearchWorkspace$list_accepted_graft_references()

------------------------------------------------------------------------

### `ResearchWorkspace$set_citation_audit()`

Record the latest claim-centered citation audit.

#### Usage

    ResearchWorkspace$set_citation_audit(citation_audit)

#### Arguments

- `citation_audit`:

  A citation-audit data frame, or `NULL` to clear it.

------------------------------------------------------------------------

### `ResearchWorkspace$validate_integrity()`

Validate all authoritative workspace cross-record links.

#### Usage

    ResearchWorkspace$validate_integrity()

------------------------------------------------------------------------

### `ResearchWorkspace$to_tibbles()`

Convert sources, claims, and disputes to tibbles.

#### Usage

    ResearchWorkspace$to_tibbles()

------------------------------------------------------------------------

### `ResearchWorkspace$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResearchWorkspace$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
