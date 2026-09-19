test_that("tempest_research_workspace creates explicit provisional compartments", {
  references <- list(
    list(snapshot_id = "snapshot-1", record_id = "record-z"),
    list(record_id = "record-a", snapshot_id = "snapshot-1")
  )
  workspace <- tempest_research_workspace(
    max_sources = 3L
  )

  expect_r6_class(workspace, "ResearchWorkspace")
  expect_length(workspace$retrieved_resources, 0L)
  expect_length(workspace$proposed_claims, 0L)

  expect_equal(workspace$max_sources, 3L)
  expect_contains(
    names(workspace),
    c(
      "claim_supports",
      "citation_audit",
      "disputes",
      "evidence_spans",
      "proposed_claims",
      "retrieved_resources"
    )
  )

  references[[1]]$record_id <- "changed-input"

  expect_equal(
    c("artifacts", "set_artifact", "get_artifact") %in% names(workspace),
    rep(FALSE, 3L)
  )
})


test_that("ResearchWorkspace listings are deterministic", {
  workspace <- tempest_research_workspace()
  source_z <- test_typed_web_resource("https://example.org/z")
  source_a <- test_typed_web_resource("https://example.org/a")
  workspace$upsert_retrieved_resource(source_z)
  workspace$upsert_retrieved_resource(source_a)

  workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-z",
    claim_text = "Z claim",
    source_ids = source_z@resource_id
  ))
  workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-a",
    claim_text = "A claim",
    source_ids = source_z@resource_id
  ))
  workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-z",
    source_id = source_z@resource_id,
    quote = "Photosynthesis converts light"
  ))
  workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-a",
    source_id = source_a@resource_id,
    quote = "chemical energy"
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-z",
    topic = "Z dispute",
    claim_ids = c("claim-z", "claim-a")
  ))
  workspace$add_dispute(tempest_dispute(
    dispute_id = "dispute-a",
    topic = "A dispute",
    claim_ids = c("claim-z", "claim-a")
  ))

  expect_equal(
    vapply(workspace$list_retrieved_sources(), `[[`, character(1), "id"),
    sort(c(source_z@resource_id, source_a@resource_id))
  )
  expect_equal(
    vapply(
      workspace$list_proposed_claims(),
      \(claim) S7::prop(claim, "claim_id"),
      character(1)
    ),
    c("claim-a", "claim-z")
  )
  expect_equal(
    vapply(
      workspace$proposed_claims_for_resource(source_z@resource_id),
      \(claim) S7::prop(claim, "claim_id"),
      character(1)
    ),
    c("claim-a", "claim-z")
  )
  expect_equal(
    vapply(
      workspace$list_evidence_spans(),
      \(span) S7::prop(span, "evidence_span_id"),
      character(1)
    ),
    c("span-a", "span-z")
  )
  expect_equal(
    vapply(
      workspace$list_disputes(),
      \(dispute) S7::prop(dispute, "dispute_id"),
      character(1)
    ),
    c("dispute-a", "dispute-z")
  )
})

test_that("proposed claims cannot become accepted through workspace mutation", {
  workspace <- tempest_research_workspace()
  claim <- tempest_claim(claim_id = "proposal", claim_text = "A proposal")
  workspace$add_proposed_claim(claim)

  expect_equal("accepted" %in% S7::prop_names(claim), FALSE)
  expect_equal(workspace$list_proposed_claims(), list(claim))

  expect_snapshot(error = TRUE, S7::set_props(claim, accepted = TRUE))
})

test_that("ResearchWorkspace exposes copies instead of mutable backing stores", {
  workspace <- tempest_research_workspace(max_sources = 2L)
  source <- test_typed_web_resource("https://example.org/private")
  workspace$upsert_retrieved_resource(source)
  claim <- tempest_claim(
    claim_id = "claim-private",
    claim_text = "The workspace stays private.",
    source_ids = source@resource_id
  )
  workspace$add_proposed_claim(claim)

  resources <- workspace$retrieved_resources
  resources[[source@resource_id]] <- S7::set_props(
    resources[[source@resource_id]],
    title = "Changed outside"
  )
  claims <- workspace$proposed_claims
  claims[["claim-private"]] <- S7::set_props(
    claims[["claim-private"]],
    claim_text = "Changed outside"
  )

  expect_identical(
    workspace$get_retrieved_source(source@resource_id)$title,
    source@title
  )
  expect_identical(
    workspace$get_proposed_claim("claim-private")@claim_text,
    "The workspace stays private."
  )
  expect_type(workspace$retrieved_resources, "list")
  expect_type(workspace$proposed_claims, "list")
  expect_error(
    workspace$retrieved_resources <- list(),
    class = "tempest_research_workspace_error",
    regexp = "read-only snapshot"
  )
  expect_error(
    workspace$proposed_claims[["claim-private"]] <- claim,
    class = "tempest_research_workspace_error",
    regexp = "read-only snapshot"
  )
  expect_error(
    workspace$max_sources <- 10L,
    class = "tempest_research_workspace_error",
    regexp = "set_max_sources"
  )
})

test_that("ResearchWorkspace revalidates product-owned content hashes", {
  workspace <- tempest_research_workspace()
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/content-hash",
    title = "Content hash",
    media_type = "text/plain",
    content = "Exact evidence bytes."
  )
  tampered <- S7::set_props(
    resource,
    content_hash = strrep("0", 64L)
  )

  expect_error(
    workspace$upsert_retrieved_resource(tampered),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(workspace$list_retrieved_resources(), 0L)
  expect_identical(
    workspace$upsert_retrieved_resource(resource),
    resource@resource_id
  )
})

test_that("claims and evidence spans retain source coherence", {
  workspace <- tempest_research_workspace()
  source_a <- test_typed_web_resource("https://example.org/source-a")
  source_b <- test_typed_web_resource("https://example.org/source-b")
  workspace$upsert_retrieved_resource(source_a)
  workspace$upsert_retrieved_resource(source_b)
  span <- tempest_evidence_span(
    evidence_span_id = "span-shared",
    source_id = source_a@resource_id,
    quote = "Photosynthesis converts light"
  )
  workspace$add_evidence_span(span)

  expect_error(
    workspace$add_proposed_claim(tempest_claim(
      claim_id = "claim-mismatch",
      claim_text = "Mismatched evidence",
      source_ids = source_b@resource_id,
      evidence_span_ids = span@evidence_span_id
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "source cited by the claim"
  )

  claim <- tempest_claim(
    claim_id = "claim-linked",
    claim_text = "Linked evidence",
    source_ids = source_a@resource_id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(span@quote)
  )
  workspace$add_proposed_claim(claim)
  expect_error(
    workspace$add_evidence_span(tempest_evidence_span(
      evidence_span_id = span@evidence_span_id,
      source_id = source_b@resource_id,
      quote = "Photosynthesis converts light"
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "Cannot replace linked evidence span"
  )
  expect_identical(
    workspace$get_evidence_span(span@evidence_span_id)@source_id,
    source_a@resource_id
  )
})

test_that("linking evidence atomically maintains ordered quote lineage", {
  workspace <- tempest_research_workspace()
  source <- test_typed_web_resource()
  workspace$upsert_retrieved_resource(source)
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-link-lineage",
    claim_text = "Photosynthesis stores chemical energy.",
    source_ids = source@resource_id
  ))
  spans <- list(
    tempest_evidence_span(
      evidence_span_id = "span-link-first",
      source_id = source@resource_id,
      quote = "Photosynthesis"
    ),
    tempest_evidence_span(
      evidence_span_id = "span-link-unquoted",
      source_id = source@resource_id
    ),
    tempest_evidence_span(
      evidence_span_id = "span-link-last",
      source_id = source@resource_id,
      quote = "chemical energy"
    )
  )
  span_ids <- vapply(
    spans,
    workspace$add_evidence_span,
    character(1)
  )

  for (span_id in span_ids) {
    expect_identical(
      workspace$link_evidence_to_proposed_claim(claim_id, span_id),
      claim_id
    )
  }

  linked <- workspace$get_proposed_claim(claim_id)
  expect_identical(linked@evidence_span_ids, span_ids)
  expect_identical(
    linked@supporting_quotes,
    list("Photosynthesis", "chemical energy")
  )
  expect_no_error(
    workspace$link_evidence_to_proposed_claim(claim_id, span_ids[[1]])
  )
  expect_identical(workspace$get_proposed_claim(claim_id), linked)
})

test_that("ResearchWorkspace exposes only derived read-only support views", {
  verified <- test_verified_workspace()
  workspace <- verified$workspace
  audit <- workspace$citation_audit
  supports <- workspace$claim_supports

  expect_identical("set_citation_audit" %in% names(workspace), FALSE)
  expect_identical("verify_proposed_claim" %in% names(workspace), FALSE)
  expect_error(
    workspace$citation_audit <- NULL,
    class = "tempest_research_workspace_error"
  )
  expect_error(
    workspace$claim_supports <- list(),
    class = "tempest_research_workspace_error"
  )

  audit$support_score[[1]] <- 0
  supports[[1]]@support_score <- 0
  expect_identical(workspace$citation_audit$support_score, 0.9)
  expect_identical(workspace$list_claim_supports()[[1]]@support_score, 0.9)
})
