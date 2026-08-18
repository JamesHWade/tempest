test_that("tempest_research_workspace creates explicit provisional compartments", {
  references <- list(
    list(snapshot_id = "snapshot-1", record_id = "record-z"),
    list(record_id = "record-a", snapshot_id = "snapshot-1")
  )
  workspace <- tempest_research_workspace(
    base_snapshot_id = " snapshot-1 ",
    max_sources = 3L,
    accepted_graft_references = references
  )

  expect_r6_class(workspace, "ResearchWorkspace")
  expect_length(workspace$retrieved_resources, 0L)
  expect_length(workspace$proposed_claims, 0L)
  expect_equal(workspace$base_snapshot_id, "snapshot-1")
  expect_equal(workspace$max_sources, 3L)
  expect_contains(
    names(workspace),
    c(
      "accepted_graft_references",
      "claim_supports",
      "citation_audit",
      "disputes",
      "evidence_spans",
      "proposed_claims",
      "retrieved_resources"
    )
  )
  expect_equal(
    vapply(
      workspace$list_accepted_graft_references(),
      `[[`,
      character(1),
      "record_id"
    ),
    c("record-a", "record-z")
  )

  references[[1]]$record_id <- "changed-input"
  returned <- workspace$accepted_graft_references
  returned[[1]]$record_id <- "changed-output"
  expect_equal(
    workspace$accepted_graft_references[[1]]$record_id,
    "record-a"
  )
  expect_equal(
    c("artifacts", "set_artifact", "get_artifact") %in% names(workspace),
    rep(FALSE, 3L)
  )
})

test_that("ResearchWorkspace validates snapshot and accepted references", {
  expect_snapshot(
    error = TRUE,
    tempest_research_workspace(base_snapshot_id = character())
  )
  expect_snapshot(
    error = TRUE,
    tempest_research_workspace(base_snapshot_id = " ")
  )
  expect_snapshot(
    error = TRUE,
    tempest_research_workspace(accepted_graft_references = data.frame())
  )
  expect_snapshot(
    error = TRUE,
    tempest_research_workspace(
      accepted_graft_references = list(list(record_id = NA_character_))
    )
  )
  expect_error(
    tempest_research_workspace(
      accepted_graft_references = list(function() NULL)
    ),
    class = "tempest_research_workspace_error",
    regexp = "non-empty named records"
  )
  expect_snapshot(
    error = TRUE,
    tempest_research_workspace(
      accepted_graft_references = list(named = list(record_id = "record-a"))
    )
  )

  sensitive <- expect_error(
    tempest_research_workspace(
      accepted_graft_references = list(
        list(metadata = list(apiKey = "secret"), record_id = "record-a")
      )
    ),
    class = "tempest_research_workspace_error"
  )
  expect_match(conditionMessage(sensitive), "credential-like")
  expect_match(conditionMessage(sensitive), "apiKey")

  RuntimeReference <- R6::R6Class("WorkspaceRuntimeReference")
  connection <- file(withr::local_tempfile())
  withr::defer(close(connection))
  forbidden <- list(
    new.env(parent = emptyenv()),
    connection,
    methods::new("externalptr"),
    RuntimeReference$new(),
    tempest_config()
  )
  for (value in forbidden) {
    expect_error(
      tempest_research_workspace(
        accepted_graft_references = list(list(runtime = value))
      ),
      class = "tempest_research_workspace_error",
      regexp = "cannot contain|plain JSON-compatible"
    )
  }
  expect_error(
    tempest_research_workspace(
      accepted_graft_references = list(list(label = "not-an-id"))
    ),
    class = "tempest_research_workspace_error",
    regexp = "at least one non-empty.*_id"
  )
  expect_error(
    tempest_research_workspace(
      accepted_graft_references = list(list(record_id = 42L))
    ),
    class = "tempest_research_workspace_error",
    regexp = "single non-empty string"
  )
  expect_error(
    tempest_research_workspace(
      accepted_graft_references = list(list(
        record_id = "record-a",
        metadata = list(procedure_id = "")
      ))
    ),
    class = "tempest_research_workspace_error",
    regexp = "procedure_id.*single non-empty string"
  )
  expect_error(
    tempest_research_workspace(
      base_snapshot_id = "snapshot-a",
      accepted_graft_references = list(list(
        record_id = "record-a",
        snapshot_id = "snapshot-b"
      ))
    ),
    class = "tempest_research_workspace_error",
    regexp = "snapshot does not match"
  )
})

test_that("accepted graft references use canonical JSON array forms", {
  reference <- list(
    record_id = "record-a",
    metadata = structure(
      list(c(1, 2), character(), c(TRUE, FALSE), 0.5),
      names = c("", "", "", "")
    )
  )
  references <- list(reference)
  workspace <- tempest_research_workspace(
    accepted_graft_references = references
  )
  stored <- workspace$accepted_graft_references[[1]]

  expect_null(names(stored$metadata))
  expect_identical(stored$metadata[[1]], list(1L, 2L))
  expect_identical(stored$metadata[[2]], list())
  expect_identical(stored$metadata[[3]], list(TRUE, FALSE))
  expect_identical(stored$metadata[[4]], 0.5)
  expect_identical(
    jsonlite::fromJSON(
      tempest_research_workspace_reference_json(stored),
      simplifyVector = FALSE
    ),
    stored
  )

  references[[1]]$metadata[[1]][[1]] <- 99
  returned <- workspace$accepted_graft_references
  returned[[1]]$metadata[[1]][[1]] <- 88L
  expect_identical(
    workspace$accepted_graft_references[[1]]$metadata[[1]],
    list(1L, 2L)
  )
})

test_that("accepted graft identifiers reject credential-shaped values", {
  for (identifier in c(
    "Authorization:Bearer-sk-live-secret",
    "sk-live-secret",
    "api_key-secret"
  )) {
    expect_error(
      tempest_research_workspace(
        accepted_graft_references = list(list(record_id = identifier))
      ),
      class = "tempest_research_workspace_integrity_error"
    )
  }
})

test_that("ResearchWorkspace listings are deterministic", {
  workspace <- tempest_research_workspace(
    accepted_graft_references = list(
      list(record_id = "record-z"),
      list(record_id = "record-a"),
      list(record_id = "record-z")
    )
  )
  source_z <- fake_source("https://example.org/z")
  source_a <- fake_source("https://example.org/a")
  workspace$upsert_retrieved_resource(source_z)
  workspace$upsert_retrieved_resource(source_a)

  workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-z",
    claim_text = "Z claim",
    source_ids = source_z$id
  ))
  workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-a",
    claim_text = "A claim",
    source_ids = source_z$id
  ))
  workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-z",
    source_id = source_z$id,
    quote = "Photosynthesis converts light"
  ))
  workspace$add_evidence_span(tempest_evidence_span(
    evidence_span_id = "span-a",
    source_id = source_a$id,
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
    sort(c(source_z$id, source_a$id))
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
      workspace$proposed_claims_for_resource(source_z$id),
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
  expect_equal(
    vapply(
      workspace$list_accepted_graft_references(),
      `[[`,
      character(1),
      "record_id"
    ),
    c("record-a", "record-z")
  )
})

test_that("proposed claims cannot become accepted through workspace mutation", {
  workspace <- tempest_research_workspace(
    accepted_graft_references = list(list(record_id = "accepted-claim"))
  )
  claim <- tempest_claim(claim_id = "proposal", claim_text = "A proposal")
  workspace$add_proposed_claim(claim)

  expect_equal("accepted" %in% S7::prop_names(claim), FALSE)
  expect_equal(workspace$list_proposed_claims(), list(claim))
  expect_equal(
    workspace$accepted_graft_references,
    list(list(record_id = "accepted-claim"))
  )
  expect_snapshot(error = TRUE, S7::set_props(claim, accepted = TRUE))
  expect_snapshot(
    error = TRUE,
    workspace$accepted_graft_references <- list(list(record_id = "proposal"))
  )
  expect_snapshot(
    error = TRUE,
    workspace$base_snapshot_id <- "another-snapshot"
  )
})

test_that("ResearchWorkspace exposes copies instead of mutable backing stores", {
  workspace <- tempest_research_workspace(max_sources = 2L)
  source <- fake_source("https://example.org/private")
  workspace$upsert_retrieved_resource(source)
  claim <- tempest_claim(
    claim_id = "claim-private",
    claim_text = "The workspace stays private.",
    source_ids = source$id
  )
  workspace$add_proposed_claim(claim)

  resources <- workspace$retrieved_resources
  resources[[source$id]] <- S7::set_props(
    resources[[source$id]],
    title = "Changed outside"
  )
  claims <- workspace$proposed_claims
  claims[["claim-private"]] <- S7::set_props(
    claims[["claim-private"]],
    claim_text = "Changed outside"
  )

  expect_identical(
    workspace$get_retrieved_source(source$id)$title,
    source$title
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
  source_a <- fake_source("https://example.org/source-a")
  source_b <- fake_source("https://example.org/source-b")
  workspace$upsert_retrieved_resource(source_a)
  workspace$upsert_retrieved_resource(source_b)
  span <- tempest_evidence_span(
    evidence_span_id = "span-shared",
    source_id = source_a$id,
    quote = "Photosynthesis converts light"
  )
  workspace$add_evidence_span(span)

  expect_error(
    workspace$add_proposed_claim(tempest_claim(
      claim_id = "claim-mismatch",
      claim_text = "Mismatched evidence",
      source_ids = source_b$id,
      evidence_span_ids = span@evidence_span_id
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "source cited by the claim"
  )

  claim <- tempest_claim(
    claim_id = "claim-linked",
    claim_text = "Linked evidence",
    source_ids = source_a$id,
    evidence_span_ids = span@evidence_span_id,
    supporting_quotes = list(span@quote)
  )
  workspace$add_proposed_claim(claim)
  expect_error(
    workspace$add_evidence_span(tempest_evidence_span(
      evidence_span_id = span@evidence_span_id,
      source_id = source_b$id,
      quote = "Photosynthesis converts light"
    )),
    class = "tempest_research_workspace_integrity_error",
    regexp = "Cannot replace linked evidence span"
  )
  expect_identical(
    workspace$get_evidence_span(span@evidence_span_id)@source_id,
    source_a$id
  )
})

test_that("linking evidence atomically maintains ordered quote lineage", {
  workspace <- tempest_research_workspace()
  source <- fake_source()
  workspace$upsert_retrieved_resource(source)
  claim_id <- workspace$add_proposed_claim(tempest_claim(
    claim_id = "claim-link-lineage",
    claim_text = "Photosynthesis stores chemical energy.",
    source_ids = source$id
  ))
  spans <- list(
    tempest_evidence_span(
      evidence_span_id = "span-link-first",
      source_id = source$id,
      quote = "Photosynthesis"
    ),
    tempest_evidence_span(
      evidence_span_id = "span-link-unquoted",
      source_id = source$id
    ),
    tempest_evidence_span(
      evidence_span_id = "span-link-last",
      source_id = source$id,
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
