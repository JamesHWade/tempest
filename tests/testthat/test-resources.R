test_that("typed resources support non-web evidence and durable records", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/brief-42",
    title = "Approved brief",
    media_type = "text/plain",
    content = "Use the selected expert team.",
    origin_connection_id = "knowledge-base",
    scope_metadata = list(tenant_id = "tenant-7"),
    redaction = list(status = "reviewed"),
    retention = list(policy = "project")
  )

  expect_s7_class(resource, tempest:::TempestResource)
  expect_match(resource@resource_id, "^R")
  expect_equal(resource@resource_kind, "host.document")
  expect_equal(resource@origin_connection_id, "knowledge-base")
  expect_match(resource@content_hash, "^[a-f0-9]{64}$")

  record <- tempest:::tempest_resource_record(resource)
  restored <- tempest:::tempest_resource_from_data(record)
  expect_identical(
    tempest:::tempest_resource_record(restored),
    record
  )
})

test_that("resource records reject live runtime values and tampering", {
  expect_error(
    tempest_resource(
      resource_kind = "host.document",
      locator = "documents/unsafe",
      title = "Unsafe",
      media_type = "application/json",
      content = list(client = function() NULL)
    ),
    class = "tempest_workflow_spec_error"
  )

  resource <- tempest_resource(
    resource_kind = "database.result",
    locator = "queries/result-9",
    title = "Approved query result",
    media_type = "application/json",
    content = list(rows = list(list(metric = "retention", value = 0.9)))
  )
  record <- tempest:::tempest_resource_record(resource)
  record$title <- "Changed"
  expect_error(
    tempest:::tempest_resource_from_data(record),
    class = "tempest_artifact_codec_error"
  )
})

test_that("resource metadata rejects credential-like fields recursively", {
  base_args <- list(
    resource_kind = "host.document",
    locator = "documents/credential-boundary",
    title = "Credential boundary",
    media_type = "text/plain"
  )
  sensitive <- list(
    scope_metadata = list(provider = list(apiKey = "secret")),
    redaction = list(provider = list(authorizationHeader = "secret")),
    retention = list(provider = list(clientSecret = "secret")),
    metadata = list(provider = list(refreshToken = "secret"))
  )
  sensitive_names <- c(
    scope_metadata = "apiKey",
    redaction = "authorizationHeader",
    retention = "clientSecret",
    metadata = "refreshToken"
  )

  for (field in names(sensitive)) {
    args <- base_args
    args[[field]] <- sensitive[[field]]
    error <- expect_error(
      do.call(tempest_resource, args),
      class = "tempest_workflow_spec_error"
    )
    expect_match(conditionMessage(error), "credential-like")
    expect_match(
      conditionMessage(error),
      sensitive_names[[field]],
      fixed = TRUE
    )
  }
})

test_that("resource records revalidate credential-like metadata", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/mutated-credential-boundary",
    title = "Mutated credential boundary",
    media_type = "text/plain"
  )
  resource@metadata <- list(provider = list(API_KEY = "secret"))

  error <- expect_error(
    tempest:::tempest_resource_record(resource),
    class = "tempest_workflow_spec_error"
  )
  expect_match(conditionMessage(error), "credential-like")
  expect_match(conditionMessage(error), "API_KEY", fixed = TRUE)
})

test_that("resource metadata preserves benign nested fields", {
  metadata <- list(
    provenance = list(
      model_id = "model-reviewer",
      token_count = 12L,
      max_tokens = 100L
    ),
    review_status = "approved"
  )
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/benign-metadata",
    title = "Benign metadata",
    media_type = "text/plain",
    metadata = metadata
  )

  expect_identical(resource@metadata, metadata)
})

test_that("ResearchWorkspace indexes typed resources without web URLs", {
  store <- test_research_workspace()
  resource <- tempest_resource(
    resource_kind = "email.message",
    locator = "messages/opaque-17",
    title = "Approved request",
    media_type = "text/plain",
    content = "The rollout must finish this quarter."
  )
  store$upsert_retrieved_resource(resource)

  expect_identical(store$get_retrieved_resource(resource@resource_id), resource)
  expect_length(store$list_retrieved_resources(), 1L)
  expect_true(is.na(store$get_retrieved_source(resource@resource_id)$url))
  expect_equal(
    store$get_retrieved_source(resource@resource_id)$meta$resource_kind,
    "email.message"
  )
  claim <- tempest_claim(
    claim_text = "The rollout must finish this quarter.",
    source_ids = resource@resource_id,
    expert_id = "delivery-expert"
  )
  expect_identical(store$add_proposed_claim(claim), claim@claim_id)
  expect_identical(
    store$proposed_claims_for_resource(resource@resource_id)[[1]]@claim_id,
    claim@claim_id
  )

  snapshot <- tempest:::tempest_research_workspace_snapshot(store)
  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  expect_identical(
    restored$get_retrieved_resource(resource@resource_id)@resource_kind,
    "email.message"
  )
  expect_identical(
    restored$get_proposed_claim(claim@claim_id)@source_ids,
    resource@resource_id
  )
})

test_that("workspace persistence preserves storage-reference-only resources", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/brief-42",
    title = "Externally stored brief",
    media_type = "application/pdf",
    resource_id = "resource.external-brief",
    storage_ref = "object-store://tenant-7/briefs/42",
    origin_connection_id = "knowledge-base",
    scope_metadata = list(tenant_id = "tenant-7", project_id = "project-9"),
    metadata = list(version_id = "v3"),
    retrieved_at = "2026-07-18 UTC"
  )
  store <- test_research_workspace()
  store$upsert_retrieved_resource(resource)

  path <- withr::local_tempfile(fileext = ".json")
  tempest:::tempest_write_json(
    path,
    tempest:::tempest_research_workspace_snapshot(store)
  )
  snapshot <- tempest:::tempest_read_json_strict(path)

  expect_null(snapshot$retrieved_resources[[1]]$content)

  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  restored_resource <- restored$get_retrieved_resource(resource@resource_id)
  restored_source <- restored$get_retrieved_source(resource@resource_id)

  expect_null(restored_resource@content)
  expect_equal(restored_resource@locator, "documents/brief-42")
  expect_equal(
    restored_resource@storage_ref,
    "object-store://tenant-7/briefs/42"
  )
  expect_equal(restored_resource@origin_connection_id, "knowledge-base")
  expect_equal(
    restored_resource@scope_metadata,
    list(tenant_id = "tenant-7", project_id = "project-9")
  )
  expect_equal(restored_resource@metadata, list(version_id = "v3"))
  expect_equal(restored_source$content_text, NA_character_)
  expect_equal(
    restored_source$meta$storage_ref,
    "object-store://tenant-7/briefs/42"
  )
})

test_that("legacy web sources have typed resource views", {
  store <- test_research_workspace()
  source <- tempest:::tempest_source(
    "https://example.org/evidence",
    title = "Evidence",
    content_text = "Evidence body"
  )
  store$upsert_retrieved_resource(source)

  resource <- store$get_retrieved_resource(source$id)
  expect_s7_class(resource, tempest:::TempestResource)
  expect_equal(resource@resource_kind, "web")
  expect_equal(resource@locator, source$url)
  expect_equal(store$get_retrieved_source(source$id)$url, source$url)
})
