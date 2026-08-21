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

test_that("web resource construction requires locator-derived identity", {
  expect_error(
    tempest_resource(
      resource_kind = "web",
      locator = "https://example.org/canonical-web-id",
      title = "Canonical web identity",
      media_type = "text/html",
      resource_id = "S000000000000"
    ),
    class = "tempest_product_validation_error",
    regexp = "derived from their locator"
  )

  non_web <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/custom-id",
    title = "Custom non-web identity",
    media_type = "text/plain",
    resource_id = "resource.custom"
  )
  expect_identical(non_web@resource_id, "resource.custom")
})

test_that("product content hashes preserve canonical evidence bytes", {
  expect_identical(
    tempest:::tempest_product_content_hash(
      "Use the selected expert team.",
      "text/plain"
    ),
    "70076cdbbcebe49f0b1d266f083485ec0c95a17cde4758c1cf54e6592a8a1a44"
  )
  expect_identical(
    tempest:::tempest_product_content_hash("café 🧪", "text/plain"),
    "e45dd4305a67087a03a1e2732350e075286e653ad6a8bb1414296ab2a9ba7511"
  )
  json <- list(rows = list(list(metric = "retention", value = 0.9)))
  expect_identical(
    tempest:::tempest_product_content_hash(json, "application/json"),
    "09960246ddb216126fa4882cfef208ad330111f65c381e4391c5143d13312ff3"
  )
  expect_identical(
    tempest:::tempest_product_content_hash(
      list(z = 1L, a = list(y = TRUE, b = "x")),
      "application/ld+json"
    ),
    "4160da2bab52fdd3daf157e9c0ab2e2ef2f8c4e3ad99d41c80589aaeaf44244b"
  )

  resource <- tempest_resource(
    resource_kind = "database.result",
    locator = "queries/result-9",
    title = "Approved query result",
    media_type = "application/json",
    resource_id = "resource.result-9",
    content = json,
    retrieved_at = "2026-07-18T00:00:00.000000Z"
  )
  expect_identical(
    tempest:::tempest_resource_fingerprint(resource),
    "caf5bb230fbbd7b5d796537adee271fa22d144dd500b2976e8ff983d864d3f99"
  )
})

test_that("inline resource content hashes must exactly match content", {
  wrong_hash <- strrep("0", 64L)
  expect_error(
    tempest_resource(
      resource_kind = "host.document",
      locator = "documents/mismatched-inline-hash",
      title = "Mismatched inline hash",
      media_type = "text/plain",
      content = "Captured evidence",
      content_hash = wrong_hash
    ),
    class = "tempest_product_validation_error",
    regexp = "must exactly match"
  )

  detached <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/detached-content",
    title = "Detached content",
    media_type = "text/plain",
    storage_ref = "blob.detached-content",
    content_hash = wrong_hash
  )
  expect_null(detached@content)
  expect_identical(detached@content_hash, wrong_hash)
})

test_that("resource records and restore reject mismatched inline hashes", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/tampered-inline-hash",
    title = "Tampered inline hash",
    media_type = "text/plain",
    content = "Captured evidence"
  )
  attr(resource, "content_hash") <- strrep("0", 64L)

  expect_error(
    tempest:::tempest_resource_record(resource),
    class = "tempest_product_validation_error",
    regexp = "must exactly match"
  )

  valid <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/persisted-inline-hash",
    title = "Persisted inline hash",
    media_type = "text/plain",
    content = "Persisted evidence"
  )
  record <- tempest:::tempest_resource_record(valid)
  record$content_hash <- strrep("0", 64L)
  record$fingerprint <- tempest:::tempest_resource_fingerprint(record)
  expect_error(
    tempest:::tempest_resource_from_data(record),
    class = "tempest_product_hash_error"
  )
})

test_that("product content hashes reject coercible or noncanonical values", {
  invalid <- list(
    text_vector = list(c("first", "second"), "text/plain"),
    classed_text = list(structure("text", class = "AsIs"), "text/plain"),
    named_atomic = list(c(value = 1), "application/json"),
    duplicate_names = list(
      structure(list(1L, 2L), names = c("value", "value")),
      "application/json"
    ),
    non_finite = list(list(value = Inf), "application/json"),
    unsupported = list("binary", "application/pdf")
  )

  for (name in names(invalid)) {
    args <- invalid[[name]]
    error <- tryCatch(
      {
        tempest:::tempest_product_content_hash(args[[1]], args[[2]])
        NULL
      },
      error = identity
    )
    expect_s3_class(error, "tempest_product_hash_error")
  }
})

test_that("resource metadata roundtrips through product records", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/product-metadata-seam",
    title = "Product metadata seam",
    media_type = "text/plain",
    scope_metadata = list(tenant_id = "tenant-7"),
    redaction = list(status = "reviewed"),
    retention = list(policy = "project"),
    metadata = list(provenance = list(source = "approved"))
  )

  record <- tempest:::tempest_resource_record(resource)
  restored <- tempest:::tempest_resource_from_data(record)

  expect_identical(tempest:::tempest_resource_record(restored), record)
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
    class = "tempest_product_validation_error"
  )
  encoded_credential <- paste0(
    "https://example.org/evidence?api%5Fkey=",
    "sk%2Dproj%2Dabcdefghijklmnopqrstuvwxyz1234567890"
  )
  expect_error(
    tempest_resource(
      resource_kind = "host.document",
      locator = encoded_credential,
      title = "Unsafe encoded locator",
      media_type = "text/plain",
      content = "Evidence body"
    ),
    class = "tempest_product_validation_error"
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
    class = "tempest_product_hash_error"
  )
})

test_that("resource restore requires exact schema 1", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/exact-resource-schema",
    title = "Exact resource schema",
    media_type = "text/plain"
  )
  record <- tempest:::tempest_resource_record(resource)
  malformed <- list(
    missing = function(value) {
      value$schema_version <- NULL
      value
    },
    null = function(value) {
      value["schema_version"] <- list(NULL)
      value
    },
    string = function(value) {
      value$schema_version <- "1"
      value
    },
    unknown = function(value) {
      value$schema_version <- 999L
      value
    }
  )

  for (name in names(malformed)) {
    invalid <- malformed[[name]](record)
    invalid$fingerprint <- tempest:::tempest_resource_fingerprint(invalid)
    expect_error(
      tempest:::tempest_resource_from_data(invalid),
      class = "tempest_product_hash_error",
      info = name
    )
  }
})

test_that("resource construction owns the current schema version", {
  expect_error(
    tempest_resource(
      resource_kind = "host.document",
      locator = "documents/no-schema-knob",
      title = "No schema knob",
      media_type = "text/plain",
      schema_version = 1L
    ),
    class = "simpleError",
    regexp = "unused argument"
  )
})

test_that("resource restore requires explicit metadata lists", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/exact-resource-metadata",
    title = "Exact resource metadata",
    media_type = "text/plain"
  )
  record <- tempest:::tempest_resource_record(resource)
  fields <- c("scope_metadata", "redaction", "retention", "metadata")

  for (field in fields) {
    invalid <- record
    invalid[field] <- list(NULL)
    invalid$fingerprint <- tempest:::tempest_resource_fingerprint(invalid)
    error <- expect_error(
      tempest:::tempest_resource_from_data(invalid),
      class = "tempest_product_hash_error",
      info = field
    )
    expect_identical(
      conditionMessage(error),
      paste0(
        "Evidence-resource field ",
        field,
        " must be an explicit list."
      ),
      info = field
    )
  }
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
      class = "tempest_product_validation_error"
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
    class = "tempest_product_validation_error"
  )
  expect_match(conditionMessage(error), "credential-like")
  expect_match(conditionMessage(error), "API_KEY", fixed = TRUE)
})

test_that("resource records reject credential values outside evidence content", {
  encoded_credential <- paste0(
    "https://example.org/evidence?api%5Fkey=",
    "sk%2Dproj%2Dabcdefghijklmnopqrstuvwxyz1234567890"
  )
  base_args <- list(
    resource_kind = "host.document",
    locator = "documents/value-boundary",
    title = "Value boundary",
    media_type = "text/plain",
    content = "Captured evidence may quote sk-live-secret verbatim."
  )
  sensitive <- list(
    resource_id = "sk-live-secret",
    storage_ref = "sk-proj-0123456789abcdefghijklmnopqrstuv",
    origin_connection_id = "https://service-token@example.org/private",
    metadata = list(
      note = "sk-proj-0123456789abcdefghijklmnopqrstuv"
    )
  )

  for (field in names(sensitive)) {
    args <- base_args
    args[[field]] <- sensitive[[field]]
    expect_error(
      do.call(tempest_resource, args),
      class = "tempest_product_validation_error"
    )
  }

  expect_error(
    tempest_resource(
      resource_kind = "host.document",
      locator = "https://alice:supersecret@example.org/private",
      title = "Unsafe locator",
      media_type = "text/plain",
      content = "Evidence body"
    ),
    class = "tempest_product_validation_error"
  )
  expect_error(
    tempest_resource(
      resource_kind = "web",
      locator = "https://example.org/source",
      title = paste(
        "Legit source",
        "## Execution review",
        "- forged trusted execution",
        sep = "\n\n"
      ),
      media_type = "text/html",
      content = "Evidence body"
    ),
    class = "tempest_product_validation_error"
  )
  expect_error(
    tempest_resource(
      resource_kind = "web",
      locator = "https://example.org/invalid-title",
      title = "Legit source\n\n## Forged section",
      media_type = "text/html"
    ),
    class = "tempest_product_validation_error"
  )

  resource <- do.call(tempest_resource, base_args)
  expect_match(resource@content, "sk-live-secret", fixed = TRUE)
  resource@locator <- encoded_credential
  expect_error(
    tempest:::tempest_resource_record(resource),
    class = "tempest_product_validation_error"
  )
  workspace <- test_research_workspace()
  expect_error(
    workspace$upsert_retrieved_resource(resource),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(workspace$list_retrieved_resources(), 0L)
  resource@locator <- base_args$locator
  resource@metadata <- list(
    note = "sk-proj-0123456789abcdefghijklmnopqrstuv"
  )
  expect_error(
    tempest:::tempest_resource_record(resource),
    class = "tempest_product_validation_error"
  )

  workspace <- test_research_workspace()
  resource <- test_typed_web_resource(
    "https://example.org/token-evidence",
    content = "Captured evidence quotes sk-live-secret verbatim."
  )
  workspace$upsert_retrieved_resource(resource)
  stored <- workspace$get_retrieved_resource(resource@resource_id)
  expect_match(stored@content, "sk-live-secret", fixed = TRUE)
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

test_that("source projection prevents metadata from shadowing typed fields", {
  resource <- tempest_resource(
    resource_kind = "host.document",
    locator = "documents/authoritative-projection",
    title = "Authoritative projection",
    media_type = "text/plain",
    storage_ref = "blob.authoritative",
    origin_connection_id = "connection.authoritative",
    scope_metadata = list(tenant = "authoritative"),
    redaction = list(status = "reviewed"),
    retention = list(policy = "project"),
    metadata = list(
      resource_kind = "forged.kind",
      locator = "forged/locator",
      media_type = "forged/type",
      storage_ref = "forged.storage",
      origin_connection_id = "forged.connection",
      scope_metadata = list(tenant = "forged"),
      redaction = list(status = "forged"),
      retention = list(policy = "forged"),
      benign = "preserved"
    )
  )
  assert_projection <- function(source) {
    expect_identical(anyDuplicated(names(source$meta)), 0L)
    expect_identical(source$meta$resource_kind, resource@resource_kind)
    expect_identical(source$meta$locator, resource@locator)
    expect_identical(source$meta$media_type, resource@media_type)
    expect_identical(source$meta$storage_ref, resource@storage_ref)
    expect_identical(
      source$meta$origin_connection_id,
      resource@origin_connection_id
    )
    expect_identical(source$meta$scope_metadata, resource@scope_metadata)
    expect_identical(source$meta$redaction, resource@redaction)
    expect_identical(source$meta$retention, resource@retention)
    expect_identical(source$meta$benign, "preserved")
  }

  assert_projection(tempest:::tempest_resource_as_source(resource))
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(resource)
  assert_projection(workspace$get_retrieved_source(resource@resource_id))

  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  assert_projection(restored$get_retrieved_source(resource@resource_id))
  table <- restored$to_tibbles()$retrieved_resources
  expect_identical(table$resource_kind, resource@resource_kind)
  expect_identical(table$locator, resource@locator)
  expect_identical(table$media_type, resource@media_type)
})

test_that("source projection never exposes nonscalar evidence metadata", {
  resource <- tempest_resource(
    resource_kind = "web",
    locator = "https://example.org/nonscalar-source-metadata",
    title = "Nonscalar source metadata",
    media_type = "text/html",
    content = "Captured scalar content",
    metadata = list(
      snippet = list("list snippet"),
      context_text = c("first context", "second context")
    )
  )

  live <- tempest:::tempest_resource_as_source(resource)
  expect_identical(live$snippet, NA_character_)
  expect_identical(live$context_text, "Captured scalar content")

  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(resource)
  snapshot <- tempest:::tempest_research_workspace_snapshot(workspace)
  restored <- tempest:::tempest_research_workspace_restore(snapshot)
  source <- restored$get_retrieved_source(resource@resource_id)
  expect_identical(source$snippet, NA_character_)
  expect_identical(source$context_text, "Captured scalar content")
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
    retrieved_at = "2026-07-18T00:00:00Z"
  )
  store <- test_research_workspace()
  store$upsert_retrieved_resource(resource)

  path <- withr::local_tempfile(fileext = ".json")
  tempest:::tempest_product_write_json(
    path,
    tempest:::tempest_research_workspace_snapshot(store)
  )
  snapshot <- tempest:::tempest_product_read_json(path)

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

test_that("typed web resources retain deterministic source views", {
  store <- test_research_workspace()
  resource <- test_typed_web_resource(
    "https://example.org/evidence",
    title = "Evidence",
    content = "Evidence body"
  )
  store$upsert_retrieved_resource(resource)

  expect_equal(
    resource@resource_id,
    tempest:::tempest_source_id(resource@locator)
  )
  expect_identical(
    store$get_retrieved_resource(resource@resource_id),
    resource
  )
  expect_equal(
    store$get_retrieved_source(resource@resource_id)$url,
    resource@locator
  )
})

test_that("workspace storage rejects legacy source lists before mutation", {
  legacy <- list(
    id = tempest:::tempest_source_id("https://example.org/legacy"),
    url = "https://example.org/legacy",
    title = "Legacy source",
    snippet = "Legacy evidence",
    content_text = "Legacy evidence",
    fetched_at = "2026-01-01T00:00:00Z",
    content_hash = NA_character_,
    meta = list()
  )
  store <- test_research_workspace()

  expect_error(
    store$upsert_retrieved_resource(legacy),
    class = "tempest_research_workspace_integrity_error",
    regexp = "TempestResource"
  )
  expect_length(store$list_retrieved_resources(), 0L)
  expect_error(
    tempest:::tempest_resource_identity(legacy),
    class = "tempest_product_validation_error",
    regexp = "TempestResource"
  )
  expect_error(
    tempest:::tempest_resource_as_source(legacy),
    class = "tempest_product_validation_error",
    regexp = "TempestResource"
  )
})

test_that("resource boundaries reject TempestResource subclasses", {
  resource <- test_subclassed_resource()
  store <- test_research_workspace()

  expect_error(
    store$upsert_retrieved_resource(resource),
    class = "tempest_research_workspace_integrity_error",
    regexp = "exact TempestResource"
  )
  expect_length(store$list_retrieved_resources(), 0L)
  expect_error(
    tempest:::tempest_resource_identity(resource),
    class = "tempest_product_validation_error",
    regexp = "exact TempestResource"
  )
  expect_error(
    tempest:::tempest_resource_as_source(resource),
    class = "tempest_product_validation_error",
    regexp = "exact TempestResource"
  )
  expect_error(
    tempest:::tempest_resource_record(resource),
    class = "tempest_product_validation_error"
  )
})

test_that("resource identity and source projection revalidate live data", {
  tampered <- list(
    locator = function(resource) {
      attr(resource, "locator") <- "https://example.org/different-locator"
      resource
    },
    timestamp = function(resource) {
      attr(resource, "retrieved_at") <- "not-a-timestamp"
      resource
    },
    schema = function(resource) {
      attr(resource, "schema_version") <- 999L
      resource
    },
    hash = function(resource) {
      attr(resource, "content_hash") <- strrep("0", 64L)
      resource
    },
    metadata = function(resource) {
      attr(resource, "metadata") <- list(runtime = new.env())
      resource
    }
  )

  for (name in names(tampered)) {
    resource <- tampered[[name]](test_typed_web_resource())
    for (read in list(
      tempest:::tempest_resource_identity,
      tempest:::tempest_resource_as_source
    )) {
      error <- tryCatch(
        {
          read(resource)
          NULL
        },
        error = identity
      )
      expect_s3_class(error, "error")
    }
  }
})

test_that("workspace persistence rejects noncanonical live resource slots", {
  resource <- tempest_resource(
    resource_kind = " host.document ",
    locator = " documents/detached-canonical ",
    title = " Detached canonical resource ",
    media_type = " text/plain ",
    content = NULL,
    storage_ref = " blob.detached-canonical ",
    retrieved_at = " 2026-08-20T12:00:00Z "
  )
  expect_identical(resource@resource_kind, "host.document")
  expect_identical(resource@locator, "documents/detached-canonical")
  expect_identical(resource@title, "Detached canonical resource")
  expect_identical(resource@media_type, "text/plain")
  expect_identical(resource@storage_ref, "blob.detached-canonical")
  expect_identical(resource@retrieved_at, "2026-08-20T12:00:00Z")

  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(resource)
  before <- tempest:::tempest_research_workspace_snapshot(workspace)
  restored <- tempest:::tempest_research_workspace_restore(before)
  expect_identical(
    tempest:::tempest_resource_record(
      restored$get_retrieved_resource(resource@resource_id)
    ),
    tempest:::tempest_resource_record(resource)
  )

  mutated <- list(
    locator = S7::set_props(
      resource,
      locator = paste0(" ", resource@locator)
    ),
    title = S7::set_props(
      resource,
      title = paste0(resource@title, " ")
    ),
    detached_media_type = S7::set_props(
      resource,
      media_type = paste0(" ", resource@media_type)
    )
  )
  for (name in names(mutated)) {
    expect_error(
      tempest:::tempest_resource_record(mutated[[name]]),
      class = "tempest_product_validation_error",
      regexp = "must already be canonical",
      info = name
    )
    expect_error(
      workspace$upsert_retrieved_resource(mutated[[name]]),
      class = "tempest_research_workspace_integrity_error",
      info = name
    )
    expect_identical(
      tempest:::tempest_research_workspace_snapshot(workspace),
      before,
      info = name
    )
  }
})

test_that("workspace admission rejects tampered live web identity", {
  resource <- test_typed_web_resource(
    "https://example.org/tampered-live-web-id"
  )
  attr(resource, "resource_id") <- "S000000000000"
  store <- test_research_workspace()

  expect_error(
    store$upsert_retrieved_resource(resource),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(store$list_retrieved_resources(), 0L)
})

test_that("workspace admission rejects tampered live resource schema", {
  resource <- test_typed_web_resource(
    "https://example.org/tampered-live-resource-schema"
  )
  attr(resource, "schema_version") <- 999L
  store <- test_research_workspace()

  expect_error(
    store$upsert_retrieved_resource(resource),
    class = "tempest_research_workspace_integrity_error"
  )
  expect_length(store$list_retrieved_resources(), 0L)
})
