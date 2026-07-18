test_that("skill registry resolves exact capabilities and instructions", {
  operations <- tempest_operation_registry()
  operations$register(
    "skill.compare",
    function(left, right) identical(left, right),
    version = "2",
    kind = "skill"
  )
  compare <- tempest_skill(
    "compare",
    purpose = "Compare evidence",
    instructions = "Compare evidence and preserve disagreements.",
    version = "3",
    required_capability_ids = "evidence.read",
    operation_ids = "skill.compare",
    operation_versions = c("skill.compare" = "2")
  )
  summarize <- tempest_skill(
    "summarize",
    purpose = "Summarize evidence",
    instructions = "Summarize only supported claims.",
    required_capability_ids = "evidence.write"
  )
  registry <- tempest_skill_registry(
    list(compare, summarize),
    operations = operations
  )

  expect_error(
    registry$resolve(
      "compare",
      required_capability_ids = "evidence.read",
      optional_capability_ids = "evidence.read"
    ),
    class = "tempest_skill_registry_error"
  )

  resolved <- registry$resolve(
    c("compare", "summarize"),
    versions = c(compare = "3"),
    required_capability_ids = "documents.search",
    optional_capability_ids = c("evidence.read", "browser.open")
  )

  expect_equal(
    resolved$required_capability_ids,
    c("documents.search", "evidence.read", "evidence.write")
  )
  expect_equal(resolved$optional_capability_ids, "browser.open")
  expect_named(resolved$instructions, c("compare", "summarize"))
  expect_match(resolved$prompt, "preserve disagreements")
  expect_match(resolved$prompt, "supported claims")
  expect_equal(resolved$operation_ids, "skill.compare")
  expect_equal(resolved$operation_versions, c("skill.compare" = "2"))
  expect_named(registry$list(), c("compare", "summarize"))
  expect_identical(registry$has("compare", version = "3"), TRUE)
  expect_identical(registry$has("compare", version = "2"), FALSE)

  expert <- tempest_expert(
    expert_id = "expert.policy",
    name = "Policy analyst",
    title = "Policy analyst",
    description = "Compares evidence across policies.",
    instructions = "Use the assigned comparison skill.",
    skill_ids = "compare",
    skill_versions = c(compare = "3"),
    required_capability_ids = "documents.search",
    optional_capability_ids = c("evidence.read", "browser.open")
  )
  expert_resolution <- registry$resolve_for_expert(expert)
  expect_equal(expert_resolution$expert_id, "expert.policy")
  expect_equal(
    expert_resolution$required_capability_ids,
    c("documents.search", "evidence.read")
  )
  expect_equal(
    expert_resolution$optional_capability_ids,
    "browser.open"
  )
})

test_that("skill registry verifies runtime operation kind and version", {
  operations <- tempest_operation_registry()
  operations$register(
    "skill.compare",
    identity,
    version = "1",
    kind = "step"
  )
  invalid_kind <- tempest_skill(
    "compare",
    purpose = "Compare evidence",
    instructions = "Compare the evidence.",
    operation_ids = "skill.compare",
    operation_versions = c("skill.compare" = "1")
  )

  expect_error(
    tempest_skill_registry(list(invalid_kind), operations),
    class = "tempest_skill_registry_error"
  )

  operations$register(
    "skill.compare",
    identity,
    version = "1",
    kind = "skill",
    replace = TRUE
  )
  invalid_version <- tempest_skill(
    "compare",
    purpose = "Compare evidence",
    instructions = "Compare the evidence.",
    operation_ids = "skill.compare",
    operation_versions = c("skill.compare" = "2")
  )

  expect_error(
    tempest_skill_registry(list(invalid_version), operations),
    class = "tempest_skill_registry_error"
  )
})

test_that("connection providers enforce explicit opaque grants", {
  calls <- 0L
  reference <- tempest_connection_ref(
    "approved-documents",
    provider_id = "test.host",
    connection_type = "document-search",
    title = "Approved documents",
    description = "A test-only document index"
  )
  provider <- tempest_connection_provider(
    list(reference),
    bindings = list(
      "approved-documents" = function(connection_ref, context) {
        calls <<- calls + 1L
        list(
          connection_id = connection_ref@connection_id,
          tenant = context$tenant
        )
      }
    )
  )

  expect_error(
    provider$resolve(
      "approved-documents",
      allowed_ref_ids = character(),
      context = list(tenant = "acme")
    ),
    class = "tempest_connection_provider_error"
  )
  expect_equal(calls, 0L)

  provider$preflight(
    "approved-documents",
    allowed_ref_ids = "approved-documents"
  )
  expect_equal(calls, 0L)

  client <- provider$resolve(
    "approved-documents",
    allowed_ref_ids = "approved-documents",
    context = list(tenant = "acme")
  )
  expect_equal(client[["approved-documents"]]$tenant, "acme")
  expect_equal(calls, 1L)
  expect_named(provider$list(), "approved-documents")
  expect_identical(
    grepl(
      "tenant",
      tempest:::tempest_canonical_json(
        provider$list()
      )
    ),
    FALSE
  )
})

test_that("capability resolver preflights all requests before factories", {
  calls <- list(granted = 0L, unused = 0L)
  granted <- tempest_capability_spec(
    "evidence.read",
    purpose = "Read evidence",
    instructions = "Read only.",
    operation_id = "capability.evidence.read"
  )
  unused <- tempest_capability_spec(
    "evidence.write",
    purpose = "Write evidence",
    instructions = "Write only.",
    operation_id = "capability.evidence.write"
  )
  resolver <- tempest_capability_resolver(
    list(granted, unused),
    implementations = list(
      "evidence.read" = function(capability_spec, connections, context) {
        calls$granted <<- calls$granted + 1L
        list(tools = list("read"))
      },
      "evidence.write" = function(capability_spec, connections, context) {
        calls$unused <<- calls$unused + 1L
        list(tools = list("write"))
      }
    )
  )

  expect_error(
    resolver$resolve(
      required_capability_ids = c("evidence.read", "missing")
    ),
    class = "tempest_capability_resolution_error"
  )
  expect_equal(calls, list(granted = 0L, unused = 0L))

  resolution <- resolver$resolve(
    required_capability_ids = "evidence.read",
    optional_capability_ids = "missing"
  )
  expect_equal(resolution$tools, list("read"))
  expect_equal(resolution$grants[["evidence.read"]]$status, "granted")
  expect_equal(resolution$grants$missing$status, "denied")
  expect_equal(
    resolution$grants$missing$reason_code,
    "specification_missing"
  )
  expect_equal(calls, list(granted = 1L, unused = 0L))
  expect_silent(tempest:::tempest_canonical_json(resolution$grants))
})

test_that("capabilities receive only allowed runtime connections", {
  connection_calls <- 0L
  capability_calls <- 0L
  reference <- tempest_connection_ref(
    "customer-documents",
    provider_id = "test.host",
    connection_type = "search",
    title = "Customer documents",
    description = "Test customer documents"
  )
  provider <- tempest_connection_provider(
    list(reference),
    bindings = list(
      "customer-documents" = function(connection_ref, context) {
        connection_calls <<- connection_calls + 1L
        list(tenant = context$tenant)
      }
    )
  )
  specification <- tempest_capability_spec(
    "documents.search",
    purpose = "Search documents",
    instructions = "Search only the approved connection.",
    operation_id = "capability.documents.search",
    connection_ref_ids = "customer-documents",
    model_roles = "expert"
  )
  resolver <- tempest_capability_resolver(
    list(specification),
    implementations = list(
      "documents.search" = function(
        capability_spec,
        connections,
        context
      ) {
        capability_calls <<- capability_calls + 1L
        expect_equal(capability_spec@capability_id, "documents.search")
        expect_named(connections, "customer-documents")
        expect_equal(connections[[1]]$tenant, context$tenant)
        list(
          tools = list("document-search"),
          metadata = list(provider = "test")
        )
      }
    ),
    connection_provider = provider
  )

  expect_error(
    resolver$resolve(
      required_capability_ids = "documents.search",
      model_role = "expert",
      context = list(tenant = "acme")
    ),
    class = "tempest_capability_resolution_error"
  )
  expect_equal(connection_calls, 0L)
  expect_equal(capability_calls, 0L)

  denied <- resolver$resolve(
    optional_capability_ids = "documents.search",
    model_role = "moderator",
    allowed_connection_ref_ids = "customer-documents",
    context = list(tenant = "acme")
  )
  expect_equal(denied$grants[["documents.search"]]$status, "denied")
  expect_equal(
    denied$grants[["documents.search"]]$reason_code,
    "model_role_denied"
  )
  expect_equal(connection_calls, 0L)
  expect_equal(capability_calls, 0L)

  granted <- resolver$resolve(
    required_capability_ids = "documents.search",
    model_role = "expert",
    allowed_connection_ref_ids = "customer-documents",
    context = list(tenant = "acme")
  )
  expect_equal(granted$tools, list("document-search"))
  expect_equal(granted$grants[["documents.search"]]$status, "granted")
  expect_equal(granted$grants[["documents.search"]]$metadata$provider, "test")
  expect_equal(connection_calls, 1L)
  expect_equal(capability_calls, 1L)
})

test_that("connection factories cannot resolve to NULL", {
  reference <- tempest_connection_ref(
    "documents",
    provider_id = "test.host",
    connection_type = "search",
    title = "Documents",
    description = "Approved documents"
  )
  provider <- tempest_connection_provider(
    list(reference),
    bindings = list(documents = \(...) NULL)
  )

  expect_error(
    provider$resolve("documents", allowed_ref_ids = "documents"),
    class = "tempest_connection_provider_error"
  )
})

test_that("factory and authorization failures follow required policy", {
  specification <- tempest_capability_spec(
    "browser.open",
    purpose = "Open approved pages",
    instructions = "Open only approved pages.",
    operation_id = "capability.browser.open"
  )
  failing <- tempest_capability_resolver(
    list(specification),
    implementations = list(
      "browser.open" = function(capability_spec, connections, context) {
        stop("factory failed")
      }
    )
  )

  expect_error(
    failing$resolve(required_capability_ids = "browser.open"),
    class = "tempest_capability_resolution_error"
  )
  optional <- failing$resolve(optional_capability_ids = "browser.open")
  expect_equal(optional$grants[["browser.open"]]$status, "denied")
  expect_equal(
    optional$grants[["browser.open"]]$reason_code,
    "factory_failed"
  )
  expect_error(
    failing$resolve(
      required_capability_ids = "browser.open",
      optional_capability_ids = "browser.open"
    ),
    class = "tempest_capability_resolution_error"
  )

  denied <- tempest_capability_resolver(
    list(specification),
    implementations = list(
      "browser.open" = list(
        factory = function(capability_spec, connections, context) {
          list(tools = list("browser"))
        },
        authorize = function(capability_spec, context) {
          list(granted = FALSE, reason = "Host policy denied access.")
        }
      )
    )
  )
  resolution <- denied$resolve(optional_capability_ids = "browser.open")
  expect_equal(
    resolution$grants[["browser.open"]]$reason_code,
    "authorization_denied"
  )
})

test_that("capability grant metadata excludes secrets and runtime values", {
  expect_error(
    tempest:::tempest_capability_grant_record(
      capability_id = "documents.search",
      required = TRUE,
      status = "granted",
      metadata = list(api_key = "must-not-persist")
    ),
    class = "tempest_capability_resolution_error"
  )
  expect_error(
    tempest:::tempest_capability_grant_record(
      capability_id = "documents.search",
      required = TRUE,
      status = "granted",
      metadata = list(client = \() NULL)
    ),
    class = "tempest_capability_resolution_error"
  )
})

test_that("resolved tools and registrars attach to a chat", {
  specification <- tempest_capability_spec(
    "local.search",
    purpose = "Search a local index",
    instructions = "Search locally.",
    operation_id = "capability.local.search"
  )
  registered_tools <- NULL
  registrar_context <- NULL
  chat <- list(
    register_tools = function(tools) {
      registered_tools <<- tools
      invisible(NULL)
    }
  )
  resolver <- tempest_capability_resolver(
    list(specification),
    implementations = list(
      "local.search" = function(capability_spec, connections, context) {
        list(
          tools = list("local-tool"),
          registrars = list(function(chat, context) {
            registrar_context <<- context
          })
        )
      }
    )
  )
  resolution <- resolver$resolve(
    required_capability_ids = "local.search"
  )

  returned <- tempest:::tempest_register_capabilities(
    chat,
    resolution,
    context = list(run_id = "run-1")
  )

  expect_identical(returned, chat)
  expect_equal(registered_tools, list("local-tool"))
  expect_equal(registrar_context$run_id, "run-1")
})
