test_that("runtime exposes narrow built-in capabilities", {
  runtime <- tempest_runtime()
  capabilities <- runtime$capabilities$list()

  expect_setequal(
    names(capabilities),
    c(
      "tempest.evidence.read",
      "tempest.evidence.write",
      "tempest.expert.delegate",
      "tempest.research.web",
      "tempest.retrieval.semantic"
    )
  )
  expect_true(runtime$operations$has(
    "tempest.capability.evidence.read",
    kind = "capability"
  ))
})

test_that("runtime resolves expert skill requirements before capabilities", {
  operations <- tempest_operation_registry()
  operations$register(
    "skill.compare",
    \(left, right) identical(left, right),
    kind = "skill"
  )
  skill <- tempest_skill(
    "compare",
    purpose = "Compare evidence",
    instructions = "Compare claims and preserve disagreements.",
    required_capability_ids = "tempest.evidence.read",
    operation_ids = "skill.compare"
  )
  expert <- tempest_expert(
    expert_id = "expert.compare",
    name = "Comparison expert",
    title = "Evidence analyst",
    description = "Compares evidence.",
    instructions = "Use the assigned comparison skill.",
    skill_ids = "compare",
    optional_capability_ids = "tempest.retrieval.semantic"
  )
  retriever <- tempest_retriever(
    config = tempest_config(search_provider = "wikipedia"),
    store = SourceStore$new()
  )
  runtime <- tempest_runtime(
    operations = operations,
    skill_specs = list(skill)
  )

  resolution <- runtime$resolve_expert(
    expert,
    context = list(retriever = retriever)
  )

  expect_equal(
    resolution$skills$required_capability_ids,
    "tempest.evidence.read"
  )
  expect_equal(
    resolution$grants[["tempest.evidence.read"]]$status,
    "granted"
  )
  expect_equal(
    resolution$grants[["tempest.retrieval.semantic"]]$status,
    "denied"
  )
  expect_match(resolution$instructions, "Compare claims")
})

test_that("runtime keeps connection bindings out of inspectable records", {
  reference <- tempest_connection_ref(
    "connection.documents",
    provider_id = "host",
    connection_type = "documents",
    title = "Documents",
    description = "Approved documents"
  )
  secret_client <- new.env(parent = emptyenv())
  secret_client$token <- "do-not-persist"
  runtime <- tempest_runtime(
    connection_refs = list(reference),
    connection_bindings = list(
      "connection.documents" = secret_client
    )
  )

  records <- runtime$connections$list()

  expect_equal(names(records), "connection.documents")
  expect_false(grepl(
    "do-not-persist",
    tempest_canonical_json(records),
    fixed = TRUE
  ))
})
