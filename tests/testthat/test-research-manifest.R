test_that("research manifests validate schema and lifecycle enums", {
  manifest <- tempest_research_manifest(
    research_run_id = "research-123",
    mode = "storm",
    config = tempest_config()
  )

  expect_identical(S7::S7_inherits(manifest, TempestResearchManifest), TRUE)
  expect_identical(manifest@schema_version, 3L)
  expect_identical(manifest@research_run_id, "research-123")
  expect_identical(manifest@mode, "storm")
  expect_identical(manifest@status, "running")
  expect_match(manifest@config_digest, "^sha256:[a-f0-9]{64}$")

  expect_error(
    tempest_research_manifest("research-123", "other", tempest_config()),
    class = "tempest_research_manifest_error",
    regexp = "mode.*storm.*costorm"
  )
  expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      status = "pending"
    ),
    class = "tempest_research_manifest_error",
    regexp = "status.*running"
  )
  expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      schema_version = 1L
    ),
    class = "tempest_research_manifest_error",
    regexp = "schema_version.*version.*3"
  )
})

test_that("research manifest records survive canonical JSON without drift", {
  program_id <- paste0("sha256:", strrep("a", 64L))
  manifest <- tempest_research_manifest(
    research_run_id = "research-123",
    mode = "costorm",
    config = tempest_config(),
    programs = list(
      extract_claims = list(
        stage = "extract_claims",
        contract_version = 1L,
        program_artifact_id = program_id,
        artifact_reference = list(
          type = "builtin",
          id = "tempest::extract_claims"
        ),
        governed_procedure_ref = NULL,
        evaluator_id = "dsprrr",
        evaluator_version = "1.0.0"
      )
    ),
    knowledge_snapshot = list(
      snapshot_id = "snapshot:opaque",
      store_id = "store:opaque",
      commit_order = 117L
    ),
    runtime = list(
      deputy_session_ids = c("session-a", "session-b"),
      deputy_run_ids = "run-a"
    ),
    traces = list(list(trace_id = "trace-a", stage = "extract_claims")),
    deliverables = list(report = list(deliverable_id = "report-a"))
  )
  record <- tempest_research_manifest_record(manifest)
  json <- tempest_research_manifest_canonical_json(record)
  decoded <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  restored <- tempest_research_manifest_from_record(decoded)
  restored_record <- tempest_research_manifest_record(restored)
  expect_error(
    tempest_research_manifest_from_record(record[rev(names(record))]),
    class = "tempest_research_manifest_error"
  )

  expect_named(
    record,
    c(
      "schema_version",
      "research_run_id",
      "mode",
      "config_digest",
      "programs",
      "knowledge_snapshot",
      "runtime",
      "traces",
      "deliverables",
      "status"
    )
  )
  expect_identical(restored_record, record)
  expect_identical(
    tempest_research_manifest_canonical_json(restored_record),
    json
  )
})

test_that("research manifest readers require exact current records", {
  program_id <- paste0("sha256:", strrep("a", 64L))
  record <- tempest_research_manifest_record(tempest_research_manifest(
    research_run_id = "research-current-record",
    mode = "storm",
    config = tempest_config(),
    programs = list(
      extract_claims = list(
        stage = "extract_claims",
        contract_version = 1L,
        program_artifact_id = program_id,
        artifact_reference = list(
          type = "builtin",
          id = "tempest::extract_claims"
        ),
        governed_procedure_ref = NULL,
        evaluator_id = "dsprrr",
        evaluator_version = "1.0.0"
      )
    ),
    runtime = list(deputy_run_ids = "run-a")
  ))

  whole_double_schema <- record
  whole_double_schema$schema_version <- 3
  expect_error(
    tempest_research_manifest_from_record(whole_double_schema),
    class = "tempest_research_manifest_error"
  )
  prior_schema <- record
  prior_schema$schema_version <- 2L
  expect_error(
    tempest_research_manifest_from_record(prior_schema),
    class = "tempest_research_manifest_error",
    regexp = "supported version.*3"
  )

  for (field in c(
    "programs",
    "knowledge_snapshot",
    "runtime",
    "traces",
    "deliverables"
  )) {
    null_field <- record
    null_field[field] <- list(NULL)
    expect_error(
      tempest_research_manifest_from_record(null_field),
      class = "tempest_research_manifest_error"
    )
  }

  scalar_runtime_id <- record
  scalar_runtime_id$runtime$deputy_run_ids <- "run-a"
  expect_error(
    tempest_research_manifest_from_record(scalar_runtime_id),
    class = "tempest_research_manifest_error"
  )

  whole_double_contract <- record
  whole_double_contract$programs$extract_claims$contract_version <- 1
  expect_error(
    tempest_research_manifest_from_record(whole_double_contract),
    class = "tempest_research_manifest_error"
  )
})

test_that("Deputy terminal traces require exact completion disposition", {
  trace <- list(
    agent_id = "agent.disposition",
    correlation_id = "correlation.disposition",
    deputy_run_id = "run.disposition",
    deputy_session_id = "session.disposition",
    role = "expert",
    stage = "research",
    status = "complete",
    trace_id = "run.disposition",
    trace_type = "deputy_run"
  )
  expect_error(
    tempest:::tempest_research_manifest_traces(list(trace)),
    class = "tempest_research_manifest_error"
  )

  for (disposition in c("issued", "discarded")) {
    candidate <- trace
    candidate$completion_disposition <- disposition
    expect_no_error(tempest:::tempest_research_manifest_traces(
      list(candidate)
    ))
  }
  delegation <- trace
  delegation$trace_type <- "deputy_delegation"
  expect_error(
    tempest:::tempest_research_manifest_traces(list(delegation)),
    class = "tempest_research_manifest_error"
  )
  delegation$completion_disposition <- "issued"
  expect_no_error(tempest:::tempest_research_manifest_traces(
    list(delegation)
  ))

  nested <- list(group = list(trace))
  expect_error(
    tempest:::tempest_research_manifest_traces(nested),
    class = "tempest_research_manifest_error"
  )
  nested$group[[1]]$completion_disposition <- "issued"
  expect_no_error(tempest:::tempest_research_manifest_traces(nested))

  terminal <- trace
  terminal$status <- "error"
  terminal$completion_disposition <- "terminal"
  expect_no_error(tempest:::tempest_research_manifest_traces(list(terminal)))

  incoherent <- trace
  incoherent$completion_disposition <- "terminal"
  expect_error(
    tempest:::tempest_research_manifest_traces(list(incoherent)),
    class = "tempest_research_manifest_error"
  )
  stage_trace <- list(
    completion_disposition = "issued",
    status = "succeeded",
    trace_id = "attempt.disposition",
    trace_type = "stage_attempt"
  )
  expect_error(
    tempest:::tempest_research_manifest_traces(list(stage_trace)),
    class = "tempest_research_manifest_error"
  )
})

test_that("research manifests preserve complete portable ProgramSet references", {
  program_id <- paste0("sha256:", strrep("c", 64L))
  builtin <- list(
    stage = "extract_claims",
    contract_version = 1L,
    program_artifact_id = program_id,
    artifact_reference = list(
      type = "builtin",
      id = "tempest::extract_claims"
    ),
    governed_procedure_ref = tempest:::tempest_governed_procedure_record(
      test_governed_procedure_ref(
        "extract_claims",
        program_id,
        evaluator_id = "support-evaluator",
        evaluator_version = "2.1.0"
      )
    ),
    evaluator_id = "support-evaluator",
    evaluator_version = "2.1.0"
  )
  file <- builtin
  file$artifact_reference <- list(
    type = "file",
    path = "programs/extract_claims.rds"
  )

  builtin_manifest <- tempest_research_manifest(
    "research-builtin-program",
    config = tempest_config(),
    programs = list(extract_claims = builtin)
  )
  file_manifest <- tempest_research_manifest(
    "research-file-program",
    config = tempest_config(),
    programs = list(extract_claims = file)
  )

  expect_identical(builtin_manifest@programs$extract_claims, builtin)
  expect_identical(file_manifest@programs$extract_claims, file)
  expect_identical(
    tempest:::tempest_research_manifest_programs_same_identity(
      builtin_manifest@programs,
      file_manifest@programs
    ),
    TRUE
  )
  changed_evaluator <- file_manifest@programs
  changed_evaluator$extract_claims$evaluator_version <- "2.2.0"
  expect_error(
    tempest:::tempest_research_manifest_programs_same_identity(
      builtin_manifest@programs,
      changed_evaluator
    ),
    class = "tempest_research_manifest_error",
    regexp = "must match its exact ProgramSet"
  )
})

test_that("research manifests reject corrupt ProgramSet references", {
  reference <- list(
    stage = "extract_claims",
    contract_version = 1L,
    program_artifact_id = paste0("sha256:", strrep("d", 64L)),
    artifact_reference = list(
      type = "file",
      path = "programs/extract_claims.rds"
    ),
    governed_procedure_ref = NULL,
    evaluator_id = "dsprrr",
    evaluator_version = "1.0.0"
  )
  corrupt <- list(
    missing_field = reference[names(reference) != "evaluator_version"],
    wrong_stage = utils::modifyList(reference, list(stage = "personas")),
    wrong_contract = utils::modifyList(reference, list(contract_version = 2L)),
    wrong_id = utils::modifyList(
      reference,
      list(program_artifact_id = "program")
    ),
    escaping_path = utils::modifyList(
      reference,
      list(artifact_reference = list(type = "file", path = "../program.rds"))
    ),
    wrong_builtin = utils::modifyList(
      reference,
      list(
        artifact_reference = list(type = "builtin", id = "tempest::personas")
      )
    )
  )

  for (value in corrupt) {
    expect_error(
      tempest_research_manifest(
        "research-corrupt-program",
        config = tempest_config(),
        programs = list(extract_claims = value)
      ),
      class = "tempest_research_manifest_error"
    )
  }
})

test_that("research manifest references reject sensitive nested fields", {
  api_error <- expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      programs = list(
        extract_claims = list(metadata = list(apiKey = "secret"))
      )
    ),
    class = "tempest_research_manifest_error"
  )
  expect_match(conditionMessage(api_error), "credential-like")
  expect_match(conditionMessage(api_error), "apiKey")

  token_error <- expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      traces = list(list(auth = list(refresh_token = "secret")))
    ),
    class = "tempest_research_manifest_error"
  )
  expect_match(conditionMessage(token_error), "credential-like")
  expect_match(conditionMessage(token_error), "auth")
})

test_that("manifest identifiers share the portable credential boundary", {
  biomedical <- tempest_research_manifest(
    "SK-BR-3",
    "storm",
    tempest_config(),
    knowledge_snapshot = list(
      snapshot_id = "SK-N-SH",
      store_id = "SK-MEL-28",
      commit_order = 1L
    ),
    traces = list(list(trace_id = "SK-OV-3"))
  )
  expect_identical(biomedical@research_run_id, "SK-BR-3")
  expect_identical(
    biomedical@knowledge_snapshot$snapshot_id,
    "SK-N-SH"
  )
  expect_identical(biomedical@traces[[1]]$trace_id, "SK-OV-3")

  credential <- "https://service-token@example.org/private"
  expect_error(
    tempest_research_manifest(credential, "storm", tempest_config()),
    class = "tempest_research_manifest_error"
  )
  expect_error(
    tempest_research_manifest(
      "research-safe",
      "storm",
      tempest_config(),
      knowledge_snapshot = list(
        snapshot_id = credential,
        store_id = "store-safe",
        commit_order = 1L
      )
    ),
    class = "tempest_research_manifest_error"
  )
  expect_error(
    tempest_research_manifest(
      "research-safe",
      "storm",
      tempest_config(),
      traces = list(list(trace_id = credential))
    ),
    class = "tempest_research_manifest_error"
  )
})

test_that("research manifest references reject runtime objects", {
  RuntimeObject <- R6::R6Class("ManifestRuntimeObject")
  connection <- file(withr::local_tempfile())
  withr::defer(close(connection))
  forbidden <- list(
    function() NULL,
    new.env(parent = emptyenv()),
    connection,
    methods::new("externalptr"),
    RuntimeObject$new(),
    tempest_config()
  )

  for (value in forbidden) {
    expect_error(
      tempest_research_manifest(
        "research-123",
        "storm",
        tempest_config(),
        programs = list(extract_claims = list(runtime = value))
      ),
      class = "tempest_research_manifest_error",
      regexp = "cannot contain|plain JSON-compatible"
    )
  }
})

test_that("research manifest reference schemas are closed", {
  invalid <- list(
    list(
      programs = list(
        extract_claims = list(program_artifact_id = "program-1", label = "x")
      )
    ),
    list(
      knowledge_snapshot = list(
        snapshot_id = "snapshot-1",
        label = "not-part-of-the-snapshot-contract"
      )
    ),
    list(runtime = list(parent_run_id = "run-parent")),
    list(traces = list(list(trace_id = "trace-1", content = "secret"))),
    list(
      deliverables = list(list(
        deliverable_id = "report-1",
        headers = "secret"
      ))
    )
  )

  for (fields in invalid) {
    expect_error(
      do.call(
        tempest_research_manifest,
        c(
          list(
            research_run_id = "research-123",
            mode = "storm",
            config = tempest_config()
          ),
          fields
        )
      ),
      class = "tempest_research_manifest_error",
      regexp = "Unknown reference fields|credential-like"
    )
  }
})

test_that("research manifest references enforce canonical plain values", {
  expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      traces = structure(list("trace-a", "trace-b"), names = c("", "trace"))
    ),
    class = "tempest_research_manifest_error",
    regexp = "fully named or fully unnamed"
  )
  expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      traces = list(list(trace_id = NA_character_))
    ),
    class = "tempest_research_manifest_error",
    regexp = "missing or non-finite"
  )
  expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      programs = list(
        extract_claims = list(
          stage = "extract_claims",
          contract_version = 1L,
          program_artifact_id = "",
          artifact_reference = list(
            type = "builtin",
            id = "tempest::extract_claims"
          ),
          governed_procedure_ref = NULL,
          evaluator_id = "dsprrr",
          evaluator_version = "1.0.0"
        )
      )
    ),
    class = "tempest_research_manifest_error",
    regexp = "sha256"
  )
  expect_error(
    tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      traces = list(group = list())
    ),
    class = "tempest_research_manifest_error",
    regexp = "non-empty reference record"
  )
})

test_that("research manifest references normalize JSON arrays consistently", {
  manifest <- tempest_research_manifest(
    "research-123",
    "storm",
    tempest_config(),
    runtime = list(
      deputy_session_ids = c("session-b", "session-a", "session-a"),
      deputy_run_ids = character()
    ),
    traces = list(list(stage = "extract_claims", trace_id = "trace-a"))
  )

  expect_identical(
    manifest@runtime$deputy_session_ids,
    list("session-a", "session-b")
  )
  expect_identical(manifest@runtime$deputy_run_ids, list())
  expect_null(names(manifest@traces))
  expect_named(manifest@traces[[1]], c("stage", "trace_id"))

  record <- tempest_research_manifest_record(manifest)
  decoded <- jsonlite::fromJSON(
    tempest_research_manifest_canonical_json(record),
    simplifyVector = FALSE
  )
  expect_identical(
    tempest_research_manifest_record(
      tempest_research_manifest_from_record(decoded)
    ),
    record
  )
})

test_that("research manifests do not retain caller reference aliases", {
  traces <- list(
    list(trace_id = "trace-1", stage = "original")
  )
  manifest <- tempest_research_manifest(
    "research-123",
    "storm",
    tempest_config(),
    traces = traces
  )

  traces[[1]]$stage <- "changed-input"
  returned <- manifest@traces
  returned[[1]]$stage <- "changed-output"

  expect_identical(manifest@traces[[1]]$stage, "original")
})

test_that("configuration digests cover behavior and exclude runtime details", {
  first_cache <- withr::local_tempdir()
  second_cache <- withr::local_tempdir()
  first <- tempest_config(
    params = list(
      temperature = 0.2,
      max_tokens = 500L,
      token_budget = 2e4,
      token_count = 12L,
      nested = list(
        top_p = 0.9,
        api_key = "first-secret",
        authorization = "Bearer first",
        cookie = "session=first",
        signing_key = "first-signing-key",
        ssh_private_key = "first-ssh-key",
        provider_token = "first-token"
      )
    ),
    cache_dir = first_cache
  )
  reordered <- tempest_config(
    params = list(
      nested = list(
        provider_token = "second-token",
        ssh_private_key = "second-ssh-key",
        signing_key = "second-signing-key",
        cookie = "session=second",
        authorization = "Bearer second",
        api_key = "second-secret",
        top_p = 0.9
      ),
      token_count = 12L,
      token_budget = 2e4,
      max_tokens = 500L,
      temperature = 0.2
    ),
    cache_dir = second_cache,
    chat_fn = function(...) NULL
  )
  changed <- tempest_config(
    params = list(
      temperature = 0.2,
      nested = list(top_p = 0.9)
    ),
    max_sources = 25
  )
  changed_tokens <- tempest_config(
    params = list(
      temperature = 0.2,
      max_tokens = 501L,
      token_budget = 2e4,
      token_count = 12L,
      nested = list(top_p = 0.9)
    )
  )
  changed_user_agent <- tempest_config(
    params = list(
      temperature = 0.2,
      max_tokens = 500L,
      token_budget = 2e4,
      token_count = 12L,
      nested = list(top_p = 0.9)
    ),
    user_agent = "tempest-contract-test"
  )

  first_projection <- tempest_research_config_projection(first)
  expect_null(first_projection$chat)
  expect_null(first_projection$chat_fn)
  expect_null(first_projection$ragnar_store)
  expect_null(first_projection$artifact_store)
  expect_null(first_projection$cache_dir)
  expect_null(first_projection$params$nested$api_key)
  expect_null(first_projection$params$nested$authorization)
  expect_null(first_projection$params$nested$cookie)
  expect_null(first_projection$params$nested$signing_key)
  expect_null(first_projection$params$nested$ssh_private_key)
  expect_null(first_projection$params$nested$provider_token)
  expect_identical(first_projection$params$max_tokens, 500L)
  expect_identical(first_projection$params$token_budget, 20000L)
  expect_identical(first_projection$params$token_count, 12L)
  expect_identical(first_projection$cache_ttl, "unbounded")
  expect_identical(
    tempest_research_config_digest(first),
    tempest_research_config_digest(reordered)
  )
  expect_identical(
    identical(
      tempest_research_config_digest(first),
      tempest_research_config_digest(changed)
    ),
    FALSE
  )
  expect_identical(
    identical(
      tempest_research_config_digest(first),
      tempest_research_config_digest(changed_tokens)
    ),
    FALSE
  )
  expect_identical(
    identical(
      tempest_research_config_digest(first),
      tempest_research_config_digest(changed_user_agent)
    ),
    FALSE
  )
})

test_that("research manifest records contain references only", {
  program_id <- paste0("sha256:", strrep("b", 64L))
  manifest <- tempest_research_manifest(
    "research-123",
    "storm",
    tempest_config(),
    programs = list(
      extract_claims = list(
        stage = "extract_claims",
        contract_version = 1L,
        program_artifact_id = program_id,
        artifact_reference = list(
          type = "builtin",
          id = "tempest::extract_claims"
        ),
        governed_procedure_ref = NULL,
        evaluator_id = "dsprrr",
        evaluator_version = "1.0.0"
      )
    ),
    knowledge_snapshot = list(snapshot_id = "snapshot-1"),
    runtime = list(deputy_run_ids = "deputy-run-1"),
    traces = list(list(trace_id = "trace-1")),
    deliverables = list(list(deliverable_id = "report-1"))
  )
  record <- tempest_research_manifest_record(manifest)

  expect_type(record, "list")
  expect_no_error(tempest_research_manifest_canonical_json(record))
  expect_error(
    tempest_research_manifest_from_record(
      c(record, list(chat = list(messages = list())))
    ),
    class = "tempest_research_manifest_error",
    regexp = "exactly.*manifest fields"
  )
})

test_that("research manifest updates return a new validated value", {
  manifest <- tempest_research_manifest(
    "research-123",
    "storm",
    tempest_config()
  )
  updated <- tempest_research_manifest_update(
    manifest,
    status = "succeeded",
    runtime = list(deputy_run_ids = "deputy-run-1"),
    traces = list(list(trace_id = "trace-1")),
    deliverables = list(list(deliverable_id = "report-1"))
  )

  expect_identical(manifest@status, "running")
  expect_length(manifest@traces, 0L)
  expect_identical(updated@status, "succeeded")
  expect_identical(updated@runtime$deputy_run_ids, list("deputy-run-1"))
  expect_identical(updated@traces[[1]]$trace_id, "trace-1")
  expect_identical(updated@deliverables[[1]]$deliverable_id, "report-1")
  expect_identical(updated@programs, manifest@programs)
  expect_identical(updated@knowledge_snapshot, manifest@knowledge_snapshot)
})

test_that("terminal research manifest statuses are absorbing and idempotent", {
  for (terminal_status in c("succeeded", "failed", "cancelled")) {
    terminal <- tempest_research_manifest(
      "research-123",
      "storm",
      tempest_config(),
      status = terminal_status
    )
    same <- tempest_research_manifest_update(
      terminal,
      status = terminal_status
    )

    expect_identical(same@status, terminal_status)
    expect_error(
      tempest_research_manifest_update(terminal, status = "running"),
      class = "tempest_research_manifest_error",
      regexp = "terminal.*cannot transition"
    )
  }
})

test_that("execution path and support status are independent dimensions", {
  combinations <- expand.grid(
    execution_path = tempest_execution_paths(),
    support_status = tempest_support_statuses(),
    stringsAsFactors = FALSE
  )
  records <- lapply(
    seq_len(nrow(combinations)),
    function(index) {
      tempest_research_provenance_record(
        combinations$execution_path[[index]],
        combinations$support_status[[index]]
      )
    }
  )

  expect_length(records, 15L)
  expect_identical(
    tempest_research_provenance_record("governed", "unsupported"),
    list(execution_path = "governed", support_status = "unsupported")
  )
  expect_error(
    tempest_research_provenance_record("dsprrr", "verified"),
    class = "tempest_research_manifest_error",
    regexp = "execution_path"
  )
  expect_error(
    tempest_research_provenance_record("grounded", "supported"),
    class = "tempest_research_manifest_error",
    regexp = "support_status"
  )
})
