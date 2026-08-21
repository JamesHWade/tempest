test_promotion_program_set <- local({
  value <- NULL
  function() {
    if (is.null(value)) {
      value <<- test_program_set()
    }
    value
  }
})

test_promotion_storm_fixture <- function() {
  run_id <- "research-promotion-1"
  config <- tempest_config(chat_fn = function(...) fake_chat())
  program_set <- test_promotion_program_set()
  completed <- test_persistence_complete_storm_product(
    topic = "Promotion evidence",
    run_id = run_id,
    config = config,
    program_set = program_set
  )
  manifest <- tempest:::tempest_product_authority_finalize_manifest(
    manifest = completed$manifest,
    stage_records = completed$state$stage_records,
    workspace = completed$workspace,
    report_md = completed$state$report_md,
    config = config,
    experts = completed$state$experts,
    product_state = completed$state,
    status = "succeeded",
    require_publishable = TRUE
  )
  retriever <- tempest_retriever(
    config = config,
    workspace = completed$workspace
  )
  product_stage_records <- completed$state$stage_records
  stages <- vapply(
    product_stage_records,
    function(record) record@stage,
    character(1)
  )
  stage_records <- product_stage_records[
    stages %in% c("extract_claims", "verify_claim_support")
  ]
  trace_types <- vapply(
    completed$manifest@traces,
    function(trace) trace$trace_type,
    character(1)
  )
  deputy_traces <- completed$manifest@traces[
    trace_types %in% c("deputy_run", "deputy_delegation")
  ]
  authority_manifest <- tempest_research_manifest(
    research_run_id = run_id,
    mode = "storm",
    config = config,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    status = "succeeded"
  )
  authority_manifest <- tempest:::tempest_product_authority_bind_stage_records(
    authority_manifest,
    stage_records,
    deputy_traces = deputy_traces,
    expert_ids = vapply(
      completed$state$experts,
      function(expert) expert@expert_id,
      character(1)
    )
  )
  authority_manifest <- tempest:::tempest_product_authority_bind_report(
    authority_manifest,
    tempest:::tempest_product_report_for_stage_records(
      completed$state$report_md,
      stage_records,
      prior_records = product_stage_records
    )
  )
  tempest:::tempest_research_workspace_seal(completed$workspace)
  research <- list(
    title = completed$state$title,
    perspectives = completed$state$perspectives,
    experts = completed$state$experts,
    outline = completed$state$outline,
    draft_md = completed$state$draft_md,
    report_md = completed$state$report_md,
    manifest = manifest,
    state = completed$state,
    workspace = completed$workspace,
    retriever = retriever,
    output_dir = NULL
  )
  claim <- completed$workspace$get_proposed_claim(completed$claim_id)
  span <- completed$workspace$get_evidence_span(completed$span_id)
  support <- completed$workspace$list_claim_supports()[[1L]]
  list(
    research = research,
    workspace = completed$workspace,
    manifest = authority_manifest,
    stage_records = stage_records,
    experts = completed$state$experts,
    claim = claim,
    span = span,
    resource = completed$workspace$get_retrieved_resource(
      completed$source@resource_id
    ),
    support = support,
    programs = tempest:::tempest_program_set_manifest_programs(program_set),
    config = config
  )
}

test_promotion_costorm_fixture <- function() {
  config <- tempest_config(chat_fn = function(...) fake_chat())
  session <- tempest_session(
    "CoSTORM promotion evidence",
    config = config,
    experts = list(tempest_expert(
      name = "Promotion Analyst",
      title = "Scientific evidence reviewer",
      description = "Reviews exact evidence for promotion.",
      instructions = "Retain only verified evidence."
    )),
    session_id = "research-promotion-costorm",
    program_set = test_promotion_program_set()
  )
  evidence <- test_persistence_add_costorm_evidence(
    session,
    key = "promotion-costorm",
    claim_text = "Co-STORM evidence is eligible for reviewed promotion."
  )
  report_md <- paste0(
    "# CoSTORM promotion evidence\n\n",
    evidence$claim@claim_text,
    " [",
    evidence$source@resource_id,
    "]."
  )
  report_md <- test_persistence_commit_costorm_report(session, report_md)
  support <- session$workspace$list_claim_supports()[[1L]]
  list(
    research = session,
    workspace = session$workspace,
    manifest = session$manifest,
    stage_records = tempest:::tempest_session_stage_records(session),
    experts = session$get_active_experts(),
    claim = session$workspace$get_proposed_claim(evidence$claim@claim_id),
    span = evidence$span,
    resource = evidence$source,
    support = support,
    programs = tempest:::tempest_program_set_manifest_programs(
      tempest:::tempest_session_program_set(session)
    ),
    config = config,
    report_md = report_md
  )
}

test_promotion_fixture <- local({
  fixtures <- new.env(parent = emptyenv())
  function(mode = c("storm", "costorm")) {
    mode <- match.arg(mode)
    if (!exists(mode, fixtures, inherits = FALSE)) {
      value <- if (identical(mode, "storm")) {
        test_promotion_storm_fixture()
      } else {
        test_promotion_costorm_fixture()
      }
      assign(mode, value, fixtures)
    }
    get(mode, fixtures, inherits = FALSE)
  }
})

test_promotion_bundle <- local({
  fixtures <- new.env(parent = emptyenv())
  function(mode = c("storm", "costorm")) {
    mode <- match.arg(mode)
    if (!exists(mode, fixtures, inherits = FALSE)) {
      fixture <- test_promotion_fixture(mode)
      fixture$bundle <- tempest_promotion_bundle(fixture$research)
      assign(mode, fixture, fixtures)
    }
    get(mode, fixtures, inherits = FALSE)
  }
})

test_promotion_store <- function() {
  graft::graft_open(
    tempest_graft_schema(),
    path = ":memory:",
    okf = "disabled"
  )
}

test_promotion_resign_data <- function(data) {
  payload <- data[setdiff(names(data), "bundle_id")]
  data$bundle_id <- tempest:::tempest_promotion_digest(payload)
  data
}

test_promotion_rewrite_saved <- function(path, data) {
  data <- test_promotion_resign_data(data)
  bundle_path <- file.path(path, "bundle.json")
  tempest:::tempest_promotion_write_json(bundle_path, data)
  manifest <- tempest:::tempest_promotion_manifest_core(
    data$bundle_id,
    tempest:::tempest_promotion_file_checksum(bundle_path)
  )
  manifest$manifest_digest <- tempest:::tempest_promotion_digest(manifest)
  tempest:::tempest_promotion_write_json(
    file.path(path, "manifest.json"),
    manifest
  )
  invisible(data)
}
