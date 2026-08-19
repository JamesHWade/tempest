test_run_review_lane <- function(items, total = length(items)) {
  list(
    total = as.integer(total),
    retained = as.integer(length(items)),
    omitted = as.integer(max(0L, total - length(items))),
    digest = paste0("sha256:", paste(rep("a", 64L), collapse = "")),
    items = items
  )
}

test_run_review_stage <- function(
  index = 1L,
  stage = "outline",
  status = "succeeded",
  trace_id = paste0("trace-", index)
) {
  list(
    stage = stage,
    attempt_id = paste0("attempt-", index),
    trace_id = trace_id,
    status = status,
    started_at = "2026-08-19T12:00:00Z",
    completed_at = "2026-08-19T12:01:00Z",
    output = list(
      kind = "outline",
      count = 1L,
      digest = paste0("sha256:output-", index)
    ),
    program_artifact_id = paste0("sha256:program-", index),
    governed_procedure_revision_id = NULL,
    failure_class = NULL,
    fallback_policy = "fail_closed",
    fallback_implementation = NULL,
    fallback_taken = FALSE,
    execution_path = "program",
    support_status = "verified",
    publication_allowed = TRUE
  )
}

test_run_review_agent <- function(
  index = 1L,
  trace_id = paste0("trace-", index)
) {
  list(
    trace_id = trace_id,
    trace_type = "deputy_run",
    stage = "outline",
    role = "researcher",
    status = "succeeded",
    completion_disposition = "completed",
    agent_id = paste0("agent-", index),
    expert_id = NULL,
    deputy_run_id = paste0("deputy-run-", index),
    deputy_session_id = paste0("deputy-session-", index),
    parent_agent_id = paste0("parent-agent-", index),
    parent_run_id = paste0("parent-run-", index),
    delegation_id = paste0("delegation-", index),
    tool_call_id = paste0("tool-call-", index),
    program_artifact_id = paste0("sha256:program-", index),
    correlation_id = paste0("correlation-", index)
  )
}

test_run_review_value <- function(
  stages = list(test_run_review_stage()),
  agents = list(
    test_run_review_agent(),
    test_run_review_agent(2L, trace_id = "unlinked-trace")
  ),
  findings = list(list(
    code = "fallback_taken",
    severity = "warning",
    ref_type = "stage_attempt",
    ref_id = "attempt-1"
  ))
) {
  list(
    schema_version = 1L,
    review_id = "sha256:review-safe",
    product = list(
      research_run_id = "run-safe",
      mode = "storm",
      status = "succeeded",
      config_digest = "sha256:config-safe",
      report_reference = list(
        report_id = "report_md",
        sha256 = "sha256:report-safe"
      )
    ),
    stages = test_run_review_lane(stages),
    agent_runs = test_run_review_lane(agents),
    programs = list(),
    knowledge = list(),
    evidence = test_run_review_lane(list()),
    joins = test_run_review_lane(list()),
    findings = test_run_review_lane(findings)
  )
}
