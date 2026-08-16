test_research_workspace <- function(...) {
  tempest_research_workspace(...)
}

test_session_artifact_catalog <- function(session) {
  tempest:::tempest_session_artifact_catalog(session)
}

test_session_workflow_run <- function(session) {
  tempest:::tempest_session_workflow_run(session)
}
