# Host-owned recipe. Read checkpoints only from trusted local storage.
read_decision <- function(store, stream, decision) {
  matches <- Filter(
    function(event) identical(event@id, decision),
    graft::graft_history(store, stream)
  )
  stopifnot(length(matches) == 1L)
  matches[[1L]]
}

capture_briefing_basis <- function(store, stream, decision, purpose) {
  event <- read_decision(store, stream, decision)
  stopifnot(
    identical(event@action, "accept"),
    identical(event@purpose, purpose)
  )
  # Verify the complete scientific evidence and report before retaining its pin.
  tempest::tempest_read_artifact_research(store, event@selection)
  list(
    format = 1L,
    stream = stream,
    decision = event@id,
    selection = event@selection,
    purpose = purpose
  )
}

read_briefing_basis <- function(store, basis) {
  stopifnot(identical(basis$format, 1L))
  event <- read_decision(store, basis$stream, basis$decision)
  stopifnot(
    identical(event@action, "accept"),
    identical(event@selection, basis$selection),
    identical(event@purpose, basis$purpose)
  )
  tempest::tempest_read_artifact_research(store, basis$selection)
}

reuse_briefing_basis <- function(store, basis, eligible) {
  read_briefing_basis(store, basis)
  tempest::tempest_reuse_artifact_research(
    store,
    basis$stream,
    basis$decision,
    basis$purpose,
    eligible = eligible
  )
}

briefing_changes <- function(store, basis) {
  read_briefing_basis(store, basis)
  head <- tail(graft::graft_history(store, basis$stream), 1L)[[1L]]
  list(
    decision_changed = !identical(head@id, basis$decision),
    selection_changed = !identical(head@selection, basis$selection),
    action = head@action
  )
}
