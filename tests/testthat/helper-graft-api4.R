test_graft_store <- function(path, create = FALSE) {
  graft::graft_store(path, create = create)
}

test_graft_ref <- function(ref) {
  if (S7::S7_inherits(ref, graft::ArtifactRef)) {
    return(ref)
  }
  graft::ArtifactRef(id = ref$id, revision = ref$revision)
}

test_graft_ref_record <- function(ref) {
  list(id = ref@id, revision = ref@revision)
}

test_graft_id <- function(value) {
  if (is.list(value) && !is.null(value$id)) {
    value$id
  } else {
    value
  }
}

test_graft_save <- function(
  store,
  id,
  bytes,
  media_type,
  dependencies = list()
) {
  dependencies <- lapply(dependencies, test_graft_ref)
  test_graft_ref_record(graft::graft_save(
    store,
    bytes,
    id,
    media_type,
    dependencies = dependencies
  ))
}

test_graft_select <- function(store, roots) {
  roots <- lapply(roots, test_graft_ref)
  selection <- graft::graft_select(store, roots)
  list(
    id = selection@id,
    roots = lapply(selection@roots, test_graft_ref_record),
    artifacts = lapply(selection@artifacts, test_graft_ref_record)
  )
}

test_graft_read_selection <- function(store, selection) {
  selection <- graft::graft_read_selection(store, test_graft_id(selection))
  list(
    id = selection@id,
    roots = lapply(selection@roots, test_graft_ref_record),
    artifacts = lapply(selection@artifacts, test_graft_ref_record)
  )
}

test_graft_read <- function(store, ref) {
  artifact <- graft::graft_read(store, test_graft_ref(ref))
  list(
    ref = test_graft_ref_record(artifact@ref),
    metadata = list(
      format = 1L,
      id = artifact@ref@id,
      payload = digest::digest(
        artifact@bytes,
        algo = "sha256",
        serialize = FALSE
      ),
      size = length(artifact@bytes),
      media_type = artifact@media_type,
      dependencies = lapply(artifact@dependencies, test_graft_ref_record)
    ),
    bytes = artifact@bytes
  )
}

test_graft_decision_record <- function(decision) {
  list(
    id = decision@id,
    sequence = decision@sequence,
    stream = decision@stream,
    key = decision@key,
    previous = decision@previous,
    action = decision@action,
    selection = decision@selection,
    actor = decision@actor,
    reason = decision@reason,
    purpose = decision@purpose
  )
}

test_graft_decide <- function(
  store,
  stream,
  key,
  expected,
  selection,
  action,
  actor,
  reason,
  purpose
) {
  expected_id <- if (is.null(expected)) NULL else test_graft_id(expected)
  selection_id <- test_graft_id(selection)
  decision <- if (identical(action, "accept")) {
    graft::graft_accept(
      store,
      graft::graft_read_selection(store, selection_id),
      stream,
      expected = expected_id,
      key = key,
      actor = actor,
      reason = reason,
      purpose = purpose
    )
  } else {
    graft::graft_withdraw(
      store,
      stream,
      expected = expected_id,
      key = key,
      actor = actor,
      reason = reason
    )
  }
  test_graft_decision_record(decision)
}

test_graft_read_decision <- function(store, stream, decision = NULL) {
  history <- graft::graft_history(store, stream)
  if (is.null(decision)) {
    return(test_graft_decision_record(tail(history, 1L)[[1L]]))
  }
  decision_id <- test_graft_id(decision)
  matches <- Filter(
    function(event) identical(event@id, decision_id),
    history
  )
  stopifnot(length(matches) == 1L)
  test_graft_decision_record(matches[[1L]])
}
