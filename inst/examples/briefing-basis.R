# Host-owned recipe. Load checkpoints only from trusted local storage.
briefing_selection <- function(receipt) {
  Filter(
    function(record) {
      record$class %in% c("Claim", "ClaimSupport", "EvidenceSpan", "Source")
    },
    receipt@record_revisions
  )
}

briefing_revisions <- function(selections) {
  records <- unlist(selections, recursive = FALSE)
  ids <- vapply(records, `[[`, character(1), "record_id")
  numbers <- vapply(records, `[[`, integer(1), "revision_number")
  ordered <- order(ids, -numbers, method = "radix")
  records <- records[ordered]
  ids <- ids[ordered]
  newest <- !duplicated(ids)
  revisions <- stats::setNames(
    vapply(records[newest], `[[`, character(1), "revision_id"),
    ids[newest]
  )
  if (length(revisions) > 1000L) {
    stop(
      "The complete evidence basis exceeds 1,000 records; narrow the selection."
    )
  }
  revisions
}

capture_briefing_basis <- function(store, selections, report_md = character()) {
  selected <- briefing_revisions(selections)
  ids <- names(selected)
  snapshot <- graft::graft_snapshot(store)
  view <- graft::graft_at(store, snapshot)
  revisions <- vapply(
    ids,
    function(id) {
      value <- graft::graft_get(view, id, include = character())
      if (
        identical(value$class, "Claim") &&
          !identical(value$record$status, "active")
      ) {
        stop("Review the selection before consulting an inactive claim.")
      }
      revision <- graft::graft_history(view, id, limit = 1L)$revision_id[[1L]]
      if (!identical(revision, selected[[id]])) {
        stop(
          "Selected evidence changed after its receipt; review before checkpointing."
        )
      }
      revision
    },
    character(1)
  )
  list(
    snapshot = snapshot,
    selections = selections,
    record_ids = ids,
    revision_ids = unname(revisions),
    report_md = report_md
  )
}

read_briefing_basis <- function(store, basis) {
  selected <- briefing_revisions(basis$selections)
  ids <- names(selected)
  if (!identical(ids, basis$record_ids)) {
    stop("The checkpoint is missing part of its selected evidence.")
  }
  if (!identical(unname(selected), basis$revision_ids)) {
    stop("The checkpoint revisions are not covered by the selected receipts.")
  }
  view <- graft::graft_at(store, basis$snapshot)
  revisions <- vapply(
    ids,
    function(id) {
      graft::graft_get(view, id, include = character())
      graft::graft_history(view, id, limit = 1L)$revision_id[[1L]]
    },
    character(1)
  )
  if (!identical(unname(revisions), basis$revision_ids)) {
    stop("The checkpoint does not match its exact accepted revisions.")
  }
  tempest::tempest_knowledge(view, record_ids = ids)
}

briefing_changes <- function(store, basis) {
  changed <- graft::graft_changes(
    store,
    since = basis$snapshot,
    record_ids = basis$record_ids,
    limit = 1000L
  )
  if (isTRUE(attr(changed, "truncated"))) {
    stop("The change assessment is incomplete; review before continuing.")
  }
  changed
}
