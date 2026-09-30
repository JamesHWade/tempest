# Each operation runs in a new local process. Only frozen cards and plain review
# handles cross the process boundary; authority is restored from saved runs.
tempest_demo_worker <- function(
  operation,
  directory,
  candidate = NULL,
  expected = NULL,
  review_id = NULL,
  reason = NULL,
  decision_id = NULL,
  package_path = NULL
) {
  if (!is.null(package_path)) {
    pkgload::load_all(package_path, quiet = TRUE)
  }
  helper <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "ellmerverse", "fixture.R", package = "tempest"),
    helper
  )
  evidence_path <- file.path(directory, "evidence")
  store <- graft::graft_store(
    evidence_path,
    create = !dir.exists(evidence_path)
  )
  events <- graft::graft_history(store, "synthetic-pilot")
  head <- if (length(events)) tail(events, 1L)[[1L]] else NULL
  value <- switch(
    operation,
    initial = helper$tempest_demo_research(directory),
    correction = {
      knowledge <- if (!is.null(head) && identical(head@action, "accept")) {
        tempest::tempest_reuse_artifact_research(
          store,
          "synthetic-pilot",
          head@id,
          "demo-briefing",
          eligible = function(event) TRUE
        )
      } else {
        NULL
      }
      helper$tempest_demo_research(
        directory,
        correction = TRUE,
        knowledge = knowledge
      )
    },
    accept = {
      if (!identical(if (is.null(head)) NULL else head@id, expected)) {
        stop("The acceptance basis changed. Refresh and review again.")
      }
      if (
        is.null(candidate) ||
          !dir.exists(file.path(directory, "runs", candidate$run_id))
      ) {
        stop("The reviewed run is unavailable. Research and review again.")
      }
      knowledge <- if (!is.null(candidate$input_decision_id)) {
        tempest::tempest_reuse_artifact_research(
          store,
          "synthetic-pilot",
          candidate$input_decision_id,
          "demo-briefing",
          eligible = function(event) TRUE
        )
      } else {
        NULL
      }
      research <- helper$tempest_demo_research(
        directory,
        correction = candidate$correction,
        knowledge = knowledge
      )
      current_review <- tempest::tempest_trajectory_review_data(tempest::tempest_trajectory_review(
        research
      ))$review_id
      if (!identical(current_review, review_id)) {
        stop("The candidate changed after review.")
      }
      selection <- tempest::tempest_publish_artifact_research(research, store)
      key <- digest::digest(
        list(current_review, expected, reason),
        algo = "sha256"
      )
      graft::graft_accept(
        store,
        graft::graft_read_selection(store, selection),
        "synthetic-pilot",
        expected = expected,
        key = paste0("demo-review-", key),
        actor = "local-demo-reviewer",
        reason = reason,
        purpose = "demo-briefing"
      )
    },
    withdraw = graft::graft_withdraw(
      store,
      "synthetic-pilot",
      expected = expected,
      key = paste0("demo-withdraw-", expected),
      actor = "local-demo-reviewer",
      reason = "The reviewer requested another review."
    ),
    reopen = helper$tempest_demo_reopen(directory, decision_id),
    refresh = NULL,
    stop("Unknown demonstration operation.")
  )
  events <- graft::graft_history(store, "synthetic-pilot")
  accepted <- Filter(function(event) identical(event@action, "accept"), events)
  retained_report <- if (length(accepted)) {
    event <- tail(accepted, 1L)[[1L]]
    tempest::tempest_read_artifact_research(store, event@selection)$report_md
  } else {
    "No report has been reviewed and accepted yet."
  }
  state <- list(
    head = if (length(events)) tail(events, 1L)[[1L]] else NULL,
    retained_report = retained_report,
    history = data.frame(
      sequence = vapply(events, function(event) event@sequence, integer(1)),
      action = vapply(events, function(event) event@action, character(1)),
      reviewer = vapply(events, function(event) event@actor, character(1)),
      reason = vapply(events, function(event) event@reason, character(1))
    )
  )
  if (operation %in% c("initial", "correction")) {
    review <- tempest::tempest_trajectory_review_data(tempest::tempest_trajectory_review(
      value
    ))
    return(list(
      operation = operation,
      candidate = list(
        run_id = review$product$research_run_id,
        correction = identical(operation, "correction"),
        input_decision_id = if (
          identical(operation, "correction") &&
            !is.null(head) &&
            identical(head@action, "accept")
        ) {
          head@id
        } else {
          NULL
        }
      ),
      card = tempest::tempest_mcp_app(value),
      head = head,
      state = state,
      review_id = review$review_id
    ))
  }
  list(operation = operation, value = value, state = state)
}

tempest_demo_process_promise <- function(process) {
  promises::promise(function(resolve, reject) {
    poll <- function() {
      if (process$is_alive()) {
        later::later(poll, 0.1)
      } else {
        tryCatch(resolve(process$get_result()), error = reject)
      }
    }
    poll()
  })
}
