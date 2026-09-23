#' Preserve completed research in a Graft artifact store
#'
#' Publish the validated proposal, exact source bodies, evidence, program
#' provenance and readable report as one immutable selection. Publication is
#' not acceptance. The host reviews the selection and explicitly calls
#' `graft::graft_accept()` to accept it or [graft::graft_withdraw()] to
#' withdraw it.
#'
#' @param research A completed research product accepted by
#'   [tempest_promotion_bundle()], or a validated `TempestPromotionBundle`.
#' @param store A `graft::graft_store()` handle. Applications enforce
#'   access to this trusted local, single-writer store.
#' @param claim_ids Optional exact claim selection, passed to
#'   [tempest_promotion_bundle()].
#' @param report Exact report text when `research` is a promotion bundle. Its
#'   digest must match the retained completed-product manifest. Omit it for a
#'   live completed product.
#' @returns An immutable Graft selection digest. Identical publication reuses
#'   the same artifacts; corrected contents retain the earlier selection.
#' @seealso [tempest_read_artifact_research()], [tempest_reuse_artifact_research()]
#' @export
tempest_publish_artifact_research <- function(
  research,
  store,
  claim_ids = NULL,
  report = NULL
) {
  if (S7::S7_inherits(research, TempestPromotionBundle)) {
    if (!is.null(claim_ids)) {
      tempest_knowledge_abort(
        "A promotion bundle already fixes its claim selection."
      )
    }
    tempest_promotion_bundle_data(research)
    bundle <- research
  } else {
    if (!is.null(report)) {
      tempest_knowledge_abort(
        "A completed product supplies its own exact report."
      )
    }
    bundle <- tempest_promotion_bundle(research, claim_ids)
    report <- tempest_report(research)
  }
  tempest_artifact_report_validate(bundle, report)
  data <- tempest_artifact_research_records(bundle)
  refs <- list()
  for (record in data) {
    dependencies <- unname(refs[record$dependencies])
    refs[[record$id]] <- graft::graft_save(
      store,
      charToRaw(enc2utf8(record$content)),
      record$id,
      "text/plain",
      dependencies = dependencies
    )
  }
  report_ref <- graft::graft_save(
    store,
    charToRaw(enc2utf8(report)),
    paste0("tempest:report:", bundle@research_run_id),
    "text/markdown",
    dependencies = unname(refs)
  )
  candidate <- list(
    format = "tempest-research-1",
    bundle = tempest_promotion_bundle_data(bundle),
    report = tempest_graft_ref_record(report_ref),
    records = unname(lapply(data, function(record) {
      list(id = record$id, ref = tempest_graft_ref_record(refs[[record$id]]))
    }))
  )
  root <- graft::graft_save(
    store,
    charToRaw(as.character(jsonlite::toJSON(
      candidate,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      force = TRUE
    ))),
    paste0("tempest:research:", bundle@research_run_id),
    "application/json",
    dependencies = c(list(report_ref), unname(refs))
  )
  graft::graft_select(store, root)@id
}

#' Inspect retained research without admitting it to execution
#'
#' Verify the complete selected contents and Tempest evidence proof. Historical
#' inspection does not imply current acceptance or grant reuse permission.
#' Reports remain model-generated synthesis, separate from source evidence.
#'
#' @inheritParams tempest_publish_artifact_research
#' @param selection Exact digest returned by [tempest_publish_artifact_research()].
#' @returns A list with `selection`, validated `bundle`, `report_md`, and the
#'   exact evidence `records` and text `contents` used for research admission.
#'   The selection retains program identities as inert provenance, never tools
#'   or execution authority. Evidence is limited to 998 records and 1 MiB of
#'   projected text; Graft also enforces its store and selection bounds.
#' @export
tempest_read_artifact_research <- function(store, selection) {
  selection_id <- if (S7::S7_inherits(selection, graft::ArtifactSelection)) {
    selection@id
  } else if (is.list(selection) && !is.null(selection$id)) {
    selection$id
  } else {
    selection
  }
  selected <- graft::graft_read_selection(store, selection_id)
  if (length(selected@roots) != 1L) {
    tempest_knowledge_abort("Research requires one exact proposal root.")
  }
  root <- graft::graft_read(store, selected@roots[[1L]])
  candidate <- tryCatch(
    jsonlite::fromJSON(rawToChar(root@bytes), simplifyVector = FALSE),
    error = function(error) {
      tempest_knowledge_abort(
        "Research proposal is not valid JSON.",
        parent = error
      )
    }
  )
  if (
    !is.list(candidate) ||
      anyDuplicated(names(candidate)) ||
      !setequal(names(candidate), c("format", "bundle", "report", "records")) ||
      !identical(candidate$format, "tempest-research-1")
  ) {
    tempest_knowledge_abort("Unsupported retained research proposal.")
  }
  bundle <- tryCatch(
    tempest_promotion_bundle_from_data(candidate$bundle),
    error = function(error) {
      tempest_knowledge_abort(
        "Retained research contains an invalid bundle.",
        parent = error
      )
    }
  )
  if (
    !tempest_artifact_ref_matches(
      selected@roots[[1L]],
      paste0("tempest:research:", bundle@research_run_id)
    )
  ) {
    tempest_knowledge_abort(
      "Research proposal identity differs from its proof."
    )
  }
  if (
    !tempest_artifact_ref_matches(
      candidate$report,
      paste0("tempest:report:", bundle@research_run_id)
    )
  ) {
    tempest_knowledge_abort("Research report identity differs from its proof.")
  }
  expected <- tempest_artifact_research_records(bundle)
  if (
    !is.list(candidate$records) ||
      !is.null(names(candidate$records)) ||
      length(candidate$records) != length(expected)
  ) {
    tempest_knowledge_abort("Research proposal does not cover its evidence.")
  }
  refs <- list()
  records <- list()
  contents <- list()
  for (i in seq_along(expected)) {
    record <- expected[[i]]
    saved <- candidate$records[[i]]
    if (
      !is.list(saved) ||
        anyDuplicated(names(saved)) ||
        !setequal(names(saved), c("id", "ref")) ||
        !identical(saved$id, record$id) ||
        !tempest_artifact_ref_matches(saved$ref, record$id)
    ) {
      tempest_knowledge_abort(
        "Research evidence identity differs from its proof."
      )
    }
    item <- graft::graft_read(store, tempest_graft_ref_value(saved$ref))
    dependencies <- unname(refs[record$dependencies])
    if (
      !identical(item@bytes, charToRaw(enc2utf8(record$content))) ||
        !identical(
          lapply(item@dependencies, tempest_graft_ref_record),
          lapply(dependencies, tempest_graft_ref_record)
        ) ||
        !identical(item@media_type, "text/plain")
    ) {
      tempest_knowledge_abort(
        "Research evidence content or dependencies differ from its proof."
      )
    }
    refs[[record$id]] <- saved$ref
    contents[[record$id]] <- record$content
    records[[i]] <- list(
      record_id = record$id,
      revision_id = saved$ref$revision,
      class = record$class,
      sha256 = digest::digest(item@bytes, algo = "sha256", serialize = FALSE),
      dependencies = lapply(dependencies, function(ref) {
        list(record_id = ref@id, revision_id = ref@revision)
      })
    )
  }
  report <- graft::graft_read(store, tempest_graft_ref_value(candidate$report))
  report_md <- tryCatch(
    rawToChar(report@bytes),
    error = function(error) {
      tempest_knowledge_abort(
        "Retained research contains invalid report bytes.",
        parent = error
      )
    }
  )
  if (!validUTF8(report_md)) {
    tempest_knowledge_abort(
      "Retained research report must contain valid UTF-8."
    )
  }
  tempest_artifact_report_validate(bundle, report_md)
  if (
    !identical(
      lapply(report@dependencies, tempest_graft_ref_record),
      lapply(unname(refs), tempest_graft_ref_record)
    ) ||
      !identical(report@media_type, "text/markdown") ||
      !identical(
        lapply(root@dependencies, tempest_graft_ref_record),
        c(
          list(candidate$report),
          lapply(unname(refs), tempest_graft_ref_record)
        )
      ) ||
      !identical(root@media_type, "application/json")
  ) {
    tempest_knowledge_abort(
      "Research proposal or report has inconsistent dependencies."
    )
  }
  list(
    selection = selected@id,
    bundle = bundle,
    report_md = report_md,
    records = records,
    contents = contents
  )
}

#' Admit a current accepted research decision
#'
#' Resolve an exact accepted decision through Graft and validate its research
#' evidence. The host callback is called again when this value enters
#' [tempest_run()] or [tempest_session()], or is supplied to
#' [tempest_session_resume()]. It must return exactly `TRUE` for the current
#' purpose. The callback and store handle are transient and are never saved in
#' a session. Resume requires a newly supplied knowledge value.
#'
#' This checks admission at a point in time. Hosts own active-task cancellation,
#' authorization on all read paths and any subsequent policy changes.
#'
#' @inheritParams tempest_publish_artifact_research
#' @param stream Graft decision stream.
#' @param decision Exact acceptance event digest, distinct from the selection.
#' @param purpose Requested consultation purpose.
#' @param eligible Host function receiving the recorded decision and returning
#'   a single logical eligibility decision. Do not cache an earlier permission.
#' @returns A `TempestKnowledge` value retaining decision provenance and exact
#'   selected evidence, with no executable procedures.
#' @export
tempest_reuse_artifact_research <- function(
  store,
  stream,
  decision,
  purpose,
  eligible
) {
  if (!is.function(eligible)) {
    tempest_knowledge_abort(
      "{.arg eligible} must be a current host policy function."
    )
  }
  admit <- function() {
    event <- tempest_graft_decision(store, stream, decision)
    event_record <- tempest_graft_decision_record(event)
    current <- graft::graft_recall(
      store,
      stream,
      purpose,
      eligible = eligible(event_record)
    )
    if (
      !identical(current@status, "accepted") ||
        is.null(current@decision) ||
        !identical(current@decision@id, event@id)
    ) {
      tempest_knowledge_abort(
        "The requested Graft decision is not the current accepted decision."
      )
    }
    research <- tempest_read_artifact_research(store, current@selection@id)
    input <- list(
      selection_id = research$selection,
      purpose = purpose,
      records = research$records,
      provenance = list(
        graft_decision = event_record,
        bundle_id = research$bundle@bundle_id,
        research_run_id = research$bundle@research_run_id
      )
    )
    knowledge <- tempest_artifact_knowledge(input, research$contents)
    # Recheck after domain validation and materialization, before admission.
    current <- graft::graft_recall(
      store,
      stream,
      purpose,
      eligible = eligible(event_record)
    )
    if (
      !identical(current@status, "accepted") ||
        is.null(current@decision) ||
        !identical(current@decision@id, event@id)
    ) {
      tempest_knowledge_abort(
        "The requested Graft decision is not the current accepted decision."
      )
    }
    knowledge
  }
  knowledge <- admit()
  knowledge@admission <- admit
  knowledge
}

tempest_graft_ref_record <- function(ref) {
  if (S7::S7_inherits(ref, graft::ArtifactRef)) {
    return(list(id = ref@id, revision = ref@revision))
  }
  if (
    !is.list(ref) ||
      !identical(names(ref), c("id", "revision")) ||
      !rlang::is_string(ref$id) ||
      !rlang::is_string(ref$revision)
  ) {
    tempest_knowledge_abort("Graft artifact references have an invalid shape.")
  }
  ref
}

tempest_graft_ref_value <- function(ref) {
  ref <- tempest_graft_ref_record(ref)
  graft::ArtifactRef(id = ref$id, revision = ref$revision)
}

tempest_artifact_ref_matches <- function(ref, id) {
  ref <- tryCatch(
    tempest_graft_ref_record(ref),
    error = function(error) NULL
  )
  !is.null(ref) &&
    identical(ref$id, id) &&
    grepl("^[0-9a-f]{64}$", ref$revision)
}

tempest_graft_decision <- function(store, stream, decision) {
  decision_id <- if (S7::S7_inherits(decision, graft::Decision)) {
    decision@id
  } else {
    decision
  }
  history <- graft::graft_history(store, stream)
  matches <- Filter(
    function(event) identical(event@id, decision_id),
    history
  )
  if (length(matches) != 1L) {
    tempest_knowledge_abort("The requested Graft decision was not found.")
  }
  matches[[1L]]
}

tempest_graft_decision_record <- function(event) {
  list(
    id = event@id,
    sequence = event@sequence,
    stream = event@stream,
    key = event@key,
    previous = event@previous,
    action = event@action,
    selection = event@selection,
    actor = event@actor,
    reason = event@reason,
    purpose = event@purpose
  )
}

tempest_artifact_report_validate <- function(bundle, report) {
  expected <- bundle@research_manifest$deliverables$report_md
  actual <- c(
    tempest_product_report_reference(report),
    list(status = "durable")
  )
  if (!identical(expected, actual)) {
    tempest_knowledge_abort(
      "Retained synthesis differs from the completed report."
    )
  }
}

tempest_artifact_research_records <- function(bundle) {
  result <- list()
  for (class in c("Source", "Claim", "EvidenceSpan", "ClaimSupport")) {
    for (row in bundle@records[[class]]) {
      id <- paste(
        class,
        row[[tempest_promotion_record_id_field(class)]],
        sep = ":"
      )
      fields <- switch(
        class,
        EvidenceSpan = c(source_id = "Source"),
        ClaimSupport = c(
          statement_id = "Claim",
          source_id = "Source",
          evidence_span_id = "EvidenceSpan"
        ),
        character()
      )
      dependencies <- vapply(
        names(fields),
        function(field) {
          paste(fields[[field]], row[[field]], sep = ":")
        },
        character(1)
      )
      content <- if (class == "Claim") {
        as.character(jsonlite::toJSON(
          row,
          auto_unbox = TRUE,
          null = "null",
          digits = NA
        ))
      } else {
        tempest_knowledge_record_text(row, id)
      }
      if (class == "Source") {
        source <- Filter(
          function(resource) {
            identical(resource$resource_id, row$tempest_source_id)
          },
          bundle@proof$resources
        )
        if (length(source) != 1L) {
          tempest_knowledge_abort("Source proof is missing or ambiguous.")
        }
        content <- paste(content, "", source[[1L]]$content, sep = "\n")
      }
      result[[length(result) + 1L]] <- list(
        id = id,
        class = class,
        content = content,
        dependencies = unname(dependencies)
      )
    }
  }
  if (
    !length(result) ||
      length(result) > 998L ||
      sum(vapply(
        result,
        function(x) nchar(enc2utf8(x$content), type = "bytes"),
        numeric(1)
      )) >
        1024^2
  ) {
    tempest_knowledge_abort(
      "Research evidence exceeds artifact admission bounds."
    )
  }
  result
}

tempest_artifact_resume_admission <- function(selection, knowledge) {
  admitted <- tempest_knowledge_argument(knowledge, admit = FALSE)
  if (!identical(admitted$artifact_selection, selection)) {
    tempest_knowledge_abort(
      "Resuming artifact research requires fresh admission of its exact retained knowledge."
    )
  }
  if (length(selection)) {
    tempest_knowledge_argument(knowledge)
  }
  invisible(NULL)
}
