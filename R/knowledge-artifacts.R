# Tempest's evidence input contract; storage and current eligibility are host-owned.
tempest_artifact_selection <- function(selection, allow_empty = FALSE) {
  if (allow_empty && identical(selection, list())) {
    return(list())
  }
  selection <- tempest_resource_metadata(selection, "selection")
  selection <- tempest_product_canonical_value(selection)
  if (
    !setequal(
      names(selection),
      c("selection_id", "purpose", "records", "provenance")
    )
  ) {
    tempest_knowledge_abort(
      "{.arg selection} must contain selection_id, purpose, records and provenance."
    )
  }
  for (field in c("selection_id", "purpose")) {
    selection[[field]] <- tempest_resource_safe_scalar(
      selection[[field]],
      field,
      identifier = identical(field, "selection_id")
    )
  }
  if (!is.list(selection$provenance) || is.data.frame(selection$provenance)) {
    tempest_knowledge_abort(
      "Artifact provenance must be a JSON-compatible list."
    )
  }
  refs <- selection$records
  if (
    !is.list(refs) ||
      !is.null(names(refs)) ||
      !length(refs) ||
      length(refs) > tempest_knowledge_max_records()
  ) {
    tempest_knowledge_abort(
      "{.field selection$records} must be an unnamed list of 1 to 1000 records."
    )
  }
  fields <- c("record_id", "revision_id", "class", "sha256", "dependencies")
  for (ref in refs) {
    if (!is.list(ref) || !setequal(names(ref), fields)) {
      tempest_knowledge_abort(
        "Each artifact reference must contain record_id, revision_id, class, sha256 and dependencies."
      )
    }
    for (field in c("record_id", "revision_id", "class")) {
      value <- tempest_resource_safe_scalar(
        ref[[field]],
        field,
        identifier = TRUE
      )
      if (!identical(value, ref[[field]])) {
        tempest_knowledge_abort(
          "Artifact identifiers must not contain surrounding whitespace."
        )
      }
    }
    if (!ref$class %in% tempest_knowledge_record_allowlist()) {
      tempest_knowledge_abort(
        "Artifact class {.val {ref$class}} is not readable evidence."
      )
    }
    if (!rlang::is_string(ref$sha256) || !grepl("^[0-9a-f]{64}$", ref$sha256)) {
      tempest_knowledge_abort(
        "Each artifact requires a lowercase SHA-256 content digest."
      )
    }
    if (!is.list(ref$dependencies) || !is.null(names(ref$dependencies))) {
      tempest_knowledge_abort(
        "Artifact dependencies must be an unnamed list of exact references."
      )
    }
    for (dependency in ref$dependencies) {
      if (
        !is.list(dependency) ||
          !setequal(names(dependency), c("record_id", "revision_id"))
      ) {
        tempest_knowledge_abort(
          "Each dependency requires record_id and revision_id."
        )
      }
      for (field in c("record_id", "revision_id")) {
        value <- tempest_resource_safe_scalar(
          dependency[[field]],
          field,
          identifier = TRUE
        )
        if (!identical(value, dependency[[field]])) {
          tempest_knowledge_abort(
            "Artifact identifiers must not contain surrounding whitespace."
          )
        }
      }
      matched <- vapply(
        refs,
        function(candidate) {
          identical(candidate$record_id, dependency$record_id) &&
            identical(candidate$revision_id, dependency$revision_id)
        },
        logical(1)
      )
      if (sum(matched) != 1L) {
        tempest_knowledge_abort(
          "Artifact selection is missing an exact dependency."
        )
      }
    }
    if (anyDuplicated(ref$dependencies)) {
      tempest_knowledge_abort("Artifact dependencies must be unique.")
    }
  }
  ids <- vapply(refs, \(ref) ref$record_id, character(1))
  if (anyDuplicated(ids)) {
    tempest_knowledge_abort("Artifact selection record ids must be unique.")
  }
  selection$records <- refs[order(ids, method = "radix")]
  if (
    nchar(tempest_product_canonical_json(selection), type = "bytes") > 1024^2
  ) {
    tempest_knowledge_abort("Artifact selection metadata exceeds 1 MiB.")
  }
  selection
}

tempest_artifact_resource <- function(selection, ref, content) {
  if (
    !rlang::is_string(content) ||
      is.na(content) ||
      !nzchar(content) ||
      !identical(
        digest::digest(
          charToRaw(enc2utf8(content)),
          algo = "sha256",
          serialize = FALSE
        ),
        ref$sha256
      )
  ) {
    tempest_knowledge_abort(
      "Artifact {.val {ref$record_id}} content is missing or differs from its SHA-256 digest."
    )
  }
  tempest_resource(
    resource_kind = "artifact.record",
    locator = paste0(
      "artifact/",
      tempest_product_record_hash(ref[c("class", "record_id", "revision_id")])
    ),
    title = paste(ref$class, ref$record_id),
    media_type = "text/plain",
    content = content,
    metadata = list(
      artifact_record_id = ref$record_id,
      artifact_revision_id = ref$revision_id,
      artifact_record_class = ref$class,
      artifact_selection_id = selection$selection_id,
      artifact_selection_digest = tempest_product_record_hash(selection)
    )
  )
}

#' Bring a retained artifact selection into a research run
#'
#' The host resolves immutable content and checks current permission and
#' consultation eligibility before calling this constructor. Tempest verifies
#' exact selection coverage, content digests and declared dependencies. It does
#' not open storage, infer approval, or grant execution authority.
#'
#' @param selection A JSON-compatible list with `selection_id` (host-owned
#'   identity), `purpose`, `records`, and `provenance` (a list of source references).
#'   Each record has `record_id`, `revision_id`, `class`, `sha256` (lowercase
#'   SHA-256 of UTF-8 text bytes), and `dependencies` (a list of exact
#'   `record_id`/`revision_id` pairs). Supported classes are `Claim`,
#'   `ClaimSupport`, `EvidenceSpan`, and `Source`. The selection contains 1 to
#'   1000 unique records; its metadata is limited to 1 MiB. Record and revision
#'   identifiers must not contain surrounding whitespace. Dependencies describe
#'   the host's evidence selection, not an inferred graph or authorization.
#' @param contents Named list of retained text strings, keyed by `record_id`,
#'   with exactly the selection's records and at most 1 MiB of UTF-8 text total.
#'   Claims use the same inert `statement_text: ...` and `status: ...` fields as
#'   the accepted research projection; active statements support no-change
#'   briefing detection. Content is never evaluated as code or instructions.
#' @return A `TempestKnowledge` value for [tempest_run()] or [tempest_session()].
#'   The run retains the selection separately from any Graft snapshot. Its
#'   computed input digest binds the input description; it does not authenticate
#'   a host, prove factual truth, or represent a native acceptance event.
#' @seealso [tempest_knowledge()] for the existing Graft producer.
#' @export
tempest_artifact_knowledge <- function(selection, contents) {
  selection <- tempest_artifact_selection(selection)
  ids <- vapply(selection$records, \(ref) ref$record_id, character(1))
  if (
    !is.list(contents) ||
      is.data.frame(contents) ||
      anyDuplicated(names(contents)) ||
      !setequal(names(contents), ids)
  ) {
    tempest_knowledge_abort(
      "{.arg contents} must cover the selected record ids exactly once."
    )
  }
  sizes <- vapply(
    contents,
    function(content) {
      if (!rlang::is_string(content) || is.na(content)) {
        tempest_knowledge_abort(
          "Artifact contents must be nonmissing text strings."
        )
      }
      nchar(enc2utf8(content), type = "bytes")
    },
    numeric(1)
  )
  if (sum(sizes) > 1024^2) {
    tempest_knowledge_abort("Artifact contents exceed 1 MiB.")
  }
  records <- lapply(selection$records, function(ref) {
    tempest_artifact_resource(selection, ref, contents[[ref$record_id]])
  })
  TempestKnowledge(
    record_ids = ids,
    records = records,
    artifact_selection = selection
  )
}

# Revalidate the retained input at admission, publication and restore. Ordinary
# retrieved resources may coexist, but artifact resources cannot be added/dropped.
tempest_artifact_selection_validate <- function(selection, resources) {
  selection <- tempest_artifact_selection(selection, allow_empty = TRUE)
  artifacts <- Filter(
    \(resource) identical(resource@resource_kind, "artifact.record"),
    resources
  )
  if (!length(selection)) {
    if (length(artifacts)) {
      tempest_knowledge_abort(
        "Artifact evidence requires its retained selection."
      )
    }
    return(invisible(NULL))
  }
  ids <- vapply(
    artifacts,
    \(resource) resource@metadata$artifact_record_id %||% "",
    character(1)
  )
  expected <- vapply(
    selection$records,
    \(ref) ref$record_id,
    character(1)
  )
  if (anyDuplicated(ids) || !setequal(ids, expected)) {
    tempest_knowledge_abort(
      "Artifact resources do not cover the retained selection exactly."
    )
  }
  if (
    sum(vapply(
      artifacts,
      function(resource) {
        nchar(enc2utf8(resource@content), type = "bytes")
      },
      numeric(1)
    )) >
      1024^2
  ) {
    tempest_knowledge_abort("Artifact contents exceed 1 MiB.")
  }
  for (ref in selection$records) {
    resource <- artifacts[[match(ref$record_id, ids)]]
    tempest_resource_data(resource)
    rebuilt <- tempest_artifact_resource(selection, ref, resource@content)
    fields <- setdiff(tempest_resource_data_fields(), "retrieved_at")
    if (
      !identical(
        tempest_resource_data(resource)[fields],
        tempest_resource_data(rebuilt)[fields]
      )
    ) {
      tempest_knowledge_abort(
        "Artifact resource identity or provenance differs from its retained selection."
      )
    }
  }
  invisible(NULL)
}
