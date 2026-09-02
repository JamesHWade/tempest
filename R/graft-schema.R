# Tempest's compiled Graft research contract and review-only planning adapter

tempest_graft_contract_version <- "0.2.0"

# The Graft store format whose snapshots, receipts, and trajectory reviews
# Tempest can validate offline.
tempest_graft_store_format_version <- "3.1.0"

tempest_graft_required_exports <- function() {
  c(
    "graft_at",
    "graft_changes",
    "graft_contract_version",
    "graft_get",
    "graft_history",
    "graft_plan",
    "graft_provenance",
    "graft_schema",
    "graft_snapshot",
    "graft_view_snapshot"
  )
}

tempest_graft_plan_call <- function(store, records, provenance) {
  graft::graft_plan(store, records = records, provenance = provenance)
}

tempest_graft_snapshot_call <- function(store) {
  graft::graft_snapshot(store)
}

tempest_graft_contract_call <- function() {
  graft::graft_contract_version()
}

# Graft publishes a semantic consumer contract version. Tempest accepts the
# same major and minor contract at any patch level; a different minor or
# major version may change argument shapes or return values Tempest relies on.
tempest_graft_pin_valid <- function(version) {
  if (!is.list(version) || !rlang::is_string(version$contract)) {
    return(FALSE)
  }
  observed <- tryCatch(
    numeric_version(version$contract),
    error = function(error) NULL
  )
  if (is.null(observed)) {
    return(FALSE)
  }
  required <- numeric_version(tempest_graft_contract_version)
  identical(observed[[1L, 1L]], required[[1L, 1L]]) &&
    identical(observed[[1L, 2L]], required[[1L, 2L]]) &&
    observed >= required
}

tempest_graft_require <- function() {
  if (!requireNamespace("graft", quietly = TRUE)) {
    tempest_promotion_abort(
      paste0(
        "Graft with consumer contract ",
        tempest_graft_contract_version,
        " is required for research promotion."
      ),
      class = "tempest_graft_schema_error"
    )
  }
  contract <- tryCatch(
    tempest_graft_contract_call(),
    error = function(error) NULL
  )
  if (
    !tempest_graft_pin_valid(contract) ||
      !identical(contract$store_format, tempest_graft_store_format_version)
  ) {
    describe <- function(value) {
      if (rlang::is_string(value)) value else "unknown"
    }
    tempest_promotion_abort(
      paste0(
        "The installed Graft package reports consumer contract ",
        describe(contract$contract),
        " and store format ",
        describe(contract$store_format),
        ", but Tempest requires contract ",
        tempest_graft_contract_version,
        " and store format ",
        tempest_graft_store_format_version,
        "."
      ),
      class = "tempest_graft_schema_error"
    )
  }
  missing <- setdiff(
    tempest_graft_required_exports(),
    getNamespaceExports("graft")
  )
  if (length(missing) > 0L) {
    tempest_promotion_abort(
      "The installed Graft package lacks the approved snapshot accessor API.",
      class = "tempest_graft_schema_error"
    )
  }
  invisible(TRUE)
}

tempest_graft_schema_path <- function() {
  path <- system.file(
    "schema",
    "tempest-research.graft.json",
    package = "tempest"
  )
  if (!nzchar(path) && file.exists("inst/schema/tempest-research.graft.json")) {
    path <- "inst/schema/tempest-research.graft.json"
  }
  if (!nzchar(path) || !file.exists(path)) {
    tempest_promotion_abort(
      "The compiled Tempest research Graft schema is unavailable.",
      class = "tempest_graft_schema_error"
    )
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Load Tempest's compiled scientific Graft schema
#'
#' The packaged contract is compiled for Graft consumer contract `0.2.0`, which
#' runtime loading checks through `graft::graft_contract_version()`. Loading
#' never compiles LinkML and rejects any manifest whose immutable build digest
#' differs.
#'
#' @return A validated `graft::GraftSchema`.
#' @export
tempest_graft_schema <- function() {
  tempest_graft_require()
  schema <- tryCatch(
    graft::graft_schema(tempest_graft_schema_path()),
    error = function(error) {
      tempest_promotion_abort(
        "Could not load the compiled Tempest research schema.",
        class = "tempest_graft_schema_error"
      )
    }
  )
  if (
    !inherits(schema, "graft::GraftSchema") ||
      !identical(schema@build_digest, tempest_promotion_schema_build_digest) ||
      !identical(
        names(schema@classes),
        c(
          "Claim",
          "ClaimSupport",
          "EvidenceSpan",
          "GovernedProcedure",
          "ProgramArtifact",
          "Source",
          "GraftDefinition"
        )
      )
  ) {
    tempest_promotion_abort(
      "The packaged Tempest research schema has an unexpected identity.",
      class = "tempest_graft_schema_error"
    )
  }
  schema
}

tempest_graft_store_validate <- function(store) {
  tempest_graft_require()
  if (!inherits(store, "graft::GraftStore")) {
    tempest_promotion_abort(
      "{.arg store} must be a graft::GraftStore.",
      class = "tempest_graft_plan_error"
    )
  }
  tryCatch(
    S7::validate(store),
    error = function(error) {
      tempest_promotion_abort(
        "{.arg store} is not a valid open Graft store.",
        class = "tempest_graft_plan_error"
      )
    }
  )
  if (
    !identical(store@schema@build_digest, tempest_promotion_schema_build_digest)
  ) {
    tempest_promotion_abort(
      "The Graft store does not use Tempest's exact research schema build.",
      class = "tempest_graft_plan_error"
    )
  }
  store
}

tempest_graft_missing_value <- function(slot, size) {
  switch(
    slot$duckdb_type,
    BIGINT = rep(NA_integer_, size),
    DOUBLE = rep(NA_real_, size),
    DECIMAL = rep(NA_real_, size),
    BOOLEAN = rep(NA, size),
    rep(NA_character_, size)
  )
}

tempest_graft_records_data_frame <- function(rows, schema, record_class) {
  fields <- names(rows[[1L]])
  slots <- schema@manifest$classes[[record_class]]$slots
  columns <- stats::setNames(
    lapply(fields, function(field) {
      values <- lapply(rows, `[[`, field)
      column <- tempest_graft_missing_value(slots[[field]], length(rows))
      for (index in seq_along(values)) {
        value <- values[[index]]
        if (!is.null(value)) {
          column[[index]] <- value
        }
      }
      column
    }),
    fields
  )
  as.data.frame(
    columns,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    optional = TRUE
  )
}

tempest_graft_bundle_records <- function(records, schema) {
  stats::setNames(
    lapply(names(records), function(record_class) {
      tempest_graft_records_data_frame(
        records[[record_class]],
        schema,
        record_class
      )
    }),
    names(records)
  )
}

# Two experts can extract the same statement in one run. Accepted Claim
# identity is keyed on the statement text, so such rows must become one
# planned Claim; their supports are kept and re-pointed at that Claim, and a
# support that would then repeat the same statement and evidence span is
# dropped. The returned alias maps every bundle claim id to the id it planned
# under.
tempest_graft_coalesce_bundle_rows <- function(records) {
  records <- tempest_graft_coalesce_sources(records)
  claims <- records$Claim %||% list()
  if (length(claims) == 0L) {
    return(list(records = records, alias = character()))
  }
  ids <- vapply(claims, `[[`, character(1), "tempest_claim_id")
  keys <- tempest_claim_text_key(
    vapply(claims, `[[`, character(1), "statement_text")
  )
  # The kept row for repeated text is the best-supported claim (ties broken by
  # claim id), never the first in input order. Duplicates must agree on the
  # classification; differing judgments are a conflict, not a merge.
  scores <- vapply(
    claims,
    \(claim) as.numeric(claim$support_score %||% NA_real_),
    numeric(1)
  )
  preferred <- order(keys, -scores, ids, na.last = TRUE)
  kept_for_key <- stats::setNames(ids[preferred], keys[preferred])
  kept_for_key <- kept_for_key[!duplicated(names(kept_for_key))]
  alias <- stats::setNames(unname(kept_for_key[keys]), ids)
  for (key in unique(keys[duplicated(keys)])) {
    group <- claims[keys == key]
    for (field in tempest_claim_coalesce_invariant_fields()) {
      values <- unique(vapply(
        group,
        \(claim) as.character(claim[[field]] %||% NA_character_),
        character(1)
      ))
      if (length(values) > 1L) {
        tempest_graft_plan_abort(paste0(
          "Promotion contains claims with the same statement text but ",
          "conflicting {.field ",
          field,
          "} values ({.val {values}}); resolve the disagreement before ",
          "promoting."
        ))
      }
    }
  }
  records$Claim <- claims[ids %in% kept_for_key]
  supports <- records$ClaimSupport %||% list()
  if (length(supports) > 0L) {
    support_claims <- vapply(
      supports,
      \(row) alias[[row$tempest_claim_id]],
      character(1)
    )
    support_keys <- paste(
      support_claims,
      vapply(supports, `[[`, character(1), "evidence_span_id"),
      sep = "\u001f"
    )
    # When two coalesced claims cite the same evidence span, keep the support
    # judged for the kept claim, then the stronger judgment, then input order.
    own <- vapply(
      supports,
      \(row) identical(row$tempest_claim_id, alias[[row$tempest_claim_id]]),
      logical(1)
    )
    scores <- vapply(
      supports,
      \(row) as.numeric(row$support_score %||% NA_real_),
      numeric(1)
    )
    ordering <- order(!own, -scores, seq_along(supports), na.last = TRUE)
    kept <- supports[ordering][!duplicated(support_keys[ordering])]
    records$ClaimSupport <- kept[order(match(kept, supports))]
  }
  merged <- names(alias)[alias != names(alias)]
  if (length(merged) > 0L) {
    records$Claim <- lapply(records$Claim, function(claim) {
      if (!claim$tempest_claim_id %in% alias[merged]) {
        return(claim)
      }
      summary <- tempest_graft_coalesced_claim_summary(Filter(
        \(row) identical(alias[[row$tempest_claim_id]], claim$tempest_claim_id),
        records$ClaimSupport
      ))
      claim$verification_status <- summary$status
      claim$support_score <- summary$score
      claim
    })
  }
  list(records = records, alias = alias)
}

tempest_claim_coalesce_invariant_fields <- function() {
  "claim_type"
}

# Accepted Source identity is keyed on the locator, so two bundle Sources
# with the same locator become one planned Source; spans and supports are
# re-pointed at it.
tempest_graft_coalesce_sources <- function(records) {
  sources <- records$Source %||% list()
  if (length(sources) < 2L) {
    return(records)
  }
  ids <- vapply(sources, `[[`, character(1), "tempest_source_id")
  locators <- tempest_trim(vapply(sources, `[[`, character(1), "locator"))
  alias <- stats::setNames(ids[match(locators, locators)], ids)
  if (all(alias == names(alias))) {
    return(records)
  }
  records$Source <- sources[!duplicated(locators)]
  repoint <- function(rows) {
    lapply(rows, function(row) {
      if (rlang::is_string(row$source_id) && row$source_id %in% names(alias)) {
        row$source_id <- alias[[row$source_id]]
      }
      row
    })
  }
  records$EvidenceSpan <- repoint(records$EvidenceSpan %||% list())
  records$ClaimSupport <- repoint(records$ClaimSupport %||% list())
  records
}

# The kept Claim's summary must describe its merged support set, computed the
# same way the promotion bundle computed it for the original claim.
tempest_graft_coalesced_claim_summary <- function(supports) {
  tempest_promotion_support_summary(lapply(
    supports,
    tempest_promotion_claim_support_from_row
  ))
}

tempest_graft_plan_abort <- function(
  message,
  .envir = rlang::caller_env()
) {
  tempest_promotion_abort(
    message,
    class = "tempest_graft_plan_error",
    .envir = .envir
  )
}

tempest_graft_plan_require_valid <- function(plan, phase) {
  if (!inherits(plan, "graft::GraftCommitPlan")) {
    tempest_graft_plan_abort(
      "Graft did not return a commit plan during {.val {phase}} planning."
    )
  }
  plan <- tryCatch(
    utils::getFromNamespace("validate_graft_commit_plan", "graft")(plan),
    error = function(error) {
      tempest_graft_plan_abort(
        "The Graft {.val {phase}} plan failed its pinned integrity check."
      )
    }
  )
  if (!isTRUE(plan@valid)) {
    tempest_graft_plan_abort(
      "Tempest {.val {phase}} promotion planning returned validation issues."
    )
  }
  plan
}

tempest_graft_get_call <- function(store, record_id) {
  graft::graft_get(store, record_id, include = character())
}

# A re-verified statement resolves to its accepted Claim through the text
# origin key. When that record was retracted or superseded, planning an
# update would silently reactivate it, so the conflict is surfaced instead.
tempest_graft_assert_claim_lifecycle <- function(store, seed) {
  changes <- seed@changes
  targets <- changes$record_id[
    changes$class == "Claim" & changes$action == "update"
  ]
  for (record_id in unique(targets)) {
    current <- tryCatch(
      tempest_graft_get_call(store, record_id),
      error = function(error) {
        tempest_graft_plan_abort(
          "Could not read the accepted Claim {.val {record_id}} during planning."
        )
      }
    )
    status <- current$record$status
    if (rlang::is_string(status) && !identical(status, "active")) {
      tempest_graft_plan_abort(paste0(
        "Promotion re-proposes a statement whose accepted Claim ",
        "{.val {record_id}} is {.val {status}}; retract the proposal or ",
        "supersede that record explicitly instead of reactivating it."
      ))
    }
  }
  invisible(seed)
}

tempest_graft_seed_map <- function(plan, record_class, key_field) {
  data <- plan@records[[record_class]]
  ids <- as.character(data$id)
  keys <- as.character(data[[key_field]])
  if (
    length(ids) == 0L ||
      anyNA(ids) ||
      anyNA(keys) ||
      anyDuplicated(keys) ||
      anyDuplicated(ids)
  ) {
    tempest_graft_plan_abort(
      "Graft returned an invalid deterministic {.val {record_class}} identity map."
    )
  }
  stats::setNames(ids, keys)
}

tempest_graft_plan_idempotency_key <- function(
  bundle,
  planning_snapshot_id,
  store_id
) {
  if (
    !rlang::is_string(planning_snapshot_id) ||
      is.na(planning_snapshot_id) ||
      !grepl("^sha256:[a-f0-9]{64}$", planning_snapshot_id) ||
      !rlang::is_string(store_id) ||
      is.na(store_id) ||
      !nzchar(store_id)
  ) {
    tempest_graft_plan_abort(
      "The Graft planning snapshot identity is malformed."
    )
  }
  tempest_promotion_digest(list(
    contract = "tempest.graft-promotion-plan/1",
    promotion_bundle_id = bundle@bundle_id,
    planning_snapshot_id = planning_snapshot_id,
    store_id = store_id
  ))
}

tempest_graft_plan_provenance <- function(
  bundle,
  planning_snapshot_id,
  store_id
) {
  version <- tryCatch(
    as.character(utils::packageVersion("tempest")),
    error = function(error) {
      tempest_graft_plan_abort(
        "Could not identify the Tempest promotion producer version."
      )
    }
  )
  graft::graft_provenance(
    producer = "tempest",
    version = version,
    run_id = bundle@research_run_id,
    idempotency_key = tempest_graft_plan_idempotency_key(
      bundle,
      planning_snapshot_id,
      store_id
    ),
    metadata = list(
      graft_contract_version = tempest_graft_contract_version,
      promotion_bundle_id = bundle@bundle_id,
      research_manifest_digest = tempest_promotion_digest(
        bundle@research_manifest
      ),
      schema_build_digest = bundle@schema_build_digest,
      planning_snapshot_id = planning_snapshot_id
    )
  )
}

tempest_graft_plan_timestamp_value <- function(value) {
  if (
    !rlang::is_string(value) ||
      is.na(value) ||
      !grepl(
        paste0(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:",
          "[0-9]{2}:[0-9]{2}(?:\\.[0-9]{1,6})?Z$"
        ),
        value
      )
  ) {
    return(NULL)
  }
  parsed <- suppressWarnings(tempest_stage_time_parse(value))
  if (length(parsed) != 1L || is.na(parsed)) {
    return(NULL)
  }
  fraction <- if (nchar(value) == 20L) {
    ""
  } else {
    substr(value, 21L, nchar(value) - 1L)
  }
  paste0(
    substr(value, 1L, 19L),
    ".",
    fraction,
    strrep("0", 6L - nchar(fraction)),
    "Z"
  )
}

tempest_graft_plan_posix_value <- function(value) {
  result <- rep(NA_character_, length(value))
  value <- tryCatch(
    suppressWarnings(as.POSIXct(value)),
    error = \(error) NULL
  )
  if (is.null(value) || length(value) != length(result)) {
    return(result)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  valid <- !is.na(numeric_value) & is.finite(numeric_value)
  indices <- which(valid)
  if (length(indices) == 0L) {
    return(result)
  }
  microseconds <- round(numeric_value[indices] * 1e6)
  seconds <- floor(microseconds / 1e6)
  fractions <- microseconds - seconds * 1e6
  finite <- is.finite(microseconds) &
    is.finite(seconds) &
    is.finite(fractions) &
    fractions >= 0 &
    fractions < 1e6
  indices <- indices[finite]
  seconds <- seconds[finite]
  fractions <- fractions[finite]
  if (length(indices) == 0L) {
    return(result)
  }
  rendered <- tryCatch(
    suppressWarnings(format(
      as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )),
    error = \(error) rep(NA_character_, length(seconds))
  )
  candidates <- paste0(
    rendered,
    ".",
    sprintf("%06d", as.integer(fractions)),
    "Z"
  )
  result[indices] <- vapply(
    candidates,
    function(candidate) {
      canonical <- tempest_graft_plan_timestamp_value(candidate)
      if (is.null(canonical) || !identical(canonical, candidate)) {
        return(NA_character_)
      }
      canonical
    },
    character(1)
  )
  result
}

tempest_graft_plan_value <- function(value) {
  if (
    is.null(value) ||
      length(value) == 0L ||
      (length(value) == 1L && is.atomic(value) && is.na(value))
  ) {
    return(NULL)
  }
  if (inherits(value, "POSIXt")) {
    return(tempest_graft_plan_posix_value(value))
  }
  timestamp <- tempest_graft_plan_timestamp_value(value)
  if (!is.null(timestamp)) {
    return(timestamp)
  }
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (is.list(value)) {
    return(lapply(value, tempest_graft_plan_value))
  }
  unname(value)
}

tempest_graft_plan_row <- function(data, index, fields = names(data)) {
  stats::setNames(
    lapply(fields, function(field) {
      tempest_graft_plan_value(data[[field]][[index]])
    }),
    fields
  )
}

tempest_graft_expected_plan_value <- function(value, observed) {
  value <- tempest_graft_plan_value(value)
  if (is.null(value)) {
    return(NULL)
  }
  if (is.character(observed) && is.numeric(value)) {
    return(as.character(value))
  }
  if (is.numeric(observed) && is.character(value)) {
    numeric_value <- suppressWarnings(as.double(value))
    if (
      length(numeric_value) == 1L &&
        !is.na(numeric_value) &&
        is.finite(numeric_value)
    ) {
      if (is.integer(observed)) {
        return(as.integer(numeric_value))
      }
      return(numeric_value)
    }
  }
  if (is.integer(observed) && is.numeric(value)) {
    return(as.integer(value))
  }
  value
}

tempest_graft_bundle_plan_match <- function(
  bundle_rows,
  planned,
  fields,
  id_field
) {
  planned_ids <- as.character(planned[[id_field]])
  expected_ids <- vapply(bundle_rows, `[[`, character(1), id_field)
  if (!setequal(planned_ids, expected_ids)) {
    return(FALSE)
  }
  for (row in bundle_rows) {
    index <- match(row[[id_field]], planned_ids)
    observed <- tempest_graft_plan_row(planned, index, fields)
    expected <- stats::setNames(
      Map(
        tempest_graft_expected_plan_value,
        row[fields],
        observed
      ),
      fields
    )
    if (
      !identical(
        tempest_promotion_digest(expected),
        tempest_promotion_digest(observed)
      )
    ) {
      return(FALSE)
    }
  }
  TRUE
}

tempest_graft_plan_assert_bundle <- function(plan, bundle) {
  if (!S7::S7_inherits(bundle, TempestPromotionBundle)) {
    tempest_graft_plan_abort(
      "{.arg bundle} must be a TempestPromotionBundle."
    )
  }
  S7::validate(bundle)
  plan <- tempest_graft_plan_require_valid(plan, "final")
  metadata <- plan@provenance@metadata
  expected_provenance <- tempest_graft_plan_provenance(
    bundle,
    metadata$planning_snapshot_id %||% NULL,
    plan@store_id
  )
  if (
    !identical(plan@schema_build_digest, bundle@schema_build_digest) ||
      !identical(plan@provenance@producer, expected_provenance@producer) ||
      !identical(plan@provenance@version, expected_provenance@version) ||
      !identical(plan@provenance@run_id, expected_provenance@run_id) ||
      !identical(
        plan@provenance@idempotency_key,
        expected_provenance@idempotency_key
      ) ||
      !identical(metadata, expected_provenance@metadata) ||
      !identical(names(plan@records), names(bundle@records))
  ) {
    tempest_graft_plan_abort(
      "The Graft plan is not bound to this exact Tempest promotion bundle."
    )
  }

  coalesced <- tempest_graft_coalesce_bundle_rows(bundle@records)
  bundle_records <- coalesced$records
  claim_alias <- coalesced$alias
  source_map <- stats::setNames(
    as.character(plan@records$Source$id),
    as.character(plan@records$Source$tempest_source_id)
  )
  claim_map <- stats::setNames(
    as.character(plan@records$Claim$id),
    as.character(plan@records$Claim$tempest_claim_id)
  )
  simple_classes <- c("Source", "Claim", "ProgramArtifact")
  for (record_class in simple_classes) {
    fields <- tempest_promotion_record_fields()[[record_class]]
    if (
      !tempest_graft_bundle_plan_match(
        bundle_records[[record_class]],
        plan@records[[record_class]],
        fields,
        tempest_promotion_record_id_field(record_class)
      )
    ) {
      tempest_graft_plan_abort(
        "The planned {.val {record_class}} rows differ from the promotion bundle."
      )
    }
  }

  span_fields <- setdiff(
    tempest_promotion_record_fields()$EvidenceSpan,
    "source_id"
  )
  if (
    !tempest_graft_bundle_plan_match(
      bundle_records$EvidenceSpan,
      plan@records$EvidenceSpan,
      span_fields,
      "id"
    )
  ) {
    tempest_graft_plan_abort(
      "The planned EvidenceSpan rows differ from the promotion bundle."
    )
  }
  span_sources <- stats::setNames(
    as.character(plan@records$EvidenceSpan$source_id),
    as.character(plan@records$EvidenceSpan$id)
  )
  for (row in bundle_records$EvidenceSpan) {
    if (!identical(span_sources[[row$id]], source_map[[row$source_id]])) {
      tempest_graft_plan_abort(
        "A planned EvidenceSpan does not resolve its exact Source identity."
      )
    }
  }

  support_fields <- setdiff(
    tempest_promotion_record_fields()$ClaimSupport,
    c("statement_id", "source_id")
  )
  if (
    !tempest_graft_bundle_plan_match(
      bundle_records$ClaimSupport,
      plan@records$ClaimSupport,
      support_fields,
      "tempest_claim_support_id"
    )
  ) {
    tempest_graft_plan_abort(
      "The planned ClaimSupport rows differ from the promotion bundle."
    )
  }
  planned_support <- plan@records$ClaimSupport
  support_index <- match(
    vapply(
      bundle_records$ClaimSupport,
      `[[`,
      character(1),
      "tempest_claim_support_id"
    ),
    as.character(planned_support$tempest_claim_support_id)
  )
  for (index in seq_along(support_index)) {
    row <- bundle_records$ClaimSupport[[index]]
    planned_index <- support_index[[index]]
    if (
      !identical(
        as.character(planned_support$statement_id[[planned_index]]),
        claim_map[[claim_alias[[row$tempest_claim_id]]]]
      ) ||
        !identical(
          as.character(planned_support$source_id[[planned_index]]),
          source_map[[row$source_id]]
        )
    ) {
      tempest_graft_plan_abort(
        "A planned ClaimSupport does not resolve its exact Claim and Source."
      )
    }
  }
  invisible(plan)
}

#' Plan a Tempest research promotion without accepting it
#'
#' This adapter performs a deterministic read-only seed plan for Source and
#' Claim identities, rewrites typed references, and returns only the final
#' reviewable `graft::GraftCommitPlan`. Neither plan is committed.
#'
#' @param store A writable or read-only `graft::GraftStore` opened with
#'   [tempest_graft_schema()].
#' @param bundle A [tempest_promotion_bundle()] proposal.
#' @return The final valid `graft::GraftCommitPlan` for explicit host review.
#' @export
tempest_graft_plan <- function(store, bundle) {
  store <- tempest_graft_store_validate(store)
  if (!S7::S7_inherits(bundle, TempestPromotionBundle)) {
    tempest_graft_plan_abort(
      "{.arg bundle} must be created by tempest_promotion_bundle()."
    )
  }
  S7::validate(bundle)
  schema <- store@schema
  coalesced <- tempest_graft_coalesce_bundle_rows(bundle@records)
  records <- tempest_graft_bundle_records(coalesced$records, schema)
  planning_snapshot <- tryCatch(
    tempest_graft_snapshot_call(store),
    error = function(error) {
      tempest_graft_plan_abort(
        "Could not capture the Graft planning snapshot."
      )
    }
  )
  if (
    !inherits(planning_snapshot, "graft::GraftSnapshot") ||
      !identical(planning_snapshot@store_id, store@id) ||
      !identical(
        planning_snapshot@schema_build_digest,
        bundle@schema_build_digest
      )
  ) {
    tempest_graft_plan_abort(
      "The Graft planning snapshot does not match the promotion store."
    )
  }
  provenance <- tempest_graft_plan_provenance(
    bundle,
    planning_snapshot@snapshot_id,
    planning_snapshot@store_id
  )

  records$Claim$.graft_origin_key <- tempest_claim_origin_keys(
    records$Claim$statement_text
  )
  records$Source$.graft_origin_key <- tempest_source_origin_keys(
    records$Source$locator
  )
  seed <- tryCatch(
    tempest_graft_plan_call(
      store,
      records = records[c("Source", "Claim")],
      provenance = provenance
    ),
    error = function(error) {
      tempest_graft_plan_abort(
        "Graft Source and Claim identity planning failed."
      )
    }
  )
  seed <- tempest_graft_plan_require_valid(seed, "identity seed")
  tempest_graft_assert_claim_lifecycle(store, seed)
  source_map <- tempest_graft_seed_map(seed, "Source", "tempest_source_id")
  claim_map <- tempest_graft_seed_map(seed, "Claim", "tempest_claim_id")

  records$Source$id <- unname(source_map[records$Source$tempest_source_id])
  records$Claim$id <- unname(claim_map[records$Claim$tempest_claim_id])
  records$EvidenceSpan$source_id <- unname(
    source_map[records$EvidenceSpan$source_id]
  )
  records$ClaimSupport$statement_id <- unname(
    claim_map[unname(coalesced$alias[records$ClaimSupport$tempest_claim_id])]
  )
  records$ClaimSupport$source_id <- unname(
    source_map[records$ClaimSupport$source_id]
  )
  if (
    anyNA(records$EvidenceSpan$source_id) ||
      anyNA(records$ClaimSupport$statement_id) ||
      anyNA(records$ClaimSupport$source_id)
  ) {
    tempest_graft_plan_abort(
      "The seed plan did not resolve every typed promotion reference."
    )
  }

  plan <- tryCatch(
    tempest_graft_plan_call(store, records = records, provenance = provenance),
    error = function(error) {
      tempest_graft_plan_abort(
        "Final Tempest promotion planning failed."
      )
    }
  )
  plan <- tempest_graft_plan_require_valid(plan, "final")
  observed_snapshot <- tryCatch(
    tempest_graft_snapshot_call(store),
    error = function(error) {
      tempest_graft_plan_abort(
        "Could not verify the Graft planning snapshot."
      )
    }
  )
  if (
    !identical(
      tempest_graft_snapshot_data(observed_snapshot),
      tempest_graft_snapshot_data(planning_snapshot)
    )
  ) {
    tempest_graft_plan_abort(
      "The Graft store advanced while the promotion plan was prepared."
    )
  }
  tempest_graft_plan_assert_bundle(plan, bundle)
  plan
}


# Accepted Claim identity is keyed on the normalized statement text rather
# than on the run-scoped Tempest claim id, so a claim that later research
# re-verifies resolves to the record already accepted instead of duplicating
# it. Repeated text inside one bundle is coalesced before planning.
tempest_claim_text_key <- function(text) {
  normalized <- stringi::stri_trans_tolower(
    tempest_trim(as.character(text)),
    locale = "root"
  )
  normalized <- gsub("[[:space:]]+", " ", normalized, perl = TRUE)
  normalized <- sub("[.]$", "", normalized, perl = TRUE)
  enc2utf8(normalized)
}

# Accepted Sources are keyed on their exact locator so the same document cited
# by later research resolves to its accepted record. Repeated locators inside
# one bundle are coalesced before planning.
tempest_source_origin_keys <- function(locators) {
  if (length(locators) == 0L) {
    return(character())
  }
  vapply(
    tempest_trim(as.character(locators)),
    function(locator) {
      paste0(
        "tempest-source-locator-v1:",
        digest::digest(enc2utf8(locator), algo = "sha256", serialize = FALSE)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

tempest_claim_origin_keys <- function(texts) {
  if (length(texts) == 0L) {
    return(character())
  }
  vapply(
    tempest_claim_text_key(texts),
    function(key) {
      paste0(
        "tempest-claim-text-v1:",
        digest::digest(key, algo = "sha256", serialize = FALSE)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

tempest_graft_named_counts <- function(value) {
  value <- value[order(names(value), method = "radix")]
  stats::setNames(as.integer(value), names(value))
}

tempest_graft_expected_counts <- function(plan) {
  classes <- sort(unique(as.character(plan@changes$class)), method = "radix")
  count <- function(action) {
    stats::setNames(
      vapply(
        classes,
        function(record_class) {
          sum(
            plan@changes$class == record_class &
              plan@changes$action == action
          )
        },
        integer(1)
      ),
      classes
    )
  }
  list(
    inserted = count("insert"),
    updated = count("update"),
    matched = count("match"),
    observed = stats::setNames(
      vapply(
        classes,
        function(record_class) {
          sum(plan@changes$class == record_class)
        },
        integer(1)
      ),
      classes
    )
  )
}

tempest_graft_snapshot_data <- function(snapshot) {
  fields <- c(
    "schema_version",
    "snapshot_id",
    "store_id",
    "store_format_version",
    "schema_build_digest",
    "commit_order",
    "batch_id",
    "committed_at",
    "history_complete"
  )
  stats::setNames(
    lapply(fields, function(field) S7::prop(snapshot, field)),
    fields
  )
}

tempest_graft_record_matches <- function(expected, observed) {
  if (!setequal(names(expected), names(observed))) {
    return(FALSE)
  }
  fields <- setdiff(names(expected), c("created_at", "updated_at"))
  observed <- stats::setNames(
    lapply(observed[fields], tempest_graft_plan_value),
    fields
  )
  expected <- stats::setNames(
    Map(
      tempest_graft_expected_plan_value,
      expected[fields],
      observed
    ),
    fields
  )
  identical(
    tempest_promotion_digest(expected),
    tempest_promotion_digest(observed)
  )
}

tempest_graft_counts_data <- function(counts) {
  lapply(counts, function(value) {
    value <- tempest_graft_named_counts(value)
    stats::setNames(as.list(unname(value)), names(value))
  })
}

tempest_graft_receipt_content_digest <- function(history, planned) {
  if (
    !rlang::is_string(planned) ||
      is.na(planned) ||
      !grepl("^sha256:[a-f0-9]{64}$", planned)
  ) {
    tempest_promotion_receipt_abort(
      "The reviewed plan has an invalid proposed content digest."
    )
  }
  if (!"content_digest" %in% names(history)) {
    # Graft 81bd3f8 validates ledger content digests while hydrating history,
    # then omits the digest column from its public result. Exact record equality
    # below therefore proves that this planned digest is the validated digest.
    return(planned)
  }
  if (
    nrow(history) != 1L ||
      !identical(as.character(history$content_digest[[1L]]), planned)
  ) {
    tempest_promotion_receipt_abort(
      "An accepted Graft content digest differs from the reviewed plan."
    )
  }
  as.character(history$content_digest[[1L]])
}

#' Record exact accepted revisions for a committed promotion plan
#'
#' `tempest_promotion_receipt()` verifies the commit summary, captures the
#' immediate immutable Graft snapshot, reopens it through `graft_at()`, and
#' checks every planned record and current revision before returning a receipt.
#'
#' @param store The open Graft store used for the commit.
#' @param bundle The exact Tempest promotion bundle.
#' @param plan The reviewed plan returned by [tempest_graft_plan()].
#' @param commit_result The ordinary list returned by `graft::graft_commit()`.
#' @return A validated `TempestPromotionReceipt`.
#' @export
tempest_promotion_receipt <- function(store, bundle, plan, commit_result) {
  store <- tempest_graft_store_validate(store)
  tempest_graft_plan_assert_bundle(plan, bundle)
  if (!is.list(commit_result) || is.data.frame(commit_result)) {
    tempest_promotion_receipt_abort(
      "{.arg commit_result} must be returned by graft::graft_commit()."
    )
  }
  if (
    !identical(commit_result$batch_id, plan@plan_id) ||
      !identical(plan@store_id, store@id) ||
      !identical(plan@schema_build_digest, bundle@schema_build_digest)
  ) {
    tempest_promotion_receipt_abort(
      "The commit result, plan, store, and promotion bundle are not identical."
    )
  }
  expected_counts <- tempest_graft_expected_counts(plan)
  for (field in names(expected_counts)) {
    observed <- tempest_graft_named_counts(commit_result[[field]])
    expected <- tempest_graft_named_counts(expected_counts[[field]])
    if (!identical(observed, expected)) {
      tempest_promotion_receipt_abort(
        "The commit result has incorrect {.field {field}} counts."
      )
    }
  }

  snapshot <- tryCatch(
    tempest_graft_snapshot_call(store),
    error = function(error) {
      tempest_promotion_receipt_abort(
        "Could not capture the committed Graft snapshot."
      )
    }
  )
  if (
    !identical(snapshot@batch_id, commit_result$batch_id) ||
      !identical(snapshot@store_id, store@id) ||
      !identical(snapshot@schema_build_digest, bundle@schema_build_digest)
  ) {
    tempest_promotion_receipt_abort(
      paste0(
        "The store advanced before the promotion receipt could bind the ",
        "committed snapshot."
      )
    )
  }
  view <- tryCatch(
    graft::graft_at(store, snapshot),
    error = function(error) {
      tempest_promotion_receipt_abort(
        "Could not reopen the committed immutable Graft snapshot."
      )
    }
  )
  recovered <- tryCatch(
    graft::graft_view_snapshot(view),
    error = function(error) {
      tempest_promotion_receipt_abort(
        "Could not verify the committed immutable Graft snapshot."
      )
    }
  )
  if (
    !identical(
      tempest_graft_snapshot_data(recovered),
      tempest_graft_snapshot_data(snapshot)
    )
  ) {
    tempest_promotion_receipt_abort(
      "The immutable Graft view did not retain the exact captured snapshot."
    )
  }

  revisions <- list()
  cursor <- 0L
  for (record_class in names(plan@records)) {
    data <- plan@records[[record_class]]
    for (index in seq_len(nrow(data))) {
      id <- as.character(data$id[[index]])
      expected <- tempest_graft_plan_row(data, index)
      current <- tryCatch(
        graft::graft_get(view, id, include = character()),
        error = function(error) {
          tempest_promotion_receipt_abort(
            "Could not read an accepted Graft record for the receipt."
          )
        }
      )
      if (!identical(current$class, record_class)) {
        tempest_promotion_receipt_abort(
          "An accepted Graft record has the wrong class."
        )
      }
      history <- tryCatch(
        graft::graft_history(view, id, limit = 1L),
        error = function(error) {
          tempest_promotion_receipt_abort(
            "Could not read accepted Graft history for the receipt."
          )
        }
      )
      change_index <- which(
        plan@changes$class == record_class &
          plan@changes$record_id == id
      )
      planned_content_digest <- if (length(change_index) == 1L) {
        as.character(plan@changes$proposed_content_digest[[change_index]])
      } else {
        NA_character_
      }
      actual_content_digest <- tempest_graft_receipt_content_digest(
        history,
        planned_content_digest
      )
      if (
        length(change_index) != 1L ||
          nrow(history) != 1L ||
          !identical(as.character(history$class[[1L]]), record_class) ||
          !identical(actual_content_digest, planned_content_digest) ||
          !tempest_graft_record_matches(expected, current$record) ||
          !tempest_graft_record_matches(
            expected,
            history$record[[1L]]
          )
      ) {
        tempest_promotion_receipt_abort(
          "An accepted Graft revision differs from the reviewed plan."
        )
      }
      cursor <- cursor + 1L
      revisions[[cursor]] <- list(
        class = record_class,
        record_id = id,
        revision_id = as.character(history$revision_id[[1L]]),
        revision_number = as.integer(history$revision_number[[1L]]),
        action = as.character(plan@changes$action[[change_index]]),
        batch_id = as.character(history$batch_id[[1L]]),
        content_digest = actual_content_digest,
        schema_build_digest = as.character(
          history$schema_build_digest[[1L]]
        )
      )
    }
  }
  revisions <- revisions[order(
    vapply(
      revisions,
      function(value) {
        paste(value$class, value$record_id, sep = "\u001f")
      },
      character(1)
    ),
    method = "radix"
  )]
  snapshot_data <- tempest_graft_snapshot_data(snapshot)
  counts_data <- tempest_graft_counts_data(expected_counts)
  payload <- tempest_promotion_receipt_payload(
    bundle@bundle_id,
    plan@plan_id,
    plan@plan_digest,
    commit_result$batch_id,
    store@id,
    plan@schema_build_digest,
    snapshot_data,
    counts_data,
    revisions
  )
  TempestPromotionReceipt(
    schema_version = tempest_promotion_schema_version,
    receipt_id = tempest_promotion_digest(payload),
    bundle_id = bundle@bundle_id,
    plan_id = plan@plan_id,
    plan_digest = plan@plan_digest,
    batch_id = commit_result$batch_id,
    store_id = store@id,
    schema_build_digest = plan@schema_build_digest,
    snapshot = snapshot_data,
    counts = counts_data,
    record_revisions = revisions
  )
}
