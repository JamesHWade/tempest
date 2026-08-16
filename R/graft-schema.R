# Tempest's compiled Graft research contract and review-only planning adapter

tempest_graft_accessor_commit <-
  "81bd3f83a3c8ee2bee22b61ff09b475f58b4f0e5"

tempest_graft_behavior_digest <-
  "sha256:687520a6d3bfc963e8c5b9dc090a759349747d87d398cb2c14a5972e35c01b21"

tempest_graft_required_exports <- function() {
  c(
    "graft_at",
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

tempest_graft_remote_sha <- function() {
  utils::packageDescription("graft")[["RemoteSha"]]
}

tempest_graft_function_descriptor <- function(value) {
  list(
    formals = paste(
      deparse(formals(value), width.cutoff = 500L, control = "all"),
      collapse = "\n"
    ),
    body = paste(
      deparse(body(value), width.cutoff = 500L, control = "all"),
      collapse = "\n"
    )
  )
}

tempest_graft_property_descriptor <- function(property) {
  data <- unclass(property)
  property_class <- data$class
  class_data <- unclass(property_class)
  list(
    name = data$name,
    class_kind = class(property_class),
    class_value = if (is.null(class_data$class)) {
      character()
    } else {
      class_data$class
    },
    getter = if (is.function(data$getter)) {
      tempest_graft_function_descriptor(data$getter)
    } else {
      NULL
    },
    setter = if (is.function(data$setter)) {
      tempest_graft_function_descriptor(data$setter)
    } else {
      NULL
    },
    validator = if (is.function(data$validator)) {
      tempest_graft_function_descriptor(data$validator)
    } else {
      NULL
    },
    default = paste(capture.output(dput(data$default)), collapse = "\n")
  )
}

tempest_graft_class_descriptor <- function(value) {
  data <- attributes(value)
  list(
    name = data$name,
    package = data$package,
    abstract = data$abstract,
    parent = class(data$parent),
    constructor = tempest_graft_function_descriptor(data$constructor),
    validator = tempest_graft_function_descriptor(data$validator),
    properties = stats::setNames(
      lapply(data$properties, tempest_graft_property_descriptor),
      names(data$properties)
    )
  )
}

tempest_graft_plain_constant <- function(value) {
  if (is.function(value) || is.environment(value) || is.object(value)) {
    return(FALSE)
  }
  if (is.list(value)) {
    return(all(vapply(value, tempest_graft_plain_constant, logical(1))))
  }
  is.atomic(value)
}

tempest_graft_behavior_fingerprint <- function() {
  namespace <- asNamespace("graft")
  object_names <- sort(ls(namespace, all.names = TRUE), method = "radix")
  function_names <- Filter(
    function(name) {
      value <- get(name, envir = namespace, inherits = FALSE)
      is.function(value) && identical(environment(value), namespace)
    },
    object_names
  )
  class_names <- Filter(
    function(name) {
      inherits(
        get(name, envir = namespace, inherits = FALSE),
        "S7_class"
      )
    },
    function_names
  )
  constant_names <- Filter(
    function(name) {
      tempest_graft_plain_constant(
        get(name, envir = namespace, inherits = FALSE)
      )
    },
    object_names
  )
  payload <- list(
    exports = sort(getNamespaceExports("graft"), method = "radix"),
    functions = stats::setNames(
      lapply(function_names, function(name) {
        tempest_graft_function_descriptor(
          get(name, envir = namespace, inherits = FALSE)
        )
      }),
      function_names
    ),
    classes = stats::setNames(
      lapply(class_names, function(name) {
        tempest_graft_class_descriptor(
          get(name, envir = namespace, inherits = FALSE)
        )
      }),
      class_names
    ),
    constants = stats::setNames(
      lapply(constant_names, function(name) {
        paste(
          capture.output(dput(get(
            name,
            envir = namespace,
            inherits = FALSE
          ))),
          collapse = "\n"
        )
      }),
      constant_names
    )
  )
  paste0(
    "sha256:",
    digest::digest(payload, algo = "sha256", serialize = TRUE)
  )
}

tempest_graft_pin_valid <- function(remote_sha) {
  if (!is.null(remote_sha)) {
    return(identical(remote_sha, tempest_graft_accessor_commit))
  }
  identical(
    tempest_graft_behavior_fingerprint(),
    tempest_graft_behavior_digest
  )
}

tempest_graft_require <- function() {
  if (!requireNamespace("graft", quietly = TRUE)) {
    tempest_promotion_abort(
      paste0(
        "Graft at commit ",
        tempest_graft_accessor_commit,
        " is required for research promotion."
      ),
      class = "tempest_graft_schema_error"
    )
  }
  remote_sha <- tryCatch(
    tempest_graft_remote_sha(),
    error = function(error) {
      tempest_promotion_abort(
        "Could not verify the installed Graft package identity.",
        class = "tempest_graft_schema_error"
      )
    }
  )
  pin_valid <- tryCatch(
    tempest_graft_pin_valid(remote_sha),
    error = function(error) FALSE
  )
  if (!pin_valid) {
    tempest_promotion_abort(
      paste0(
        "The installed Graft package does not match approved accessor commit ",
        tempest_graft_accessor_commit,
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
#' The packaged contract is compiled against Graft accessor commit
#' `81bd3f83a3c8ee2bee22b61ff09b475f58b4f0e5`. Runtime loading never compiles
#' LinkML and rejects any manifest whose immutable build digest differs.
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
          "Source"
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

tempest_graft_bundle_records <- function(bundle, schema) {
  stats::setNames(
    lapply(names(bundle@records), function(record_class) {
      tempest_graft_records_data_frame(
        bundle@records[[record_class]],
        schema,
        record_class
      )
    }),
    names(bundle@records)
  )
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
    getFromNamespace("validate_graft_commit_plan", "graft")(plan),
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
      graft_accessor_commit = tempest_graft_accessor_commit,
      promotion_bundle_id = bundle@bundle_id,
      research_manifest_digest = tempest_promotion_digest(
        bundle@research_manifest
      ),
      schema_build_digest = bundle@schema_build_digest,
      planning_snapshot_id = planning_snapshot_id
    )
  )
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
    return(format(
      as.POSIXct(value, tz = "UTC"),
      "%Y-%m-%dT%H:%M:%OS6Z",
      tz = "UTC"
    ))
  }
  if (
    rlang::is_string(value) &&
      !is.na(value) &&
      grepl(
        paste0(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:",
          "[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?Z$"
        ),
        value
      )
  ) {
    parsed <- suppressWarnings(tempest_stage_time_parse(value))
    if (!is.na(parsed)) {
      return(format(
        parsed,
        "%Y-%m-%dT%H:%M:%OS6Z",
        tz = "UTC"
      ))
    }
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
        bundle@records[[record_class]],
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
      bundle@records$EvidenceSpan,
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
  for (row in bundle@records$EvidenceSpan) {
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
      bundle@records$ClaimSupport,
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
      bundle@records$ClaimSupport,
      `[[`,
      character(1),
      "tempest_claim_support_id"
    ),
    as.character(planned_support$tempest_claim_support_id)
  )
  for (index in seq_along(support_index)) {
    row <- bundle@records$ClaimSupport[[index]]
    planned_index <- support_index[[index]]
    if (
      !identical(
        as.character(planned_support$statement_id[[planned_index]]),
        claim_map[[row$tempest_claim_id]]
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
  records <- tempest_graft_bundle_records(bundle, schema)
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
  source_map <- tempest_graft_seed_map(seed, "Source", "tempest_source_id")
  claim_map <- tempest_graft_seed_map(seed, "Claim", "tempest_claim_id")

  records$Source$id <- unname(source_map[records$Source$tempest_source_id])
  records$Claim$id <- unname(claim_map[records$Claim$tempest_claim_id])
  records$EvidenceSpan$source_id <- unname(
    source_map[records$EvidenceSpan$source_id]
  )
  records$ClaimSupport$statement_id <- unname(
    claim_map[records$ClaimSupport$tempest_claim_id]
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
