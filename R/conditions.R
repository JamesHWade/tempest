# Public Tempest condition contract
#
# Every user-facing failure inherits `tempest_error` plus exactly one of the
# supported public categories. Internal subclasses stay available for package
# tests and diagnostics, and the original provider or package condition stays
# reachable through the ordinary parent chain.

tempest_public_condition_categories <- function() {
  c(
    "tempest_input_error",
    "tempest_execution_error",
    "tempest_persistence_error",
    "tempest_authority_error",
    "tempest_cancelled"
  )
}

tempest_condition_chain_sensitive <- function(error) {
  current <- error
  for (depth in seq_len(100L)) {
    call <- conditionCall(current)
    exposed <- c(
      conditionMessage(current),
      class(current),
      if (is.null(call)) character() else deparse(call, width.cutoff = 500L)
    )
    if (tempest_contract_sensitive_scalar(exposed)) {
      return(TRUE)
    }
    current <- current$parent %||% NULL
    if (is.null(current)) {
      return(FALSE)
    }
    if (!inherits(current, "condition")) {
      return(TRUE)
    }
  }
  TRUE
}

tempest_condition_safe_summary <- function(error) {
  classes <- class(error)
  exposed <- c(conditionMessage(error), classes)
  if (tempest_contract_sensitive_scalar(exposed)) {
    safe_classes <- classes[
      grepl("^[A-Za-z][A-Za-z0-9_.-]*$", classes) &
        !vapply(classes, tempest_contract_sensitive_scalar, logical(1))
    ]
    return(rlang::error_cnd(
      "tempest_redacted_cause",
      message = "Underlying failure details were redacted.",
      cause_classes = unname(safe_classes)
    ))
  }
  custom_classes <- setdiff(classes, c("error", "condition"))
  if (length(custom_classes) == 0L) {
    custom_classes <- "tempest_external_cause"
  }
  rlang::error_cnd(
    custom_classes,
    message = conditionMessage(error),
    cause_classes = unname(classes)
  )
}

# Ordered rules. The first pattern matching any class in the condition's class
# vector selects the public category, so the more specific families are listed
# before the broader ones.
tempest_public_condition_rules <- function() {
  list(
    list(
      category = "tempest_cancelled",
      pattern = "_cancelled$"
    ),
    list(
      category = "tempest_authority_error",
      pattern = paste0(
        "authority|promotion|governed_procedure|graft_|stage_governance|",
        "verification_error|provenance"
      )
    ),
    list(
      category = "tempest_persistence_error",
      pattern = paste0(
        "persistence|restore|resume|snapshot|_save_error|archive|",
        "unsupported_format|integrity|codec|cache"
      )
    ),
    list(
      category = "tempest_input_error",
      pattern = paste0(
        "config|validation|_contract_error|missing_envvar|missing_package|",
        "record_error|identity_error|_id_error|_url_error|input"
      )
    ),
    list(
      category = "tempest_execution_error",
      pattern = ".*"
    )
  )
}

# Resolve the public category for one condition class vector.
tempest_public_condition_category <- function(classes) {
  classes <- classes[nzchar(classes)]
  existing <- intersect(classes, tempest_public_condition_categories())
  if (length(existing) > 0L) {
    return(existing[[1L]])
  }
  candidates <- grep("^tempest_", classes, value = TRUE)
  if (length(candidates) == 0L) {
    return("tempest_execution_error")
  }
  for (rule in tempest_public_condition_rules()) {
    if (any(grepl(rule$pattern, candidates, perl = TRUE))) {
      return(rule$category)
    }
  }
  "tempest_execution_error"
}

# Build the exact class vector for a user-facing Tempest condition: the
# internal subclasses, then exactly one public category, then `tempest_error`.
tempest_condition_class <- function(class = character()) {
  class <- unique(class[nzchar(class)])
  category <- tempest_public_condition_category(class)
  internal <- setdiff(
    class,
    c(tempest_public_condition_categories(), "tempest_error")
  )
  c(internal, category, "tempest_error")
}
