# Product-owned canonical hashing for provisional research evidence.

tempest_product_hash_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_product_hash_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_product_canonical_value <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    is.function(value) ||
      is.environment(value) ||
      typeof(value) %in% c("externalptr", "weakref")
  ) {
    tempest_product_hash_abort(
      paste0(
        "Product records cannot contain functions, environments, or ",
        "external pointers."
      )
    )
  }
  if (inherits(value, "S7_object")) {
    tempest_product_hash_abort(
      "S7 objects must be converted to explicit product records before hashing."
    )
  }
  if (is.object(value)) {
    tempest_product_hash_abort(
      "Product records must use plain JSON-compatible R values."
    )
  }
  if (is.list(value)) {
    value_names <- names(value)
    if (!is.null(value_names)) {
      if (
        anyNA(value_names) ||
          any(!nzchar(value_names)) ||
          anyDuplicated(value_names)
      ) {
        tempest_product_hash_abort(
          paste0(
            "Product lists must be either unnamed or fully and uniquely ",
            "named."
          )
        )
      }
      value <- value[order(value_names)]
    }
    return(lapply(value, tempest_product_canonical_value))
  }
  if (!is.atomic(value)) {
    tempest_product_hash_abort(
      "Unsupported product value type {.val {typeof(value)}}."
    )
  }
  if (is.complex(value) || is.raw(value)) {
    tempest_product_hash_abort(
      "Product records do not support complex or raw values."
    )
  }
  if (
    anyNA(value) ||
      (is.numeric(value) && any(!is.finite(value)))
  ) {
    tempest_product_hash_abort(
      "Product records cannot contain missing or non-finite values."
    )
  }
  if (!is.null(attributes(value))) {
    tempest_product_hash_abort(
      "Product atomic values cannot carry names, dimensions, or classes."
    )
  }
  unclass(value)
}

tempest_product_canonical_json <- function(value) {
  tryCatch(
    as.character(jsonlite::toJSON(
      tempest_product_canonical_value(value),
      auto_unbox = TRUE,
      null = "null",
      na = "string",
      digits = NA,
      pretty = FALSE,
      force = TRUE
    )),
    error = function(error) {
      if (inherits(error, "tempest_product_hash_error")) {
        stop(error)
      }
      tempest_product_hash_abort(
        "Could not encode a canonical product record.",
        parent = error
      )
    }
  )
}

tempest_product_content_media_type <- function(media_type) {
  if (
    !rlang::is_string(media_type) ||
      is.na(media_type) ||
      !nzchar(media_type) ||
      !identical(media_type, trimws(media_type)) ||
      !grepl("^[^[:space:]/]+/[^[:space:]/]+$", media_type)
  ) {
    tempest_product_hash_abort(
      "{.arg media_type} must be one exact non-empty IANA media type."
    )
  }
  media_type
}

tempest_product_content_bytes <- function(content, media_type) {
  media_type <- tempest_product_content_media_type(media_type)
  if (startsWith(media_type, "text/")) {
    if (
      !rlang::is_string(content) ||
        is.na(content) ||
        is.object(content) ||
        !is.null(attributes(content))
    ) {
      tempest_product_hash_abort(
        "Text research content must be one unclassed non-missing string."
      )
    }
    content <- enc2utf8(content)
    if (!isTRUE(validUTF8(content))) {
      tempest_product_hash_abort(
        "Text research content must be valid UTF-8."
      )
    }
    return(charToRaw(content))
  }
  json_media_type <- identical(media_type, "application/json") ||
    grepl("^application/[^/[:space:]]+\\+json$", media_type)
  if (json_media_type) {
    return(charToRaw(enc2utf8(tempest_product_canonical_json(content))))
  }
  tempest_product_hash_abort(
    "Inline research content does not support media type {.val {media_type}}."
  )
}

tempest_product_content_hash <- function(content, media_type) {
  digest::digest(
    tempest_product_content_bytes(content, media_type),
    algo = "sha256",
    serialize = FALSE
  )
}

tempest_product_record_hash <- function(value) {
  digest::digest(
    tempest_product_canonical_json(value),
    algo = "sha256",
    serialize = FALSE
  )
}
