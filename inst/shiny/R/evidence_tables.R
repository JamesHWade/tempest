# Evidence and source DT table builders.

table_missing_text <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

table_compact_text <- function(x, max_chars = 300L, missing = "Not available") {
  vapply(
    as.character(x),
    function(value) {
      if (length(value) == 0L || is.na(value) || !nzchar(trimws(value))) {
        return(missing)
      }
      value <- gsub("\\s+", " ", trimws(value), perl = TRUE)
      if (nchar(value) <= max_chars) {
        return(value)
      }
      paste0(substr(value, 1L, max_chars - 3L), "...")
    },
    character(1)
  )
}

sources_table_data <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  n <- nrow(df)
  if (!"context_text" %in% names(df)) {
    df$context_text <- rep(NA_character_, n)
  }
  if (!"snippet" %in% names(df)) {
    df$snippet <- rep(NA_character_, n)
  }

  context <- as.character(df$context_text)
  snippet <- as.character(df$snippet)
  content <- if ("content_text" %in% names(df)) {
    as.character(df$content_text)
  } else {
    rep(NA_character_, n)
  }

  missing_context <- table_missing_text(context)
  context[missing_context] <- content[missing_context]
  missing_context <- table_missing_text(context)
  context[missing_context] <- snippet[missing_context]

  missing_snippet <- table_missing_text(snippet)
  snippet[missing_snippet] <- table_compact_text(
    context[missing_snippet],
    missing = NA_character_
  )

  ids <- if ("id" %in% names(df)) {
    table_compact_text(df$id, max_chars = 80L)
  } else {
    rep("Not available", n)
  }
  urls <- if ("url" %in% names(df)) {
    as.character(df$url)
  } else {
    rep(NA_character_, n)
  }
  locators <- urls
  fallback_locators <- if ("locator" %in% names(df)) {
    as.character(df$locator)
  } else {
    rep(NA_character_, n)
  }
  missing_locator <- table_missing_text(locators)
  locators[missing_locator] <- fallback_locators[missing_locator]
  locations <- table_compact_text(locators, max_chars = 180L)

  titles <- if ("title" %in% names(df)) {
    as.character(df$title)
  } else {
    rep(NA_character_, n)
  }
  missing_title <- table_missing_text(titles)
  titles[missing_title] <- vapply(
    locations[missing_title],
    source_location_label,
    character(1)
  )
  missing_title <- table_missing_text(titles)
  titles[missing_title] <- ids[missing_title]

  kinds <- if ("resource_kind" %in% names(df)) {
    table_display_label(df$resource_kind)
  } else {
    rep("Not available", n)
  }
  fetched <- if ("fetched_at" %in% names(df)) {
    table_timestamp_text(df$fetched_at)
  } else {
    rep("Not available", n)
  }

  data.frame(
    Source = table_compact_text(titles, max_chars = 140L),
    Location = locations,
    `Evidence excerpt` = table_compact_text(
      table_plain_text(snippet),
      max_chars = 260L
    ),
    Type = kinds,
    Retrieved = fetched,
    `Source ID` = ids,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

facts_table_data <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  n <- nrow(df)
  source_ids <- if ("source_ids" %in% names(df)) {
    vapply(
      df$source_ids,
      function(ids) {
        ids <- as.character(unlist(ids, use.names = FALSE))
        ids <- ids[!table_missing_text(ids)]
        if (length(ids) == 0L) {
          return("Not linked")
        }
        paste0(
          length(ids),
          " linked · ",
          paste(ids, collapse = ", ")
        )
      },
      character(1)
    )
  } else {
    rep("Not linked", n)
  }
  score <- if ("support_score" %in% names(df)) {
    suppressWarnings(as.numeric(df$support_score))
  } else {
    rep(NA_real_, n)
  }
  support <- ifelse(
    is.na(score),
    "Not scored",
    paste0(round(score * 100), "%")
  )
  claim_text <- if ("claim_text" %in% names(df)) {
    table_compact_text(table_plain_text(df$claim_text), max_chars = 500L)
  } else {
    rep("Not available", n)
  }
  claim_type <- if ("claim_type" %in% names(df)) {
    table_display_label(df$claim_type)
  } else {
    rep("Not available", n)
  }
  confidence <- if ("confidence" %in% names(df)) {
    table_display_label(df$confidence)
  } else {
    rep("Not available", n)
  }
  status <- if ("verification_status" %in% names(df)) {
    table_display_label(df$verification_status)
  } else {
    rep("Not available", n)
  }

  data.frame(
    Fact = claim_text,
    Sources = source_ids,
    Confidence = confidence,
    Status = status,
    Support = support,
    Type = claim_type,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

table_display_label <- function(x) {
  vapply(
    as.character(x),
    function(value) {
      if (is.na(value) || !nzchar(trimws(value))) {
        return("Not available")
      }
      value <- gsub("[._-]+", " ", trimws(value), perl = TRUE)
      tools::toTitleCase(tolower(value))
    },
    character(1)
  )
}

table_timestamp_text <- function(x) {
  values <- table_compact_text(x, max_chars = 40L)
  values <- sub(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2})T",
    "\\1 ",
    values,
    perl = TRUE
  )
  sub("Z$", " UTC", values)
}

table_plain_text <- function(x) {
  values <- as.character(x)
  values <- gsub("\\*{1,3}|_{1,3}|`+", "", values, perl = TRUE)
  gsub("\\[([^]]+)\\]\\([^)]*\\)", "\\1", values, perl = TRUE)
}

source_location_label <- function(location) {
  if (is.na(location) || !nzchar(trimws(location))) {
    return("Not available")
  }
  safe <- citation_safe_url(location)
  if (nzchar(safe)) {
    return(sub("^https?://([^/]+).*$", "\\1", safe, perl = TRUE))
  }
  table_compact_text(location, max_chars = 64L)
}

source_table_links <- function(labels, locations, ids) {
  mapply(
    function(label, location, id) {
      label <- table_compact_text(label, max_chars = 140L)
      location_label <- source_location_label(location)
      escaped_label <- htmltools::htmlEscape(label)
      escaped_location <- htmltools::htmlEscape(location_label)
      safe <- citation_safe_url(location)
      title <- if (nzchar(safe)) {
        paste0(
          '<a class="tempest-source-title" href="',
          htmltools::htmlEscape(safe, attribute = TRUE),
          '" target="_blank" rel="noopener noreferrer">',
          escaped_label,
          '<span class="visually-hidden"> (opens in a new tab)</span></a>'
        )
      } else {
        paste0('<span class="tempest-source-title">', escaped_label, "</span>")
      }
      meta <- if (identical(location_label, "Not available")) {
        htmltools::htmlEscape(id)
      } else {
        escaped_location
      }
      paste0(
        '<div class="tempest-source-cell">',
        title,
        '<span class="tempest-source-location">',
        meta,
        "</span></div>"
      )
    },
    labels,
    locations,
    ids,
    USE.NAMES = FALSE
  )
}

evidence_status_badges <- function(values, palette) {
  palette <- match.arg(palette, c("confidence", "verification"))
  vapply(
    as.character(values),
    function(value) {
      key <- tolower(trimws(value %||% ""))
      tone <- if (identical(palette, "confidence")) {
        switch(
          key,
          high = "success",
          medium = "warning",
          low = "danger",
          "neutral"
        )
      } else {
        switch(
          key,
          supported = "success",
          verified = "success",
          disputed = "warning",
          pending = "warning",
          unsupported = "danger",
          rejected = "danger",
          "neutral"
        )
      }
      paste0(
        '<span class="tempest-evidence-badge tempest-evidence-badge-',
        tone,
        '">',
        htmltools::htmlEscape(value %||% "Not available"),
        "</span>"
      )
    },
    character(1)
  )
}

# A responsive DT datatable with the app's standard search and export options.
styled_datatable <- function(
  df,
  html_columns = character(),
  search_placeholder = "Search evidence",
  column_defs = list()
) {
  html_columns <- intersect(html_columns, names(df))
  escape <- if (length(html_columns) == 0L) {
    TRUE
  } else {
    setdiff(names(df), html_columns)
  }
  DT::datatable(
    df,
    escape = escape,
    extensions = c("Buttons", "Responsive"),
    options = list(
      dom = paste0(
        "<'tempest-table-toolbar'",
        "<'tempest-table-search'f>",
        "<'tempest-table-actions'B>>",
        "rt",
        "<'tempest-table-footer'ip>"
      ),
      buttons = list(
        list(extend = "csv", text = "Export CSV"),
        list(extend = "excel", text = "Export Excel")
      ),
      pageLength = 10,
      lengthChange = FALSE,
      responsive = TRUE,
      autoWidth = FALSE,
      orderClasses = FALSE,
      columnDefs = column_defs,
      language = list(
        search = "",
        searchPlaceholder = search_placeholder,
        info = "_START_–_END_ of _TOTAL_",
        infoEmpty = "No records",
        paginate = list(previous = "Previous", `next` = "Next")
      )
    ),
    rownames = FALSE,
    class = "compact hover row-border"
  )
}
