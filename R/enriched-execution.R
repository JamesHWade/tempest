tempest_deputy_offline_chat <- function() {
  ellmer::chat_openai(
    model = "gpt-4o-mini",
    credentials = \() "offline-only-not-used"
  )
}

tempest_enriched_execution_abort <- function(message, class, ...) {
  tempest_abort(message, ..., class = class)
}

tempest_enriched_required_string <- function(value, name, class) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value)) ||
      nchar(value, type = "bytes") > 200000L
  ) {
    tempest_enriched_execution_abort(
      paste0("`", name, "` must be one bounded non-empty string."),
      class = class
    )
  }
  value
}

tempest_enriched_await_promise <- function(promise, session, timeout) {
  tempest_await_promise(
    promise,
    timeout_s = timeout,
    poll_s = 0.05,
    sleep_s = 0,
    on_timeout = function() {
      session$cancel()
      tempest_enriched_execution_abort(
        "Deputy R execution did not settle before its deadline.",
        class = "tempest_execution_error",
        deputy_outcome = "await_timed_out"
      )
    },
    on_error = function(error) {
      tempest_enriched_execution_abort(
        "Deputy R execution could not be completed.",
        class = "tempest_execution_error",
        deputy_outcome = "promise_error"
      )
    }
  )
}

tempest_enriched_directory_files <- function(directory) {
  files <- tempest_enriched_inventory(directory)
  if (!tempest_enriched_relative(files)) {
    tempest_enriched_execution_abort(
      "An analysis output path is not safely relative.",
      class = "tempest_deputy_execution_error",
      deputy_outcome = "unsafe_output_path"
    )
  }
  files
}

tempest_enriched_file_snapshot <- function(directory) {
  files <- tempest_enriched_directory_files(directory)
  if (!length(files)) {
    return(stats::setNames(list(), character()))
  }

  paths <- file.path(directory, files)
  info <- file.info(paths)
  stats::setNames(
    Map(
      function(size, mtime) list(size = size, mtime = as.numeric(mtime)),
      info$size,
      info$mtime
    ),
    files
  )
}

tempest_enriched_changed_files <- function(before, after) {
  if (!length(after)) {
    return(character())
  }

  current <- tempest_enriched_file_snapshot(after)
  current_names <- names(current)
  changed <- vapply(
    current_names,
    function(path) {
      previous <- before[[path]]
      is.null(previous) ||
        !identical(previous$size, current[[path]]$size) ||
        !identical(previous$mtime, current[[path]]$mtime)
    },
    logical(1)
  )
  current_names[changed]
}

tempest_enriched_plot_files <- function(segments, directory) {
  plots <- Filter(
    function(segment) identical(segment$type, "plot"),
    segments
  )
  if (!length(plots)) {
    return(character())
  }

  plot_directory <- file.path(directory, "plots")
  if (fs::is_link(plot_directory)) {
    tempest_enriched_execution_abort(
      "The analysis plot directory cannot be a symbolic link.",
      class = "tempest_execution_error",
      deputy_outcome = "unsafe_plot_directory"
    )
  }
  if (
    !dir.exists(plot_directory) &&
      !dir.create(plot_directory, recursive = FALSE)
  ) {
    tempest_enriched_execution_abort(
      "The analysis plot directory could not be created.",
      class = "tempest_execution_error",
      deputy_outcome = "plot_write_failed"
    )
  }

  paths <- character(length(plots))
  for (i in seq_along(plots)) {
    bytes <- plots[[i]]$data
    if (
      !is.raw(bytes) ||
        length(bytes) < 8L ||
        !identical(
          bytes[seq_len(8L)],
          as.raw(c(
            137,
            80,
            78,
            71,
            13,
            10,
            26,
            10
          ))
        )
    ) {
      tempest_enriched_execution_abort(
        "Deputy returned an invalid plot image.",
        class = "tempest_execution_error",
        deputy_outcome = "invalid_plot"
      )
    }

    relative <- file.path("plots", sprintf("plot-%02d.png", i))
    path <- file.path(directory, relative)
    if (fs::file_exists(path) || fs::is_link(path)) {
      tempest_enriched_execution_abort(
        "An analysis output already occupies a Deputy plot path.",
        class = "tempest_execution_error",
        deputy_outcome = "plot_path_conflict"
      )
    }
    writeBin(bytes, path)
    paths[[i]] <- relative
  }

  gsub("\\\\", "/", paths)
}

tempest_enriched_execution_output <- function(record) {
  segments <- record$segments
  if (is.null(segments) || !length(segments)) {
    return("R analysis completed without text output.")
  }

  text <- vapply(
    segments,
    function(segment) {
      if (!segment$type %in% c("text", "message", "warning")) {
        return("")
      }
      prefix <- if (identical(segment$type, "text")) {
        ""
      } else {
        paste0(segment$type, ": ")
      }
      value <- segment$text
      if (is.null(value)) {
        value <- ""
      }
      paste0(prefix, value)
    },
    character(1)
  )
  text <- text[nzchar(text)]
  if (!length(text)) {
    return("R analysis completed without text output.")
  }
  paste(text, collapse = "\n")
}

#' Create a Deputy-backed R execution callback
#'
#' The callback runs each analysis in a fresh Deputy `RSession`, captures text,
#' warnings, plots, and files written under the task directory, and closes the
#' worker before returning. Code runs with the current user's local R access;
#' use this callback only for code execution that has been explicitly enabled.
#'
#' @param timeout Maximum time in seconds for each R evaluation.
#'
#' @return A callback with the `function(task, directory)` interface used by
#'   [tempest_enrich_report()]. It returns `output`, `files`, and `metadata`.
#' @export
tempest_r_executor <- function(timeout = 60) {
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      !is.finite(timeout) ||
      timeout <= 0
  ) {
    tempest_enriched_execution_abort(
      "`timeout` must be one positive, finite number of seconds.",
      class = "tempest_r_executor_config_error"
    )
  }
  timeout <- as.numeric(timeout)

  function(task, directory) {
    if (!is.list(task)) {
      tempest_enriched_execution_abort(
        "`task` must be an analysis task list.",
        class = "tempest_analysis_task_input_error",
        deputy_outcome = "invalid_task"
      )
    }
    code <- tempest_enriched_required_string(
      task[["code", exact = TRUE]],
      "task$code",
      "tempest_analysis_task_input_error"
    )
    if (
      !is.character(directory) ||
        length(directory) != 1L ||
        is.na(directory) ||
        !nzchar(trimws(directory)) ||
        !dir.exists(directory)
    ) {
      tempest_enriched_execution_abort(
        "`directory` must be an existing task directory.",
        class = "tempest_task_directory_input_error",
        deputy_outcome = "invalid_directory"
      )
    }

    directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
    before <- tempest_enriched_file_snapshot(directory)
    source_hash <- digest::digest(code, algo = "sha256", serialize = FALSE)
    startup_timeout <- min(max(timeout, 5), 60)
    agent <- deputy::Agent$new(
      chat = tempest_deputy_offline_chat(),
      working_dir = directory
    )
    session <- deputy::RSession$new(
      agent,
      timeout = timeout,
      startup_timeout = startup_timeout,
      max_output_bytes = 1024^2
    )
    on.exit(try(session$close(), silent = TRUE), add = TRUE)

    result <- tempest_enriched_await_promise(
      session$run(code),
      session,
      timeout = timeout + startup_timeout + 5
    )
    deputy_record <- tryCatch(
      result@extra$deputy_r,
      error = function(error) NULL
    )
    if (!is.list(deputy_record)) {
      tempest_enriched_execution_abort(
        "Deputy did not return structured R execution metadata.",
        class = "tempest_deputy_execution_error",
        deputy_outcome = "missing_result",
        source_hash = source_hash
      )
    }
    outcome <- deputy_record$outcome
    if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome)) {
      outcome <- "unknown"
    }
    if (!identical(outcome, "complete")) {
      outcome <- if (nzchar(outcome)) {
        outcome
      } else {
        "unknown"
      }
      tempest_enriched_execution_abort(
        paste0("Deputy R execution failed (", outcome, ")."),
        class = "tempest_deputy_execution_error",
        deputy_outcome = outcome,
        source_hash = source_hash
      )
    }

    plot_files <- tempest_enriched_plot_files(
      deputy_record$segments,
      directory
    )
    all_files <- unique(c(
      tempest_enriched_changed_files(before, directory),
      plot_files
    ))
    list(
      output = tempest_enriched_execution_output(deputy_record),
      files = all_files,
      metadata = list(
        provider = "Deputy RSession",
        source_hash = source_hash,
        deputy_outcome = outcome,
        r_version = as.character(getRversion()),
        platform = R.version$platform,
        deputy_version = as.character(utils::packageVersion("deputy")),
        session_id = deputy_record$session_id,
        execution_id = deputy_record$execution_id,
        generation = deputy_record$generation,
        plot_count = length(plot_files)
      )
    )
  }
}

tempest_openai_image_perform <- function(request) {
  httr2::req_perform(request)
}

tempest_enriched_valid_image_bytes <- function(bytes) {
  is.raw(bytes) &&
    length(bytes) >= 8L &&
    identical(bytes[seq_len(8L)], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
}

#' Create an OpenAI image generation callback
#'
#' The callback reads `OPENAI_API_KEY` when invoked, requests one PNG image
#' from the OpenAI Images API, and saves it in the provided task directory.
#' The returned callback and metadata never retain the API key.
#'
#' @param model OpenAI image model. The default is GPT-Image-2.6 Flare.
#' @param size Output dimensions: `"1536x1024"`, `"1024x1024"`,
#'   `"1024x1536"`, or `"auto"`.
#' @param quality Image quality: `"medium"`, `"low"`, `"high"`,
#'   `"xhigh"`, `"max"`, or `"auto"`.
#'
#' @return A callback with the `function(task, directory)` interface used by
#'   [tempest_enrich_report()]. It returns `file` and `metadata`.
#' @export
tempest_image_generator <- function(
  model = "gpt-image-2.6-flare",
  size = "1536x1024",
  quality = "medium"
) {
  model <- tempest_enriched_required_string(
    model,
    "model",
    "tempest_image_generator_config_error"
  )
  size <- tempest_enriched_required_string(
    size,
    "size",
    "tempest_image_generator_config_error"
  )
  quality <- tempest_enriched_required_string(
    quality,
    "quality",
    "tempest_image_generator_config_error"
  )
  sizes <- c("1536x1024", "1024x1024", "1024x1536", "auto")
  qualities <- c("medium", "low", "high", "xhigh", "max", "auto")
  if (!grepl("^[A-Za-z0-9._-]+$", model)) {
    tempest_enriched_execution_abort(
      "`model` must be a valid OpenAI image model identifier.",
      class = "tempest_image_generator_config_error"
    )
  }
  if (!size %in% sizes) {
    tempest_enriched_execution_abort(
      "`size` must be a supported standard dimension or `auto`.",
      class = "tempest_image_generator_config_error"
    )
  }
  if (!quality %in% qualities) {
    tempest_enriched_execution_abort(
      "`quality` must be a supported image quality.",
      class = "tempest_image_generator_config_error"
    )
  }

  function(task, directory) {
    if (!is.list(task)) {
      tempest_enriched_execution_abort(
        "`task` must be an illustration task list.",
        class = "tempest_illustration_task_input_error"
      )
    }
    prompt <- tempest_enriched_required_string(
      task[["prompt", exact = TRUE]],
      "task$prompt",
      "tempest_illustration_task_input_error"
    )
    if (
      !is.character(directory) ||
        length(directory) != 1L ||
        is.na(directory) ||
        !nzchar(trimws(directory)) ||
        !dir.exists(directory)
    ) {
      tempest_enriched_execution_abort(
        "`directory` must be an existing task directory.",
        class = "tempest_task_directory_input_error"
      )
    }
    directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
    file <- file.path(directory, "illustration.png")
    if (fs::file_exists(file) || fs::is_link(file)) {
      tempest_enriched_execution_abort(
        "The illustration output path already exists.",
        class = "tempest_image_path_error"
      )
    }

    api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
    if (!nzchar(trimws(api_key))) {
      tempest_enriched_execution_abort(
        "Set `OPENAI_API_KEY` before requesting an OpenAI illustration.",
        class = "tempest_image_api_key_input_error"
      )
    }

    request <- httr2::request(
      "https://api.openai.com/v1/images/generations"
    ) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(
        model = model,
        prompt = prompt,
        n = 1L,
        size = size,
        quality = quality,
        output_format = "png"
      )) |>
      httr2::req_timeout(120)

    response <- tryCatch(
      tempest_openai_image_perform(request),
      error = function(error) {
        tempest_enriched_execution_abort(
          "The OpenAI image generation request failed.",
          class = "tempest_image_generation_error"
        )
      }
    )
    tryCatch(
      httr2::resp_check_status(response),
      error = function(error) {
        tempest_enriched_execution_abort(
          "The OpenAI image generation request failed.",
          class = "tempest_image_generation_error"
        )
      }
    )
    body <- tryCatch(
      httr2::resp_body_json(response, simplifyVector = FALSE),
      error = function(error) {
        tempest_enriched_execution_abort(
          "The OpenAI image generation response was not valid JSON.",
          class = "tempest_image_generation_error"
        )
      }
    )
    image_data <- if (
      is.list(body) && is.list(body$data) && length(body$data) >= 1L
    ) {
      body$data[[1L]]
    } else {
      NULL
    }
    image <- if (is.list(image_data)) image_data$b64_json else NULL
    if (
      !is.character(image) ||
        length(image) != 1L ||
        is.na(image) ||
        !nzchar(image)
    ) {
      tempest_enriched_execution_abort(
        "The OpenAI response did not contain an image.",
        class = "tempest_image_generation_error"
      )
    }
    bytes <- tryCatch(
      jsonlite::base64_dec(image),
      error = function(error) {
        tempest_enriched_execution_abort(
          "The OpenAI response contained invalid image data.",
          class = "tempest_image_generation_error"
        )
      }
    )
    if (!tempest_enriched_valid_image_bytes(bytes)) {
      tempest_enriched_execution_abort(
        "The OpenAI response was not a PNG image.",
        class = "tempest_image_generation_error"
      )
    }
    writeBin(bytes, file)

    revised_prompt <- if (is.list(image_data)) {
      image_data$revised_prompt
    } else {
      NULL
    }
    if (
      !is.character(revised_prompt) ||
        length(revised_prompt) != 1L ||
        is.na(revised_prompt) ||
        !nzchar(revised_prompt)
    ) {
      revised_prompt <- NULL
    }

    list(
      file = "illustration.png",
      metadata = list(
        provider = "openai",
        model = model,
        size = size,
        quality = quality,
        revised_prompt = revised_prompt
      )
    )
  }
}
