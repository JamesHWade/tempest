# Chat footer, slash commands, async queue, session archives, and progress helpers.

tempest_chat_attachment_types <- function() {
  c(
    "image/png",
    "image/jpeg",
    "image/webp",
    "application/pdf",
    "text/plain",
    "text/markdown",
    "text/csv",
    "application/json"
  )
}

reactive_or_value <- function(x) {
  if (shiny::is.reactive(x)) {
    x()
  } else if (is.function(x)) {
    x()
  } else {
    x
  }
}

chat_footer_ui <- function(ns) {
  shiny::div(
    class = "tempest-chat-footer tempest-chat-footer-shell w-100",
    shiny::uiOutput(
      ns("runtime_footer"),
      class = "tempest-chat-footer-output w-100"
    )
  )
}

chat_report_options_ui <- function(ns) {
  shiny::tags$details(
    id = ns("report_options_popover"),
    class = "tempest-chat-details tempest-chat-report-options-details",
    shiny::tags$summary(
      id = ns("report_options"),
      class = "bslib-toolbar-input-button tempest-chat-details-summary",
      `aria-controls` = ns("report_options_panel"),
      shiny::icon("sliders"),
      shiny::span(
        class = "bslib-toolbar-label visually-hidden",
        "Report options"
      ),
      shiny::div(style = "display:contents;", "Report options")
    ),
    shiny::div(
      id = ns("report_options_panel"),
      class = "tempest-chat-details-panel tempest-chat-report-options",
      shiny::selectInput(
        ns("report_style"),
        "Report style",
        choices = c(
          "Technical" = "technical",
          "Executive" = "executive"
        ),
        selected = "technical",
        selectize = FALSE
      ),
      shiny::p(
        class = "small text-body-secondary mb-0",
        "Uses evidence collected in this session."
      )
    )
  )
}

chat_footer_tooltip <- function(trigger, label) {
  bslib::tooltip(
    trigger,
    label,
    placement = "top"
  )
}

chat_runtime_counts <- function(ses) {
  if (is.null(ses)) {
    return(list(experts = 0L, sources = 0L, facts = 0L, report = FALSE))
  }
  sources <- tryCatch(
    tempest:::tempest_session_workspace(ses)$list_retrieved_sources(),
    error = function(e) list()
  )
  claims <- tryCatch(
    tempest:::tempest_session_workspace(ses)$list_proposed_claims(),
    error = function(e) list()
  )
  report <- tryCatch(
    {
      report_md <- tempest::tempest_report(ses)
      is.character(report_md) &&
        length(report_md) == 1L &&
        !is.na(report_md) &&
        nzchar(report_md)
    },
    error = function(e) FALSE
  )
  list(
    experts = length(ses$experts %||% list()),
    sources = length(sources),
    facts = length(claims),
    report = isTRUE(report)
  )
}

chat_runtime_footer_ui <- function(
  ses,
  progress_state = NULL,
  chat_status = "idle",
  ns = identity
) {
  if (is.null(ses)) {
    return(shiny::span(
      class = "tempest-chat-footer-idle",
      `aria-hidden` = "true"
    ))
  }
  counts <- chat_runtime_counts(ses)
  session_label <- if (identical(chat_status, "streaming")) {
    "Answering"
  } else if (
    !is.null(progress_state) &&
      !is.na(progress_state$current_stage) &&
      nzchar(progress_state$current_stage)
  ) {
    paste("Running", progress_state$current_stage)
  } else {
    "Ready"
  }

  report_status <- if (isTRUE(counts$report)) {
    chat_footer_tooltip(
      shiny::span(
        class = "badge rounded-pill text-bg-light border text-body",
        title = "Report status: Report ready",
        `aria-label` = "Report status: Report ready",
        "Report ready"
      ),
      "Report status: Report ready"
    )
  }

  shiny::div(
    class = paste(
      "tempest-chat-footer-active",
      "d-flex flex-wrap align-items-center justify-content-between gap-2"
    ),
    shiny::div(
      class = paste(
        "tempest-chat-footer-status",
        "d-flex flex-wrap align-items-center gap-1 text-body-secondary"
      ),
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      chat_footer_tooltip(
        shiny::span(
          class = "badge rounded-pill text-bg-light border text-body",
          title = paste("Session status:", session_label),
          `aria-label` = paste("Session status:", session_label),
          session_label
        ),
        paste("Session status:", session_label)
      ),
      chat_footer_tooltip(
        shiny::span(
          class = "d-inline-flex align-items-center gap-1 text-nowrap",
          title = paste("Experts:", counts$experts),
          `aria-label` = paste("Experts:", counts$experts),
          shiny::icon("users"),
          counts$experts
        ),
        paste("Experts:", counts$experts)
      ),
      chat_footer_tooltip(
        shiny::span(
          class = "d-inline-flex align-items-center gap-1 text-nowrap",
          title = paste("Sources:", counts$sources),
          `aria-label` = paste("Sources:", counts$sources),
          shiny::icon("link"),
          counts$sources
        ),
        paste("Sources:", counts$sources)
      ),
      chat_footer_tooltip(
        shiny::span(
          class = "d-inline-flex align-items-center gap-1 text-nowrap",
          title = paste("Facts:", counts$facts),
          `aria-label` = paste("Facts:", counts$facts),
          shiny::icon("clipboard-check"),
          counts$facts
        ),
        paste("Facts:", counts$facts)
      ),
      report_status
    ),
    shiny::div(
      class = "tempest-chat-footer-actions d-flex align-items-center gap-2",
      bslib::toolbar(
        align = "left",
        width = "auto",
        bslib::toolbar_input_button(
          ns("footer_settings_toggle"),
          "Workspace settings",
          icon = shiny::icon("gear"),
          `aria-controls` = ns("settings"),
          `aria-expanded` = "false",
          `data-tempest-settings-trigger` = "true",
          title = "Open workspace settings",
          border = TRUE
        )
      ),
      shiny::actionButton(
        ns("footer_new"),
        "New session",
        icon = shiny::icon("plus"),
        class = "btn-outline-secondary btn-sm text-nowrap"
      ),
      bslib::toolbar(
        align = "left",
        width = "auto",
        chat_report_options_ui(ns)
      ),
      bslib::input_task_button(
        ns("generate_report"),
        "Generate report",
        icon = shiny::icon("file-lines"),
        label_busy = "Generating...",
        type = "secondary",
        class = "btn-sm text-nowrap",
        title = "Generate a report from the evidence collected in this session"
      )
    )
  )
}

chat_command_normalize <- function(command) {
  command <- tolower(trimws(command %||% ""))
  switch(
    command,
    "new-session" = "new",
    claims = "facts",
    command
  )
}

chat_command_message <- function(command, ses = NULL, config = NULL) {
  command <- chat_command_normalize(command)
  switch(
    command,
    experts = chat_command_experts(ses),
    sources = chat_command_sources(ses),
    facts = chat_command_facts(ses),
    system = chat_command_system_prompt(),
    tools = chat_command_tools(ses, config),
    paste0(
      "Unknown command `/",
      command,
      "`.\n\nTry `/experts`, `/sources`, `/facts`, `/report`, `/system`, or `/tools`."
    )
  )
}

chat_command_experts <- function(ses) {
  if (is.null(ses) || length(ses$experts %||% list()) == 0L) {
    return("No expert panel is active. Start a session first.")
  }
  lines <- vapply(
    ses$experts,
    function(expert) {
      paste0(
        "- **",
        expert@name,
        "** [",
        expert@expert_id,
        "]: ",
        expert@title,
        if (nzchar(expert@description)) {
          paste0(" - ", expert@description)
        } else {
          ""
        }
      )
    },
    character(1)
  )
  paste(c("**Expert panel**", "", lines), collapse = "\n")
}

chat_command_sources <- function(ses, n = 5L) {
  sources <- if (is.null(ses)) {
    list()
  } else {
    tryCatch(
      tempest:::tempest_session_workspace(ses)$list_retrieved_sources(),
      error = function(e) list()
    )
  }
  if (length(sources) == 0L) {
    return("No sources collected yet. Ask a research question first.")
  }
  shown <- head(sources, n)
  lines <- vapply(
    shown,
    function(source) {
      title <- source$title %||%
        source$url %||%
        source$id %||%
        "Untitled source"
      if (length(title) == 0L || is.na(title) || !nzchar(title)) {
        title <- "Untitled source"
      }
      paste0("- **", title, "** (", source$id %||% "source", ")")
    },
    character(1)
  )
  more <- length(sources) - length(shown)
  if (more > 0L) {
    lines <- c(lines, paste0("- ...and ", more, " more."))
  }
  paste(c("**Collected sources**", "", lines), collapse = "\n")
}

chat_command_facts <- function(ses, n = 5L) {
  summary <- tempest:::tempest_session_evidence_review_summary(ses, n = n)
  if (summary$total_claims == 0L) {
    if (length(summary$execution_review) > 0L) {
      return(paste(
        c(
          "**Evidence review**",
          "",
          "Verified evidence: 0 of 0 claims.",
          "",
          "No claims were collected.",
          "",
          "**Execution review**",
          "",
          summary$execution_review
        ),
        collapse = "\n"
      ))
    }
    return("No facts collected yet. Ask a research question first.")
  }
  lines <- vapply(
    summary$shown_claims,
    function(claim) {
      score <- if (!is.na(claim@support_score)) {
        paste0(", support ", round(claim@support_score, 2))
      } else {
        ""
      }
      paste0(
        "- ",
        claim@claim_text,
        " (",
        claim@verification_status,
        score,
        ")"
      )
    },
    character(1)
  )
  if (summary$remaining_claims > 0L) {
    lines <- c(lines, paste0("- ...and ", summary$remaining_claims, " more."))
  }
  execution_section <- if (length(summary$execution_review) > 0L) {
    c("", "**Execution review**", "", summary$execution_review)
  } else {
    character()
  }
  paste(
    c(
      "**Evidence review**",
      "",
      paste0(
        "Verified evidence: ",
        summary$verified_claims,
        " of ",
        summary$total_claims,
        " claims."
      ),
      "",
      lines,
      execution_section
    ),
    collapse = "\n"
  )
}

chat_command_system_prompt <- function() {
  prompt <- tryCatch(tempest_prompt("moderator_system"), error = function(e) "")
  if (!nzchar(prompt)) {
    return("The moderator system prompt is not available.")
  }
  paste0("**Moderator system prompt**\n\n```text\n", prompt, "\n```")
}

chat_command_tools <- function(ses, config = NULL) {
  config <- if (is.null(ses)) {
    config
  } else {
    tempest:::tempest_session_config(ses) %||% config
  }
  provider <- tryCatch(config@search_provider, error = function(e) "unknown")
  paste(
    "**Tools and commands**",
    "",
    paste0("- Search provider: `", provider, "`."),
    "- Expert research executes through Deputy-owned session adapters.",
    "- Commands: `/new`, `/experts`, `/sources`, `/facts`, `/report`, `/system`, `/tools`.",
    sep = "\n"
  )
}

costorm_async_queue <- function() {
  generation <- 0L
  tail <- promises::promise_resolve(NULL)
  pending <- 0L
  list(
    enqueue = function(task, on_cancel = function() invisible(NULL)) {
      stopifnot(is.function(task))
      stopifnot(is.function(on_cancel))
      ticket <- generation
      pending <<- pending + 1L
      is_current <- function() identical(ticket, generation)
      result <- promises::then(tail, function(...) {
        if (!is_current()) {
          on_cancel()
          return(NULL)
        }
        task(is_current)
      })
      tail <<- promises::then(
        result,
        onFulfilled = function(value) {
          pending <<- max(0L, pending - 1L)
          value
        },
        onRejected = function(error) {
          pending <<- max(0L, pending - 1L)
          NULL
        }
      )
      result
    },
    cancel = function() {
      generation <<- generation + 1L
      pending <<- 0L
      tail <<- promises::promise_resolve(NULL)
      invisible(NULL)
    },
    pending = function() pending
  )
}

generate_report_for_chat_async <- function(
  ses,
  store,
  append_chat,
  report_navigation_event,
  queue,
  on_error = function(message) invisible(message),
  style = "technical",
  is_current = function() TRUE
) {
  if (is.null(ses)) {
    append_chat("No session active. Start a session first.")
    return(promises::promise_resolve(FALSE))
  }
  n_evidence <- length(tempest:::tempest_session_workspace(
    ses
  )$list_proposed_claims()) +
    length(tempest:::tempest_session_workspace(ses)$list_retrieved_sources())
  if (n_evidence == 0L) {
    append_chat(
      "No facts or sources collected yet. Ask some questions first to gather research."
    )
    return(promises::promise_resolve(FALSE))
  }
  append_chat("Generating report...")
  task <- queue$enqueue(function(queue_current) {
    current <- function() queue_current() && warmup_is_current(is_current)
    tempest:::tempest_session_report_async(
      ses,
      style = style,
      is_current = current
    )
  })
  published <- promises::then(
    task,
    onFulfilled = function(markdown) {
      if (
        !warmup_is_current(is_current) || is.null(markdown) || !nzchar(markdown)
      ) {
        return(FALSE)
      }
      authority_report <- tempest::tempest_report(ses)
      if (!identical(authority_report, markdown)) {
        stop("Generated report content does not match product authority.")
      }
      report_md <- store$publish_costorm_report(ses)
      if (!identical(report_md, markdown)) {
        stop("Published report content does not match the generated report.")
      }
      store$touch_costorm_session()
      append_chat(sprintf(
        "Report generated (%d chars). See the **Report** tab.",
        nchar(markdown)
      ))
      report_navigation_event(report_navigation_event() + 1L)
      TRUE
    }
  )
  promises::catch(published, function(error) {
    if (warmup_is_current(is_current)) {
      message <- "Report generation or publication failed."
      on_error(message)
      append_chat(message)
    }
    FALSE
  })
}

session_storage_root <- function(session) {
  tempest:::tempest_session_archive_storage_root(session$token %||% "session")
}

session_secure_permissions <- function(path) {
  tempest:::tempest_session_archive_secure_permissions(path)
}

session_bundle_enforce_quota <- function(
  path,
  max_bytes = getOption("tempest.shiny.session_quota_bytes", 100 * 1024^2)
) {
  tempest:::tempest_session_archive_enforce_quota(path, max_bytes = max_bytes)
}

session_archive_write <- function(store, file) {
  if (is.null(store$peek_costorm_session())) {
    stop("No Co-STORM session is active.", call. = FALSE)
  }
  tempest:::tempest_session_archive_write(
    save_fn = function(bundle_dir) {
      store$save_costorm_session(bundle_dir, overwrite = FALSE)
    },
    file = file
  )
}

session_archive_extract <- function(archive, root) {
  tempest:::tempest_session_archive_extract(
    archive,
    root,
    reader = function(root) tempest:::tempest_costorm_archive_read(root)
  )
}

session_archive_listing_is_safe <- function(entries, sizes) {
  tempest:::tempest_session_archive_listing_is_safe(entries, sizes)
}

session_persistence_status_ui <- function(state) {
  if (is.null(state) || identical(state$status, "idle")) {
    return(NULL)
  }
  status_class <- switch(
    state$status,
    saved = "text-success",
    restored = "text-info",
    "text-danger"
  )
  status_icon <- switch(
    state$status,
    saved = "circle-check",
    restored = "folder-open",
    "triangle-exclamation"
  )
  role <- if (identical(state$status, "error")) "alert" else "status"
  shiny::div(
    class = paste("small mt-2", status_class),
    role = role,
    `aria-live` = if (identical(role, "status")) "polite" else NULL,
    `aria-atomic` = "true",
    shiny::span(shiny::icon(status_icon), class = "me-1"),
    shiny::span(state$message %||% ""),
  )
}

session_persistence_error_ui <- function(message) {
  if (is.null(message) || !nzchar(message)) {
    return(NULL)
  }
  shiny::div(
    class = "small mt-2 text-danger",
    role = "alert",
    `aria-atomic` = "true",
    shiny::span(shiny::icon("triangle-exclamation"), class = "me-1"),
    shiny::span(message)
  )
}

should_delay_start_suggestions <- function(warmup_enabled, experts) {
  isTRUE(warmup_enabled) && length(experts) > 0
}

costorm_log <- function(format, ...) {
  if (!isTRUE(getOption("tempest.shiny.log", TRUE))) {
    return(invisible(NULL))
  }
  args <- list(...)
  text <- if (length(args) == 0L) {
    format
  } else {
    sprintf(format, ...)
  }
  message("[tempest:chat] ", text)
  invisible(NULL)
}

costorm_starting_event <- function(session_id) {
  tempest:::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "started",
    stage = "session",
    step = "created",
    message = "Starting Co-STORM session."
  )
}

costorm_session_ready_event <- function(session_id, ses) {
  tempest:::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "succeeded",
    stage = "session",
    step = "created",
    message = "Co-STORM session ready.",
    payload = list(expert_count = length(ses$experts))
  )
}

costorm_session_failed_event <- function(session_id, error = NULL) {
  message <- "Session setup failed."
  payload <- list(error_message = message)
  if (!is.null(error)) {
    payload <- progress_error_payload(error)
  }
  tempest:::tempest_progress_event(
    run_id = session_id,
    workflow = "costorm",
    event_type = "stage",
    status = "failed",
    stage = "session",
    step = "created",
    message = message,
    payload = payload
  )
}

costorm_progress_state <- function(events) {
  if (length(events) == 0L) {
    return(NULL)
  }
  tryCatch(
    tempest:::tempest_progress_state(events),
    error = function(e) {
      costorm_log("progress reducer failed")
      NULL
    }
  )
}

record_costorm_progress_event <- function(
  progress_events,
  event,
  session = NULL
) {
  update <- function() {
    progress_events(c(shiny::isolate(progress_events()), list(event)))
  }
  if (is.null(session)) {
    update()
  } else {
    shiny::withReactiveDomain(session, update())
  }
  invisible(event)
}

progress_error_payload <- function(error) {
  tempest:::tempest_progress_error_payload(error)
}

warmup_summary <- function(result) {
  evidence_text <- if (result@source_count > 0L && result@claim_count > 0L) {
    claim_label <- if (result@claim_count == 1L) "fact" else "facts"
    source_label <- if (result@source_count == 1L) "source" else "sources"
    paste0(
      " Collected ",
      result@claim_count,
      " source-backed ",
      claim_label,
      " from ",
      result@source_count,
      " ",
      source_label,
      "."
    )
  } else {
    paste0(
      " No citable evidence was collected; the orientations are scoping ",
      "context only, not research findings."
    )
  }
  failure_text <- if (result@evidence_failure_count > 0L) {
    paste0(
      " Evidence processing failed for ",
      result@evidence_failure_count,
      " expert orientation(s)."
    )
  } else {
    ""
  }
  paste0(
    "**Warmup complete.** Oriented ",
    result@orientation_count,
    " of ",
    result@expert_count,
    " experts around the topic.",
    evidence_text,
    failure_text
  )
}

warmup_result_messages <- function(result) {
  failures <- vapply(
    result@orientations,
    function(orientation) {
      if (identical(orientation$status, "succeeded")) {
        return(NA_character_)
      }
      if (identical(orientation$status, "timeout")) {
        return(paste0(
          "**",
          orientation$expert_name,
          "** orientation timed out; continuing with the rest of the panel."
        ))
      }
      detail <- orientation$error_message
      if (is.na(detail) || !nzchar(detail)) {
        detail <- "No error details were returned."
      }
      paste0(
        "**",
        orientation$expert_name,
        "** orientation was unavailable: ",
        detail
      )
    },
    character(1)
  )
  c(failures[!is.na(failures)], warmup_summary(result))
}

turn_evidence_gap_message <- function() {
  paste(
    "**Evidence gap:** This answer did not add or cite an inspected source.",
    "Treat its factual statements as unverified."
  )
}

turn_notice_message <- function(notice) {
  detail <- notice@details$error_message %||% ""
  switch(
    notice@code,
    evidence_gap = turn_evidence_gap_message(),
    evidence_failed = paste0(
      "**Evidence processing failed:** This answer was not added to the ",
      "evidence ledger. ",
      detail
    ),
    mindmap_failed = paste0("**Mind map update failed:** ", detail),
    suggestions_failed = paste0("**Suggestion generation failed:** ", detail),
    ""
  )
}

warmup_is_current <- function(is_current) {
  tryCatch(
    isTRUE(is_current()),
    error = function(e) FALSE
  )
}
