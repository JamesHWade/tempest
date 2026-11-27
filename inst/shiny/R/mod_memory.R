# Graft project-memory integration for the bundled tempest_app() workspace.

shiny_memory_context <- function() {
  ctx <- getOption("tempest.shiny.memory", NULL)
  if (!is.list(ctx) || is.null(ctx$store)) {
    return(NULL)
  }
  ctx
}

shiny_memory_wrap_config <- function(cfg, memory_ctx, use_tools = FALSE) {
  if (is.null(memory_ctx) || !isTRUE(use_tools)) {
    return(cfg)
  }
  tempest:::tempest_graft_wrap_config_tools(
    cfg = cfg,
    store = memory_ctx$store,
    purpose = memory_ctx$purpose,
    eligible = memory_ctx$eligible,
    roles = c("coordinator", "expert")
  )
}

mod_memory_config_ui <- function(id, streams = character()) {
  ns <- shiny::NS(id)
  choices <- c(
    "None (fresh session)" = "",
    stats::setNames(streams, streams)
  )
  bslib::accordion_panel(
    title = "Project memory (Graft)",
    icon = shiny::icon("book-bookmark"),
    shiny::p(
      class = "small text-body-secondary tempest-config-intro",
      paste(
        "Let the moderator and experts consult team-reviewed findings from",
        "Graft, or pin an accepted stream for new sessions."
      )
    ),
    shiny::checkboxInput(
      ns("use_memory_tools"),
      "Consult reviewed memory in chat",
      value = TRUE
    ),
    shiny::selectInput(
      ns("pinned_stream"),
      "Pin accepted stream for new sessions",
      choices = choices,
      selected = "",
      width = "100%"
    ),
    shiny::div(
      class = "small text-body-secondary",
      role = "status",
      `aria-live` = "polite",
      shiny::uiOutput(ns("stream_status"))
    )
  )
}

mod_memory_config_server <- function(
  id,
  memory_ctx,
  memory_revision = shiny::reactiveVal(0L)
) {
  shiny::moduleServer(id, function(input, output, session) {
    accepted_streams <- shiny::reactive({
      memory_revision()
      if (is.null(memory_ctx)) {
        return(character())
      }
      tryCatch(
        tempest:::tempest_graft_memory_accepted_streams(
          memory_ctx$store,
          purpose = memory_ctx$purpose,
          eligible = memory_ctx$eligible
        )$stream,
        error = function(error) character()
      )
    })

    shiny::observeEvent(
      accepted_streams(),
      {
        streams <- accepted_streams()
        selected <- shiny::isolate(input$pinned_stream %||% "")
        if (!selected %in% c("", streams)) {
          selected <- ""
        }
        shiny::updateSelectInput(
          session,
          "pinned_stream",
          choices = c(
            "None (fresh session)" = "",
            stats::setNames(streams, streams)
          ),
          selected = selected
        )
      },
      ignoreInit = FALSE
    )

    output$stream_status <- shiny::renderUI({
      n <- length(accepted_streams())
      if (n == 0L) {
        "No accepted streams yet. Publish and accept a report on the Report tab."
      } else {
        sprintf(
          "%d accepted stream%s ready for recall.",
          n,
          if (n == 1L) "" else "s"
        )
      }
    })

    use_tools <- shiny::reactive(isTRUE(input$use_memory_tools))
    pinned_knowledge <- shiny::reactive({
      memory_revision()
      stream <- trimws(input$pinned_stream %||% "")
      if (is.null(memory_ctx) || !nzchar(stream)) {
        return(NULL)
      }
      tryCatch(
        tempest:::tempest_graft_reuse_stream(
          store = memory_ctx$store,
          stream = stream,
          purpose = memory_ctx$purpose,
          eligible = memory_ctx$eligible
        ),
        error = function(error) NULL
      )
    })

    list(
      use_tools = use_tools,
      knowledge = pinned_knowledge,
      accepted_streams = accepted_streams
    )
  })
}

mod_memory_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

shiny_memory_product_key <- function(product) {
  if (is.null(product)) {
    return(NULL)
  }
  if (inherits(product, "TempestSession")) {
    manifest <- tryCatch(
      tempest:::tempest_session_manifest(product),
      error = function(error) NULL
    )
    if (is.null(manifest) || !identical(manifest@status, "succeeded")) {
      return(NULL)
    }
    report_sha <- manifest@deliverables$report_md$sha256 %||% ""
    return(paste(manifest@research_run_id, report_sha, sep = ":"))
  }
  if (tempest:::tempest_is_product_result(product)) {
    manifest <- product@manifest
    if (!identical(manifest@status, "succeeded")) {
      return(NULL)
    }
    report_sha <- manifest@deliverables$report_md$sha256 %||% ""
    return(paste(manifest@research_run_id, report_sha, sep = ":"))
  }
  NULL
}

shiny_memory_product_topic <- function(product) {
  if (inherits(product, "TempestSession")) {
    product$topic %||% ""
  } else if (tempest:::tempest_is_product_result(product)) {
    product@title %||% product@topic %||% ""
  } else {
    ""
  }
}

shiny_memory_find_selection <- function(
  store,
  product,
  purpose = "research",
  eligible = NULL
) {
  tempest:::tempest_graft_find_selection(
    store = store,
    product = product,
    purpose = purpose,
    eligible = eligible
  )
}

mod_memory_report_server <- function(
  id,
  memory_ctx,
  product,
  memory_revision = shiny::reactiveVal(0L)
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    published_cache <- new.env(parent = emptyenv())
    current_selection <- shiny::reactiveVal(NULL)
    feedback <- shiny::reactiveVal(NULL)
    feedback_is_error <- shiny::reactiveVal(FALSE)

    product_key <- shiny::reactive(shiny_memory_product_key(product()))

    recover_selection <- function(key, prod) {
      if (is.null(key) || is.null(prod) || is.null(memory_ctx)) {
        return(NULL)
      }
      if (exists(key, envir = published_cache, inherits = FALSE)) {
        return(get(key, envir = published_cache, inherits = FALSE))
      }
      found <- shiny_memory_find_selection(
        store = memory_ctx$store,
        product = prod,
        purpose = memory_ctx$purpose,
        eligible = memory_ctx$eligible
      )
      if (!is.null(found)) {
        assign(key, found, envir = published_cache)
      }
      found
    }

    shiny::observeEvent(
      product_key(),
      {
        key <- product_key()
        feedback(NULL)
        feedback_is_error(FALSE)
        if (is.null(key) || is.null(memory_ctx)) {
          current_selection(NULL)
          return()
        }
        current_selection(recover_selection(key, shiny::isolate(product())))
      },
      ignoreNULL = FALSE
    )

    ensure_published <- function() {
      sel <- shiny::isolate(current_selection())
      if (!is.null(sel)) {
        return(sel)
      }
      prod <- shiny::isolate(product())
      key <- shiny::isolate(product_key())
      if (is.null(prod) || is.null(key)) {
        tempest:::tempest_graft_memory_abort(
          "Publish a report before saving it to Graft.",
          class = "tempest_input_error"
        )
      }
      sel <- tempest::tempest_publish_artifact_research(prod, memory_ctx$store)
      assign(key, sel, envir = published_cache)
      current_selection(sel)
      sel
    }

    displayed_streams <- shiny::reactive({
      memory_revision()
      if (is.null(memory_ctx)) {
        return(data.frame(
          stream = character(),
          decision_id = character(),
          stringsAsFactors = FALSE
        ))
      }
      tryCatch(
        tempest:::tempest_graft_memory_streams(
          memory_ctx$store,
          purpose = memory_ctx$purpose,
          eligible = memory_ctx$eligible
        ),
        error = function(error) {
          data.frame(
            stream = character(),
            decision_id = character(),
            stringsAsFactors = FALSE
          )
        }
      )
    })

    selection_state <- shiny::reactive({
      memory_revision()
      sel <- current_selection()
      if (is.null(sel) || is.null(memory_ctx)) {
        return(NULL)
      }
      tempest:::tempest_graft_selection_status(
        store = memory_ctx$store,
        selection_id = sel,
        purpose = memory_ctx$purpose,
        eligible = memory_ctx$eligible
      )
    })

    shiny::observeEvent(input$refresh, {
      feedback(NULL)
      feedback_is_error(FALSE)
      if (is.null(shiny::isolate(current_selection()))) {
        key <- shiny::isolate(product_key())
        found <- recover_selection(key, shiny::isolate(product()))
        if (!is.null(found)) {
          current_selection(found)
        }
      }
      memory_revision(shiny::isolate(memory_revision()) + 1L)
    })

    shiny::observeEvent(input$accept, {
      if (is.null(memory_ctx)) {
        return()
      }
      tempest:::tempest_graft_memory_handle_accept(
        input = input,
        store = memory_ctx$store,
        purpose = memory_ctx$purpose,
        default_actor = memory_ctx$actor,
        streams_df = displayed_streams(),
        state = selection_state(),
        resolve_selection = ensure_published,
        feedback = feedback,
        feedback_is_error = feedback_is_error,
        memory_revision = memory_revision
      )
    })

    shiny::observeEvent(input$withdraw, {
      if (is.null(memory_ctx)) {
        feedback("Only a currently accepted stream can be withdrawn.")
        feedback_is_error(TRUE)
        return()
      }
      tempest:::tempest_graft_memory_handle_withdraw(
        input = input,
        store = memory_ctx$store,
        default_actor = memory_ctx$actor,
        state = selection_state(),
        feedback = feedback,
        feedback_is_error = feedback_is_error,
        memory_revision = memory_revision
      )
    })

    output$panel <- shiny::renderUI({
      if (is.null(memory_ctx) || is.null(product_key())) {
        return(NULL)
      }
      state <- selection_state()
      status_code <- if (is.null(state)) "provisional" else state$status
      default_stream <- (if (!is.null(state)) state$stream else NULL) %||%
        tempest:::tempest_graft_stream_slug(shiny_memory_product_topic(
          product()
        ))
      tempest:::tempest_graft_memory_review_card_ui(
        ns = ns,
        heading = "Retain in Graft project memory",
        status_code = status_code,
        stream = if (!is.null(state)) state$stream else NULL,
        existing_streams = displayed_streams()$stream,
        default_stream = default_stream,
        default_actor = memory_ctx$actor,
        feedback_text = feedback(),
        feedback_is_error = feedback_is_error(),
        state = state,
        card_class = "tempest-memory-report-panel card mb-3",
        card_tag = "div",
        summary_new = "Review and accept this report into project memory",
        accept_label = "Accept to project memory",
        open_when_unaccepted = FALSE
      )
    })

    list(
      selection = current_selection,
      status = selection_state
    )
  })
}

shiny_active_report_product <- function(
  report_md,
  report_workspace,
  costorm_session = NULL,
  storm_product = NULL
) {
  if (
    !is.character(report_md) ||
      length(report_md) != 1L ||
      is.na(report_md) ||
      !nzchar(report_md) ||
      is.null(report_workspace)
  ) {
    return(NULL)
  }
  if (
    !is.null(storm_product) &&
      tempest:::tempest_is_product_result(storm_product) &&
      identical(storm_product@manifest@status, "succeeded") &&
      identical(storm_product@workspace, report_workspace) &&
      identical(storm_product@report_md, report_md)
  ) {
    return(storm_product)
  }
  if (inherits(costorm_session, "TempestSession")) {
    manifest <- tryCatch(
      tempest:::tempest_session_manifest(costorm_session),
      error = function(error) NULL
    )
    workspace <- tryCatch(
      tempest:::tempest_session_workspace(costorm_session),
      error = function(error) NULL
    )
    session_report <- tryCatch(
      tempest::tempest_report(costorm_session),
      error = function(error) NULL
    )
    if (
      !is.null(manifest) &&
        identical(manifest@status, "succeeded") &&
        identical(workspace, report_workspace) &&
        identical(session_report, report_md)
    ) {
      return(costorm_session)
    }
  }
  NULL
}

shiny_product_graft_consultations <- function(product) {
  tempest:::tempest_product_graft_consultations(product)
}
