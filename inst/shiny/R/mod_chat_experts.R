# Custom expert setup UI, validation, and cards for Co-STORM chat.

expert_count_choices <- function() {
  stats::setNames(
    1:5,
    paste(1:5, ifelse(1:5 == 1L, "expert", "experts"))
  )
}

expert_setup_control_ui <- function(ns, allow_user_experts = FALSE) {
  if (!isTRUE(allow_user_experts)) {
    return(shiny::selectInput(
      ns("n_experts"),
      "Expert panel",
      choices = expert_count_choices(),
      selected = 3,
      selectize = FALSE,
      width = "100%"
    ))
  }

  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(
      class = "control-label",
      `for` = ns("expert_setup"),
      "Expert panel"
    ),
    shiny::actionButton(
      ns("expert_setup"),
      "3 generated",
      icon = shiny::icon("users"),
      class = paste(
        "btn-outline-secondary btn-sm w-100",
        "tempest-chat-expert-setup"
      ),
      title = "Choose generated or user-defined experts"
    )
  )
}

custom_expert_input_id <- function(field, index) {
  paste0("custom_expert_", field, "_", index)
}

expert_setup_count <- function(value) {
  count <- suppressWarnings(as.integer(value))
  if (
    length(count) != 1L ||
      is.na(count) ||
      count < 1L ||
      count > 5L
  ) {
    custom_expert_input_abort("Choose between one and five experts.")
  }
  count
}

custom_expert_drafts <- function(experts) {
  lapply(experts %||% list(), function(expert) {
    list(
      name = expert@name,
      title = expert@title,
      perspective = expert@description
    )
  })
}

custom_expert_fields_ui <- function(ns, count, drafts = list()) {
  count <- expert_setup_count(count %||% 3L)
  shiny::div(
    class = "tempest-custom-expert-grid",
    lapply(seq_len(count), function(index) {
      draft <- if (length(drafts) >= index) drafts[[index]] else list()
      shiny::div(
        class = "tempest-custom-expert-card",
        shiny::div(
          class = "tempest-custom-expert-heading",
          persona_icon(draft$name, paste0("expert.user.", index)),
          shiny::strong(paste("Expert", index))
        ),
        shiny::div(
          class = "tempest-custom-expert-identity",
          shiny::textInput(
            ns(custom_expert_input_id("name", index)),
            "Name",
            value = draft$name %||% "",
            placeholder = "e.g., Maya Chen"
          ),
          shiny::textInput(
            ns(custom_expert_input_id("title", index)),
            "Role or expertise",
            value = draft$title %||% "",
            placeholder = "e.g., Battery policy analyst"
          )
        ),
        shiny::textAreaInput(
          ns(custom_expert_input_id("perspective", index)),
          "Perspective and priorities",
          value = draft$perspective %||% "",
          placeholder = paste(
            "Describe what this expert should focus on, question,",
            "or contribute."
          ),
          rows = 3,
          width = "100%"
        )
      )
    })
  )
}

custom_expert_setup_modal <- function(
  ns,
  mode = "generated",
  generated_count = 3L,
  custom_experts = list()
) {
  custom_count <- if (length(custom_experts) > 0L) {
    length(custom_experts)
  } else {
    expert_setup_count(generated_count)
  }
  shiny::modalDialog(
    title = "Configure expert panel",
    size = "l",
    easyClose = TRUE,
    class = "tempest-expert-modal",
    shiny::p(
      class = "text-body-secondary",
      "Choose a generated panel or define the perspectives represented in this session."
    ),
    shiny::radioButtons(
      ns("expert_setup_mode"),
      "Panel mode",
      choices = c(
        "Generate a balanced panel" = "generated",
        "Choose my own experts" = "custom"
      ),
      selected = mode,
      inline = TRUE
    ),
    shiny::conditionalPanel(
      condition = "input.expert_setup_mode === 'generated'",
      ns = ns,
      shiny::selectInput(
        ns("generated_expert_count"),
        "Number of experts",
        choices = expert_count_choices(),
        selected = generated_count,
        selectize = FALSE,
        width = "14rem"
      )
    ),
    shiny::conditionalPanel(
      condition = "input.expert_setup_mode === 'custom'",
      ns = ns,
      shiny::div(
        class = "tempest-custom-expert-builder",
        shiny::selectInput(
          ns("custom_expert_count"),
          "Number of experts",
          choices = expert_count_choices(),
          selected = custom_count,
          selectize = FALSE,
          width = "14rem"
        ),
        shiny::uiOutput(ns("custom_expert_fields"))
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        ns("apply_expert_setup"),
        "Use this panel",
        class = "btn-primary"
      )
    )
  )
}

custom_expert_specs_from_input <- function(input, count) {
  count <- expert_setup_count(count)
  lapply(seq_len(count), function(index) {
    list(
      name = input[[custom_expert_input_id("name", index)]] %||% "",
      title = input[[custom_expert_input_id("title", index)]] %||% "",
      perspective = input[[
        custom_expert_input_id("perspective", index)
      ]] %||%
        ""
    )
  })
}

custom_expert_input_abort <- function(message) {
  rlang::abort(
    message,
    class = c("tempest_custom_expert_input_error", "tempest_error")
  )
}

custom_expert_profiles <- function(specs) {
  if (!is.list(specs) || length(specs) < 1L || length(specs) > 5L) {
    custom_expert_input_abort("Choose between one and five custom experts.")
  }

  lapply(seq_along(specs), function(index) {
    spec <- specs[[index]]
    if (!is.list(spec)) {
      custom_expert_input_abort(paste("Expert", index, "is malformed."))
    }
    values <- vapply(
      c("name", "title", "perspective"),
      function(field) {
        value <- spec[[field]] %||% ""
        if (!is.character(value) || length(value) != 1L || is.na(value)) {
          custom_expert_input_abort(paste(
            "Expert",
            index,
            "has an invalid",
            field,
            "value."
          ))
        }
        stringi::stri_trim_both(value)
      },
      character(1)
    )
    missing <- names(values)[!nzchar(values)]
    if (length(missing) > 0L) {
      labels <- c(
        name = "name",
        title = "role or expertise",
        perspective = "perspective and priorities"
      )
      custom_expert_input_abort(paste0(
        "Expert ",
        index,
        " needs a ",
        labels[[missing[[1]]]],
        "."
      ))
    }

    tempest::tempest_expert(
      name = values[["name"]],
      title = values[["title"]],
      description = values[["perspective"]],
      instructions = paste(
        "Represent this user-defined expert perspective:",
        values[["perspective"]],
        paste(
          "Gather and cite relevant evidence, distinguish evidence from",
          "interpretation, and state material uncertainty."
        )
      )
    )
  })
}

expert_setup_button_label <- function(mode, count) {
  paste(count, if (identical(mode, "custom")) "custom" else "generated")
}

costorm_session_experts <- function(
  host_experts,
  custom_experts,
  allow_user_experts,
  mode
) {
  if (
    isTRUE(allow_user_experts) &&
      identical(mode, "custom") &&
      length(custom_experts) > 0L
  ) {
    return(custom_experts)
  }
  host_experts
}

expert_intro <- function(ses) {
  if (length(ses$experts) == 0) {
    return("")
  }
  lines <- vapply(
    ses$experts,
    function(expert) {
      paste0(
        "- **",
        expert@name,
        "** (",
        expert@title,
        ")"
      )
    },
    character(1)
  )
  paste0("\n\n**Expert Panel:**\n", paste(lines, collapse = "\n"))
}

expert_card <- function(expert) {
  shiny::div(
    class = "mb-2 p-2 border rounded small",
    shiny::div(
      class = "d-flex align-items-center gap-2",
      persona_icon(expert@name, expert@expert_id),
      shiny::div(
        class = "min-w-0",
        shiny::strong(expert@name),
        shiny::br(),
        shiny::span(class = "text-muted", expert@title)
      )
    ),
    if (nzchar(expert@description)) {
      shiny::div(class = "mt-1 fst-italic", expert@description)
    }
  )
}
