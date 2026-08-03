# Mind Map tab: a KPI strip plus the interactive knowledge graph (visNetwork,
# with a text fallback).

mod_mindmap_ui <- function(id) {
  ns <- shiny::NS(id)

  kpi <- function(title, output_id, icon_name, theme) {
    bslib::value_box(
      title = title,
      value = shiny::textOutput(ns(output_id), inline = TRUE),
      showcase = shiny::icon(icon_name),
      theme = theme
    )
  }

  graph <- if (has_pkg("visNetwork")) {
    shiny::div(
      class = "tempest-mindmap-visualization",
      role = "region",
      `aria-label` = "Interactive mind map visualization",
      `aria-describedby` = ns("graph_description"),
      visNetwork::visNetworkOutput(ns("graph"), height = "520px")
    )
  } else {
    shiny::verbatimTextOutput(ns("graph_text"))
  }

  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("sitemap"), "Mind Map"),
    value = "Mind Map",
    bslib::layout_column_wrap(
      width = "180px",
      fill = FALSE,
      kpi("Nodes", "n_nodes", "sitemap", "primary"),
      kpi("Sources", "n_sources", "link", "info"),
      kpi("Facts", "n_facts", "check-circle", "success"),
      kpi("Turns", "n_turns", "comments", "secondary")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::uiOutput(ns("header"))),
      bslib::card_body(graph)
    ),
    shiny::tags$section(
      class = "mt-3 tempest-mindmap-outline",
      `aria-labelledby` = ns("outline_heading"),
      shiny::h3(
        id = ns("outline_heading"),
        class = "h5",
        "Mind map outline"
      ),
      shiny::p(
        id = ns("graph_description"),
        class = "text-muted small",
        "Use this structured view to inspect nodes, relationships, notes, and sources without the interactive canvas."
      ),
      shiny::uiOutput(ns("graph_status")),
      shiny::uiOutput(ns("graph_accessible"))
    )
  )
}

mod_mindmap_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    mindmap <- shiny::reactive({
      ses <- store$get()
      if (is.null(ses)) NULL else ses$mindmap
    })

    output$n_nodes <- shiny::renderText(length(mindmap()$nodes %||% list()))
    output$n_sources <- shiny::renderText({
      ses <- store$get()
      if (is.null(ses)) 0L else length(ses$store$list_sources())
    })
    output$n_facts <- shiny::renderText({
      ses <- store$get()
      if (is.null(ses)) 0L else length(ses$store$list_claims())
    })
    output$n_turns <- shiny::renderText({
      ses <- store$get()
      if (is.null(ses)) 0L else length(ses$transcript %||% list())
    })

    output$header <- shiny::renderUI({
      mm <- mindmap()
      if (is.null(mm)) {
        return(shiny::span("Knowledge Mind Map"))
      }
      shiny::tagList(
        shiny::span("Knowledge Mind Map"),
        shiny::span(
          class = "badge bg-secondary ms-2",
          paste0(
            length(mm$nodes %||% list()),
            " nodes, ",
            length(mm$edges %||% list()),
            " edges"
          )
        )
      )
    })

    output$graph_status <- shiny::renderUI({
      mm <- mindmap()
      counts <- if (is.null(mm)) {
        "No mind map is available."
      } else {
        paste(
          length(mm$nodes %||% list()),
          "nodes and",
          length(mm$edges %||% list()),
          "relationships available."
        )
      }
      shiny::p(
        class = "visually-hidden",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        counts
      )
    })

    output$graph_accessible <- shiny::renderUI({
      ses <- store$get()
      if (is.null(ses)) {
        return(shiny::p("Start a session to see the mind map."))
      }
      mindmap_accessible_tree(ses$mindmap, source_store = ses$store)
    })

    if (has_pkg("visNetwork")) {
      output$graph <- visNetwork::renderVisNetwork({
        mm <- mindmap()
        if (is.null(mm)) {
          return(empty_graph("Start a session to see the mind map"))
        }
        vn <- mindmap_to_visnetwork(mm)
        if (is.null(vn)) {
          return(empty_graph("No mind map nodes yet"))
        }
        knowledge_graph(vn, input_id = session$ns("selected_node"))
      })

      shiny::observeEvent(input$selected_node, {
        mm <- mindmap()
        node <- find_node(mm, input$selected_node)
        shiny::req(node)
        shiny::showModal(node_modal(node, input$selected_node))
      })
    } else {
      output$graph_text <- shiny::renderText({
        ses <- store$get()
        if (is.null(ses)) {
          return("Start a session to see the mind map.")
        }
        ses$mindmap_markdown()
      })
    }
  })
}

# --- visNetwork helpers ------------------------------------------------------

empty_graph <- function(label) {
  visNetwork::visNetwork(
    data.frame(id = 1, label = label, stringsAsFactors = FALSE),
    data.frame(from = integer(), to = integer(), stringsAsFactors = FALSE)
  ) |>
    visNetwork::visOptions(highlightNearest = FALSE) |>
    visNetwork::visInteraction(dragNodes = FALSE)
}

knowledge_graph <- function(vn, input_id) {
  click_js <- sprintf(
    "function(params) {
      if (params.nodes.length > 0) {
        Shiny.setInputValue('%s', params.nodes[0], {priority: 'event'});
      }
    }",
    input_id
  )
  visNetwork::visNetwork(vn$nodes, vn$edges) |>
    visNetwork::visHierarchicalLayout(
      direction = "UD",
      sortMethod = "directed"
    ) |>
    visNetwork::visNodes(
      shape = "box",
      font = list(size = 14),
      widthConstraint = list(maximum = 200)
    ) |>
    visNetwork::visEdges(
      arrows = "to",
      color = list(color = "#6C757D"),
      smooth = list(type = "cubicBezier")
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = FALSE
    ) |>
    visNetwork::visEvents(click = click_js) |>
    visNetwork::visInteraction(navigationButtons = TRUE, zoomView = TRUE)
}

find_node <- function(mindmap, node_id) {
  if (is.null(mindmap) || is.null(node_id)) {
    return(NULL)
  }
  for (nd in mindmap$nodes %||% list()) {
    if (identical(nd$id, node_id)) {
      return(nd)
    }
  }
  NULL
}

node_modal <- function(node, node_id) {
  notes_ui <- if (!is.null(node$notes) && nzchar(node$notes)) {
    shiny::p(node$notes)
  } else {
    shiny::p(class = "text-muted", "No notes available.")
  }
  src_ui <- if (length(node$source_ids %||% character()) > 0) {
    shiny::p(
      shiny::strong("Sources: "),
      paste(node$source_ids, collapse = ", ")
    )
  } else {
    shiny::p(class = "text-muted", "No sources linked.")
  }
  shiny::modalDialog(
    title = node$label %||% node_id,
    notes_ui,
    src_ui,
    easyClose = TRUE,
    footer = shiny::modalButton("Close")
  )
}

mindmap_accessible_tree <- function(mindmap, source_store = NULL) {
  nodes <- mindmap$nodes %||% list()
  edges <- mindmap$edges %||% list()
  if (length(nodes) == 0L) {
    return(shiny::p("No mind map nodes yet."))
  }
  labels <- stats::setNames(
    vapply(nodes, function(node) node$label %||% node$id, character(1)),
    vapply(nodes, `[[`, character(1), "id")
  )
  node_items <- lapply(nodes, function(node) {
    node_id <- node$id
    parent <- node$parent %||% NULL
    children <- vapply(
      nodes,
      function(candidate) {
        identical(candidate$parent %||% NULL, node_id)
      },
      logical(1)
    )
    relationships <- Filter(
      function(edge) {
        identical(edge$from %||% "", node_id) ||
          identical(edge$to %||% "", node_id)
      },
      edges
    )
    relationship_items <- lapply(relationships, function(edge) {
      direction <- if (identical(edge$from %||% "", node_id)) {
        paste("to", labels[[edge$to]] %||% edge$to)
      } else {
        paste("from", labels[[edge$from]] %||% edge$from)
      }
      shiny::tags$li(paste(edge$relation %||% "related", direction))
    })
    sources <- lapply(node$source_ids %||% character(), function(source_id) {
      source <- if (is.null(source_store)) {
        NULL
      } else {
        source_store$get_source(source_id)
      }
      url <- citation_safe_url(source$url %||% "")
      label <- source$title %||% source_id
      if (is.na(label) || !nzchar(label)) {
        label <- source_id
      }
      shiny::tags$li(
        if (nzchar(url)) {
          shiny::tags$a(
            href = url,
            target = "_blank",
            rel = "noopener noreferrer",
            paste(label, paste0("(", source_id, ")"))
          )
        } else {
          paste(label, paste0("(", source_id, ")"))
        }
      )
    })
    shiny::tags$li(
      shiny::tags$details(
        shiny::tags$summary(node$label %||% node_id),
        shiny::tags$dl(
          if (!is.null(parent)) {
            shiny::tagList(
              shiny::tags$dt("Parent"),
              shiny::tags$dd(labels[[parent]] %||% parent)
            )
          },
          shiny::tags$dt("Children"),
          shiny::tags$dd(
            if (any(children)) {
              paste(unname(labels[children]), collapse = ", ")
            } else {
              "None"
            }
          ),
          shiny::tags$dt("Notes"),
          shiny::tags$dd(
            if (nzchar(node$notes %||% "")) node$notes else "No notes"
          )
        ),
        if (length(relationship_items) > 0L) {
          shiny::tagList(
            shiny::tags$h4(class = "h6", "Relationships"),
            shiny::tags$ul(relationship_items)
          )
        },
        shiny::tags$h4(class = "h6", "Sources"),
        if (length(sources) > 0L) {
          shiny::tags$ul(sources)
        } else {
          shiny::p("No sources linked.")
        }
      )
    )
  })
  shiny::tags$ul(class = "list-unstyled d-grid gap-2", node_items)
}
