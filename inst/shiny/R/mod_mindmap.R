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
    visNetwork::visNetworkOutput(ns("graph"), height = "100%")
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
        ses$artifacts[["mindmap_md"]] %||% ses$mindmap_markdown()
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
