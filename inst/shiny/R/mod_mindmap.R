# Mind map with session counts, an interactive graph and a text outline.

mod_mindmap_ui <- function(id) {
  ns <- shiny::NS(id)

  metric <- function(title, output_id) {
    shiny::div(
      shiny::tags$dt(title),
      shiny::tags$dd(shiny::textOutput(ns(output_id), inline = TRUE))
    )
  }

  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("sitemap"), "Mind Map"),
    value = "Mind Map",
    shiny::tags$dl(
      class = "tempest-map-metrics",
      metric("Nodes", "n_nodes"),
      metric("Sources", "n_sources"),
      metric("Proposed facts", "n_facts"),
      metric("Discussion turns", "n_turns")
    ),
    bslib::card(
      full_screen = TRUE,
      class = "tempest-mindmap-card",
      bslib::card_header(
        class = "tempest-mindmap-heading",
        shiny::h2(class = "h4 mb-0", "Knowledge map"),
        shiny::p(
          id = ns("graph_description"),
          class = "mb-0 text-muted small",
          "Drag to explore, scroll to zoom, and select a node for its notes and linked sources."
        )
      ),
      bslib::card_body(
        class = "p-0 tempest-mindmap-canvas",
        shiny::uiOutput(ns("graph_view"))
      )
    ),
    shiny::tags$details(
      class = "mt-3 tempest-mindmap-outline",
      shiny::tags$summary(
        id = ns("outline_heading"),
        shiny::span(
          class = "tempest-mindmap-outline-title",
          shiny::icon("list"),
          "Accessible mind map outline"
        ),
        shiny::span(
          class = "tempest-mindmap-outline-hint",
          "Show structured view"
        )
      ),
      shiny::div(
        class = "tempest-mindmap-outline-body",
        shiny::p(
          class = "text-muted small",
          "Inspect nodes, relationships, notes, and sources without using the interactive canvas."
        ),
        shiny::uiOutput(ns("graph_status")),
        shiny::uiOutput(ns("graph_accessible"))
      )
    )
  )
}

mod_mindmap_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    mindmap <- shiny::reactive({
      ses <- store$costorm_session()
      if (is.null(ses)) NULL else ses$mindmap
    })

    output$n_nodes <- shiny::renderText(length(mindmap()$nodes %||% list()))
    output$n_sources <- shiny::renderText({
      ses <- store$costorm_session()
      if (is.null(ses)) {
        0L
      } else {
        length(tempest:::tempest_session_workspace(
          ses
        )$list_retrieved_sources())
      }
    })
    output$n_facts <- shiny::renderText({
      ses <- store$costorm_session()
      if (is.null(ses)) {
        0L
      } else {
        length(tempest:::tempest_session_workspace(ses)$list_proposed_claims())
      }
    })
    output$n_turns <- shiny::renderText({
      ses <- store$costorm_session()
      if (is.null(ses)) 0L else length(ses$transcript %||% list())
    })

    output$graph_view <- shiny::renderUI({
      mm <- mindmap()
      if (!length(mm$nodes)) {
        message <- if (is.null(store$costorm_session())) {
          "Start a discussion in Chat to build a knowledge map."
        } else {
          "Topics and connections will appear here as the discussion develops."
        }
        return(shiny::p(class = "tempest-map-empty", message))
      }
      if (has_pkg("visNetwork")) {
        shiny::div(
          class = "tempest-mindmap-visualization",
          role = "region",
          `aria-label` = "Interactive mind map visualization",
          `aria-describedby` = session$ns("graph_description"),
          visNetwork::visNetworkOutput(session$ns("graph"), height = "520px")
        )
      } else {
        shiny::verbatimTextOutput(session$ns("graph_text"))
      }
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
      ses <- store$costorm_session()
      if (is.null(ses)) {
        return(shiny::p("Start a session to see the mind map."))
      }
      mindmap_accessible_tree(
        ses$mindmap,
        workspace = tempest:::tempest_session_workspace(ses)
      )
    })

    if (has_pkg("visNetwork")) {
      output$graph <- visNetwork::renderVisNetwork({
        mm <- mindmap()
        shiny::req(length(mm$nodes))
        vn <- mindmap_to_visnetwork(mm)
        shiny::req(vn)
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
        ses <- store$costorm_session()
        if (is.null(ses)) {
          return("Start a session to see the mind map.")
        }
        tempest:::tempest_session_mindmap_markdown(ses)
      })
    }
  })
}

# --- visNetwork helpers ------------------------------------------------------

knowledge_graph <- function(vn, input_id) {
  click_js <- sprintf(
    "function(params) {
      if (params.nodes.length > 0) {
        Shiny.setInputValue('%s', params.nodes[0], {priority: 'event'});
      }
    }",
    input_id
  )
  overview_ids <- vn$nodes$id[vn$nodes$level <= 2L]
  fit_js <- paste0(
    "function() {",
    "this.fit({nodes: ",
    jsonlite::toJSON(overview_ids, auto_unbox = FALSE),
    ", animation: {duration: 350, easingFunction: 'easeInOutQuad'}});",
    "}"
  )
  visNetwork::visNetwork(vn$nodes, vn$edges) |>
    visNetwork::visHierarchicalLayout(
      levelSeparation = 130,
      nodeSpacing = 110,
      treeSpacing = 170,
      blockShifting = TRUE,
      edgeMinimization = TRUE,
      parentCentralization = TRUE,
      direction = "UD",
      sortMethod = "directed"
    ) |>
    visNetwork::visNodes(
      shape = "box",
      borderWidth = 2,
      borderWidthSelected = 3,
      font = list(
        size = 14,
        face = "system-ui",
        color = "#15293A"
      ),
      margin = list(top = 10, right = 12, bottom = 10, left = 12),
      shadow = list(enabled = TRUE, color = "rgba(21,41,58,0.18)", size = 6),
      widthConstraint = list(maximum = 220),
      color = list(
        border = "#326B7D",
        highlight = list(background = "#F4B740", border = "#15293A"),
        hover = list(background = "#EEF3F5", border = "#4A90A4")
      )
    ) |>
    visNetwork::visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7)),
      color = list(
        color = "#8A9AA4",
        highlight = "#326B7D",
        hover = "#4A90A4"
      ),
      font = list(size = 10, color = "#596771", strokeWidth = 0),
      smooth = list(type = "cubicBezier", forceDirection = "vertical")
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = FALSE
    ) |>
    visNetwork::visLayout(randomSeed = 42, improvedLayout = TRUE) |>
    visNetwork::visEvents(
      click = click_js,
      stabilizationIterationsDone = fit_js
    ) |>
    visNetwork::visInteraction(
      hover = TRUE,
      keyboard = list(enabled = TRUE, bindToWindow = FALSE),
      navigationButtons = TRUE,
      tooltipDelay = 200,
      zoomView = TRUE
    )
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

mindmap_accessible_tree <- function(mindmap, workspace = NULL) {
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
      source <- if (is.null(workspace)) {
        NULL
      } else {
        workspace$get_retrieved_source(source_id)
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

# Convert a mind map (list of nodes/edges) into visNetwork data frames.
mindmap_to_visnetwork <- function(mindmap) {
  nodes <- mindmap$nodes %||% list()
  edges <- mindmap$edges %||% list()
  if (length(nodes) == 0) {
    return(NULL)
  }

  parent_map <- list()
  for (nd in nodes) {
    if (!is.null(nd$parent)) {
      parent_map[[nd$id]] <- nd$parent
    }
  }
  node_level <- function(id) {
    level <- 1L
    cur <- id
    visited <- character()
    while (!is.null(parent_map[[cur]]) && !cur %in% visited) {
      visited <- c(visited, cur)
      level <- level + 1L
      cur <- parent_map[[cur]]
    }
    level
  }

  # Shade nodes from light blue toward the brand primary as evidence grows.
  color_for_count <- function(n) {
    frac <- min(n / 5, 1)
    r <- as.integer(200 - frac * (200 - 74))
    g <- as.integer(220 - frac * (220 - 144))
    b <- as.integer(240 - frac * (240 - 164))
    sprintf("#%02X%02X%02X", r, g, b)
  }

  node_tooltip <- function(nd, id) {
    notes_html <- if (!is.null(nd$notes) && nzchar(nd$notes)) {
      paste0("<p>", htmltools::htmlEscape(nd$notes), "</p>")
    } else {
      ""
    }
    src_count <- length(nd$source_ids %||% character())
    src_html <- if (src_count > 0) {
      paste0(
        "<p><em>Sources: ",
        htmltools::htmlEscape(paste(nd$source_ids, collapse = ", ")),
        "</em></p>"
      )
    } else {
      ""
    }
    paste0(
      "<b>",
      htmltools::htmlEscape(nd$label %||% id),
      "</b>",
      notes_html,
      src_html
    )
  }

  ids <- vapply(
    seq_along(nodes),
    function(i) nodes[[i]]$id %||% paste0("node_", i),
    character(1)
  )
  nodes_df <- data.frame(
    id = ids,
    label = vapply(
      seq_along(nodes),
      function(i) nodes[[i]]$label %||% ids[i],
      character(1)
    ),
    level = vapply(ids, node_level, integer(1)),
    color = vapply(
      nodes,
      function(nd) color_for_count(length(nd$source_ids %||% character())),
      character(1)
    ),
    title = vapply(
      seq_along(nodes),
      function(i) node_tooltip(nodes[[i]], ids[i]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  edge_pairs <- vapply(
    edges,
    function(edge) paste(edge$from %||% "", edge$to %||% "", sep = "\r"),
    character(1)
  )
  hierarchy_edges <- Filter(
    Negate(is.null),
    lapply(nodes, function(node) {
      parent <- node$parent %||% NULL
      if (is.null(parent)) {
        return(NULL)
      }
      pair <- paste(parent, node$id, sep = "\r")
      if (pair %in% edge_pairs) {
        return(NULL)
      }
      list(
        from = parent,
        to = node$id,
        relation = "Contains",
        structural = TRUE
      )
    })
  )
  edges <- c(edges, hierarchy_edges)

  edges_df <- if (length(edges) > 0) {
    data.frame(
      from = vapply(edges, function(e) e$from %||% "", character(1)),
      to = vapply(edges, function(e) e$to %||% "", character(1)),
      label = "",
      title = vapply(
        edges,
        function(e) htmltools::htmlEscape(e$relation %||% "Related"),
        character(1)
      ),
      dashes = vapply(edges, function(e) isTRUE(e$structural), logical(1)),
      color = vapply(
        edges,
        function(e) if (isTRUE(e$structural)) "#B7C3CA" else "#8A9AA4",
        character(1)
      ),
      arrows = "to",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      from = character(),
      to = character(),
      label = character(),
      title = character(),
      dashes = logical(),
      color = character(),
      arrows = character(),
      stringsAsFactors = FALSE
    )
  }

  list(nodes = nodes_df, edges = edges_df)
}
