# Co-STORM mind map utilities

#' Count notes and source_ids per mind map node
#'
#' @param mindmap A mind map list with `nodes` and `edges`.
#' @return A named list where each key is a node id and value is a list
#'   with `n_notes` (word count of notes) and `n_sources` (count of source_ids).
#' @keywords internal
tempest_mindmap_node_sizes <- function(mindmap) {
  nodes <- mindmap$nodes %||% list()
  if (length(nodes) == 0) return(list())

  sizes <- list()
  for (node in nodes) {
    id <- node$id %||% "unknown"
    notes_text <- node$notes %||% ""
    n_notes <- if (nzchar(notes_text)) {
      length(unlist(strsplit(tempest_trim(notes_text), "\\s+")))
    } else {
      0L
    }
    n_sources <- length(node$source_ids %||% character())
    sizes[[id]] <- list(
      n_notes = n_notes,
      n_sources = n_sources,
      total = n_notes + n_sources
    )
  }
  sizes
}

#' Find oversized mind map nodes
#'
#' Identifies nodes whose total content (notes word count + source count)
#' exceeds the trigger threshold.
#'
#' @param mindmap A mind map list with `nodes` and `edges`.
#' @param trigger_count Integer threshold. Nodes with total > trigger_count are oversized.
#' @return Character vector of oversized node ids.
#' @keywords internal
tempest_mindmap_oversized_nodes <- function(mindmap, trigger_count) {
  sizes <- tempest_mindmap_node_sizes(mindmap)
  if (length(sizes) == 0) return(character())

  oversized <- character()
  for (id in names(sizes)) {
    if (sizes[[id]]$total > trigger_count) {
      oversized <- c(oversized, id)
    }
  }
  oversized
}

#' Structured type for mind map node expansion
#'
#' @return An ellmer type definition.
#' @keywords internal
tempest_type_node_expansion <- function() {
  tempest_require("ellmer")
  child_node <- ellmer::type_object(
    label = ellmer::type_string("Child node label"),
    notes = ellmer::type_string("Notes assigned to this child node", required = FALSE),
    source_ids = ellmer::type_array(
      ellmer::type_string("Source ids assigned to this child"),
      required = FALSE
    )
  )
  ellmer::type_object(
    parent_notes = ellmer::type_string(
      "Remaining notes that stay with the parent node",
      required = FALSE
    ),
    children = ellmer::type_array(child_node)
  )
}

#' Expand an oversized mind map node into subtopics
#'
#' Uses an LLM to split a single oversized node into 2-4 child subtopics,
#' distributing notes and source_ids appropriately.
#'
#' @param chat An ellmer chat object (typically the mindmap chat).
#' @param mindmap A mind map list with `nodes` and `edges`.
#' @param node_id The id of the node to expand.
#' @return The updated mind map with the node expanded, or the original
#'   mind map if expansion fails.
#' @keywords internal
tempest_mindmap_expand_node <- function(chat, mindmap, node_id) {
  nodes <- mindmap$nodes %||% list()
  node_by_id <- setNames(nodes, purrr::map_chr(nodes, "id"))
  target <- node_by_id[[node_id]]

  if (is.null(target)) return(mindmap)

  type <- tempest_type_node_expansion()
  prompt <- paste0(
    "A mind map node has grown too large and needs to be split into subtopics.\n\n",
    "Node label: ", target$label %||% node_id, "\n",
    "Node notes: ", target$notes %||% "(none)", "\n",
    "Source IDs: ", paste(target$source_ids %||% character(), collapse = ", "), "\n\n",
    "Split this node into 2-4 meaningful subtopic children.\n",
    "Distribute the notes and source_ids to the most relevant children.\n",
    "Keep any general notes with the parent.\n",
    "Return structured data."
  )

  result <- tryCatch(
    chat$chat_structured(prompt, type = type, echo = "none", convert = FALSE),
    error = function(e) {
      tempest_warn("Failed to expand mind map node {.val {node_id}}: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(result) || is.null(result$children) || length(result$children) == 0) {
    return(mindmap)
  }

  # Update the parent node
  for (i in seq_along(mindmap$nodes)) {
    if (identical(mindmap$nodes[[i]]$id, node_id)) {
      mindmap$nodes[[i]]$notes <- result$parent_notes %||% ""
      # Remove source_ids that were distributed to children
      child_sources <- unique(unlist(purrr::map(result$children, "source_ids")))
      remaining_sources <- setdiff(target$source_ids %||% character(), child_sources)
      mindmap$nodes[[i]]$source_ids <- remaining_sources
      break
    }
  }

  # Add child nodes
  for (j in seq_along(result$children)) {
    child <- result$children[[j]]
    child_id <- paste0(node_id, "_", j)
    new_node <- list(
      id = child_id,
      label = child$label %||% paste("Subtopic", j),
      parent = node_id,
      notes = child$notes %||% "",
      source_ids = child$source_ids %||% character()
    )
    mindmap$nodes <- c(mindmap$nodes, list(new_node))
    mindmap$edges <- c(mindmap$edges, list(list(
      from = node_id,
      to = child_id,
      relation = "subtopic"
    )))
  }

  mindmap
}
