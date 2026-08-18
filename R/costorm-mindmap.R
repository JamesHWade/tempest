# Co-STORM mind map utilities

#' Count notes and source_ids per mind map node
#'
#' @param mindmap A mind map list with `nodes` and `edges`.
#' @return A named list where each key is a node id and value is a list
#'   with `n_notes` (word count of notes) and `n_sources` (count of source_ids).
#' @keywords internal
tempest_mindmap_node_sizes <- function(mindmap) {
  nodes <- mindmap$nodes %||% list()
  if (length(nodes) == 0) {
    return(list())
  }

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
  if (length(sizes) == 0) {
    return(character())
  }

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
    notes = ellmer::type_string(
      "Notes assigned to this child node",
      required = FALSE
    ),
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
#' This legacy generic expansion route is unavailable. Co-STORM mind maps are
#' deterministic projections of committed product evidence and transcript state.
#'
#' @param chat An ellmer chat object (typically the mindmap chat).
#' @param mindmap A mind map list with `nodes` and `edges`.
#' @param node_id The id of the node to expand.
#' @return The updated mind map with the node expanded, or the original
#'   mind map if expansion fails.
#' @keywords internal
tempest_mindmap_expand_node <- function(chat, mindmap, node_id) {
  tempest_costorm_session_abort(
    paste0(
      "Generic mind-map expansion is unavailable; project the mind map from ",
      "the authoritative Co-STORM session instead."
    )
  )
}
