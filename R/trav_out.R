#' Traverse outward from a selected node, skipping over edges, and creating
#' a new node selection
#' @description From a graph object of class \code{dgr_graph} move outward
#' from one or more nodes present in a selection to other nodes, replacing
#' the current nodes in the selection with those nodes traversed to.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param node_attr an optional character vector of node attribute values for
#' filtering the node ID values returned.
#' @param match an option to provide a logical expression with a comparison
#' operator (\code{>}, \code{<}, \code{==}, or \code{!=}) followed by a number
#' for numerical filtering, or, a character string for filtering the edges
#' returned through string matching.
#' @return a graph object of class \code{dgr_graph}.
#' @export trav_out

trav_out <- function(graph,
                     node_attr = NULL,
                     match = NULL){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  # Get the current selection of nodes
  selected_nodes <- get_selection(graph)$nodes

  # Get all paths leading outward from node in selection
  for (i in 1:length(selected_nodes)){
    if (i == 1) successors <- vector(mode = "character")

    if (!is.na(get_successors(graph, selected_nodes[i])[1])){
      successors <-
        c(successors,
          get_successors(graph = graph, selected_nodes[i]))
    }

    if (i == length(selected_nodes)){
      successors <- unique(successors)
    }
  }

  # if no successors returned, then there are no paths outward,
  # so return the same graph object without modifying the node selection
  if (length(successors) == 0){
    return(graph)
  }

  # If a match term provided, filter using a logical expression
  # or a regex match
  if (!is.null(match)){

    if (grepl("^>.*", match) | grepl("^<.*", match) |
        grepl("^==.*", match) | grepl("^!=.*", match)){
      logical_expression <- TRUE } else {
        logical_expression <- FALSE
      }

    if (logical_expression){

      for (i in 1:length(successors)){

        if (i == 1){
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (grepl("^>.*", match)){
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  successors[i]), column_number]) >
              as.numeric(gsub(">(.*)", "\\1", match))){

            to_nodes <- c(to_nodes, successors[i])
          }
        }

        if (grepl("^<.*", match)){
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  successors[i]), column_number]) <
              as.numeric(gsub("<(.*)", "\\1", match))){

            to_nodes <- c(to_nodes, successors[i])
          }
        }

        if (grepl("^==.*", match)){
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  successors[i]), column_number]) ==
              as.numeric(gsub("==(.*)", "\\1", match))){

            to_nodes <- c(to_nodes, successors[i])
          }
        }

        if (grepl("^!=.*", match)){
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  successors[i]), column_number]) !=
              as.numeric(gsub("!=(.*)", "\\1", match))){

            to_nodes <- c(to_nodes, successors[i])
          }
        }
      }
    }

    # Filter using a `match` value
    if (logical_expression == FALSE){

      if (is.numeric(match)){
        match <- as.character(match)
      }

      for (i in 1:length(successors)){

        if (i == 1){
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (match ==
            get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                     successors[i]), column_number]){

          to_nodes <- c(to_nodes, successors[i])
        }
      }
    }

    successors <- to_nodes
  }

  # Update node selection in graph
  if (length(successors) > 0){
    graph$selection$nodes <- successors
    return(graph)
  } else {
    return(graph)
  }
}
