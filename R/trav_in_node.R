#' Traverse from one or more selected edges toward adjacent inward nodes
#' @description From a graph object of class \code{dgr_graph} move to
#' adjacent, inward nodes from a selection of one or more selected edges,
#' thereby creating a selection of nodes. An optional filter by node attribute
#' can limit the set of nodes traversed to.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param node_attr an optional character vector of node attribute values for
#' filtering the node ID values returned.
#' @param search an option to provide a logical expression with a comparison
#' operator (\code{>}, \code{<}, \code{==}, or \code{!=}) followed by a number
#' for numerical filtering, or, a regular expression for filtering the nodes
#' returned through string matching.
#' @return a graph object of class \code{dgr_graph}.
#' @export trav_in_node

trav_in_node <- function(graph,
                         node_attr = NULL,
                         search = NULL){

  if (is.null(graph$selection$edges)){
    stop("There is no selection of edges available.")
  }

  # Create a selection of nodes using the edge selection
  landing_nodes <- unique(graph$selection$edges$from)

  # If a search term provided, filter using a logical expression
  # or a regex match
  if (!is.null(search)){

    if (grepl("^>.*", search) | grepl("^<.*", search) |
        grepl("^==.*", search) | grepl("^!=.*", search)){
      logical_expression <- TRUE } else {
        logical_expression <- FALSE
      }

    if (logical_expression){

      for (i in 1:length(landing_nodes)){

        if (i == 1){
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (grepl("^>.*", search)){
          if (get_node_attr(graph,
                            nodes = landing_nodes[i])[1,column_number] >
              as.numeric(gsub(">(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }

        if (grepl("^<.*", search)){
          if (get_node_attr(graph,
                            nodes = landing_nodes[i])[1,column_number] <
              as.numeric(gsub(">(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }

        if (grepl("^==.*", search)){
          if (get_node_attr(graph,
                            nodes = landing_nodes[i])[1,column_number] ==
              as.numeric(gsub(">(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }

        if (grepl("^!=.*", search)){
          if (get_node_attr(graph,
                            nodes = landing_nodes[i])[1,column_number] !=
              as.numeric(gsub(">(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }
      }
    }

    # Filter using a `search` value as a regular expression
    if (logical_expression == FALSE){

      for (i in 1:length(landing_nodes)){

        if (i == 1){
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (grepl(search, get_node_attr(graph,
                                        nodes = landing_nodes[i])[1,column_number])){

          to_nodes <- c(to_nodes, landing_nodes[i])
        }
      }
    }

    landing_nodes <- to_nodes
  }

  # If there are no valid traversals, return the same graph object
  # without modifying the node selection
  if (length(landing_nodes) == 0){
    return(graph)
  }

  # Remove the edge selection in graph
  graph$selection$edges <- NULL

  # Update node selection in graph
  graph$selection$nodes <- landing_nodes

  # Remove traversals in graph
  graph$traversals <- NULL

  return(graph)
}
