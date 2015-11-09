#' Traverse outward from a selected node, skipping over edges, and creating
#' a new node selection
#' @description From a graph object of class \code{dgr_graph} move to the
#' outgoing edge from a selection one or more selected nodes. An optional
#' filter by edge relationship can limit the set of edges traversed to.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param rel an optional vector of edge relationship values for filtering
#' the traversal to outgoing edges.
#' @return a graph object of class \code{dgr_graph}.
#' @export trav_out_edge

trav_out_edge <- function(graph,
                          rel = NULL){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  # Get all paths leading outward from node in selection
  distance_1_paths <-
    get_paths(graph = graph,
              from = graph$selection$nodes,
              distance = 1)

  # if NA returned, then there are no paths outward, so return
  # the same graph object without modifying the node selection
  if (length(distance_1_paths) == 1 & is.na(distance_1_paths)[1]){
    return(graph)
  }

  # For all valid paths returned, extract the nodes traversed to
  for (i in 1:length(distance_1_paths)){

    if (i == 1){
      from_nodes <- vector(mode = "character")
      to_nodes <- vector(mode = "character")
    }

    # Test for relationship matches, if specified

    if (!is.null(rel)){

      if (edge_rel(graph = graph,
               from = distance_1_paths[[i]][1],
               to = distance_1_paths[[i]][2]) %in% rel){

        from_nodes <-
          c(from_nodes,
            distance_1_paths[[i]][1])

        to_nodes <-
          c(to_nodes,
            distance_1_paths[[i]][2])
      }
    }

    if (is.null(rel)){

      from_nodes <-
        c(from_nodes,
          distance_1_paths[[i]][1])

      to_nodes <-
        c(to_nodes,
          distance_1_paths[[i]][2])
    }
  }

  # if NA returned, then there are no paths outward, so return
  # the same graph object without modifying the node selection
  if (length(from_nodes) == 0 & length(from_nodes) == 0){
    return(graph)
  }

  # Remove the node selection in graph
  graph$selection$nodes <- NULL

  # Update edge selection in graph
  graph$selection$edges$from <- from_nodes
  graph$selection$edges$to <- to_nodes

  # Update the list of traversals in graph
  graph$traversals <- c(graph$traversals, distance_1_paths)

  return(graph)
}
