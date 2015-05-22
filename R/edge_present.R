#' Determine whether a specified edge is present in an existing graph object
#' From a graph object of class \code{dgr_graph}, determine whether a directed edge (defined by a pair of node IDs extant in the graph) is present.
#' @param graph a graph object of class \code{dgr_graph} that is created using \code{create_graph}.
#' @param from a node ID from which the edge to be queried is outgoing.
#' @param to a node ID to which the edge to be queried is incoming.
#' @return a logical value.
#' @export edge_present

edge_present <- function(graph,
                         from,
                         to){

  # Verify that each of the values for 'from' and 'to' are given as a single values
  from_is_single_value <- ifelse(length(from) == 1, TRUE, FALSE)
  to_is_single_value <- ifelse(length(to) == 1, TRUE, FALSE)

  # Stop function if either node is not a single value
  if (from_is_single_value == FALSE | to_is_single_value == FALSE){

    stop("Only single nodes for 'from' and 'to' should be specified.")
  }

  # Determine whether pair of node provided are in the graph
  if (from_is_single_value == TRUE & to_is_single_value == TRUE){

    nodes_available_in_graph <- ifelse(all(c(from, to) %in% get_nodes(graph)), TRUE, FALSE)
  }

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE){

    stop("The nodes specified are not both present in the graph.")
  }

  # Determine whether a matching edge is available in the graph
  if (nodes_available_in_graph){

    edge_is_in_graph <- ifelse(any(graph$edges_df$edge_from == from &
                                     graph$edges_df$edge_to == to),
                               TRUE, FALSE)

    return(edge_is_in_graph)
  }
}
