#' Delete an edge from an existing graph object
#' From a graph object of class \code{dgr_graph}, delete an existing edge by specifying a pair of node IDs corresponding to the edge direction.
#' @param graph a graph object of class \code{dgr_graph} that is created using \code{create_graph}.
#' @param from a node ID from which the edge to be removed is outgoing.
#' @param to a node ID to which the edge to be removed is incoming.
#' @return a graph object of class \code{dgr_graph}.
#' @export delete_edge

delete_edge <- function(graph,
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

    if (any(graph$edges_df$edge_from == from & graph$edges_df$edge_to == to)){

      row_id_edge_removal <-
        which(graph$edges_df$edge_from == from & graph$edges_df$edge_to == to)

      revised_edges_df <- graph$edges_df[-row_id_edge_removal,]

      row.names(revised_edges_df) <- NULL

      dgr_graph <- create_graph(nodes_df = graph$nodes_df,
                                 edges_df = revised_edges_df)
    }
  }

  return(dgr_graph)
}
