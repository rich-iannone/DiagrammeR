#' Select edges in a graph using node ID values
#' @description Select edges in a graph object of class
#' \code{dgr_graph} using node ID values. If nodes have IDs that are
#' monotonically increasing integer values, then numeric ranges can
#' be used for the selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param nodes a vector of node IDs for the selection of nodes present
#' in the graph.
#' @param set_op the set operation to perform upon consecutive selections
#' of graph nodes. This can either be as a \code{union} (the default), as an
#' \code{intersection}, or, as a \code{difference} on the previous selection,
#' if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_edges_by_node_id

select_edges_by_node_id <- function(graph, nodes, set_op = "union"){

  # Extract the edge data frame from the graph
  edge_df <- get_edge_df(graph)

  from <-
    edge_df[unique(c(which(edge_df$from %in% nodes),
                     which(edge_df$to %in% nodes))),][,1]

  to <-
    edge_df[unique(c(which(edge_df$from %in% nodes),
                     which(edge_df$to %in% nodes))),][,2]

  # Create selection of edges
  graph$selection$edges$from <- from
  graph$selection$edges$to <- to

  # Remove any selection of nodes
  graph$selection$nodes <- NULL

  return(graph)
}
