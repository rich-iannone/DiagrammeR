#' Get node IDs for successor nodes to the specified node
#' @description Provides a vector of node IDs for all nodes that have a link from the given node.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param node a node ID for the selected node.
#' @return a vector of node ID values.
#' @export get_successors

get_successors <- function(graph,
                           node){

  # Determine whether graph has nodes
  graph_is_not_empty <- !is_graph_empty(graph)

  # Determine whether the node is in the graph
  node_is_in_graph <- node_present(graph, node)

  # Obtain the node's successors
  if (graph_is_not_empty & node_is_in_graph & nrow(edge_info(graph)) > 0){

    if (length(graph$edges_df[graph$edges_df$from == node,]$to) == 0){

      successors <- NA

    } else {

      successors <- graph$edges_df[graph$edges_df$from == node,]$to

    }

    return(successors)
  }
}
