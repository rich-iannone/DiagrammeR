#' Get node IDs for predecessor nodes to the specified node
#' Provides a vector of node IDs for all nodes that have a link to the given node.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param node a node ID for the selected node.
#' @return a vector of node ID values.
#' @export get_predecessors

get_predecessors <- function(graph,
                             node){

  # Determine whether graph has nodes
  graph_is_not_empty <- !is_graph_empty(graph)

  # Determine whether the node is in the graph
  node_is_in_graph <- node_present(graph, node)

  # Obtain the node's predecessors
  if (graph_is_not_empty & node_is_in_graph & nrow(edge_info(graph)) > 0){

    if (length(graph$edges_df[graph$edges_df$edge_to == node,]$edge_from) == 0){

      predecessors <- NA

    } else {

      predecessors <- graph$edges_df[graph$edges_df$edge_to == node,]$edge_from

    }

    return(predecessors)
  }
}
