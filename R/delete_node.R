#' Delete a node from an existing graph object
#' @description From a graph object of class 'gv_graph', delete an existing node by specifying its node ID.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a node ID for the node to be deleted from the graph.
#' @return a graph object of class 'gv_graph'.
#' @export delete_node

delete_node <- function(graph,
                        node){

  # Verify that 'node' is given as a single value
  node_is_single_value <- ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE){

    stop("Only a single node can be added using 'add_node")
  }

}
