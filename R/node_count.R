#' Get count of all nodes or certain types of nodes
#' @description From a graph object of class 'gv_graph', get a count of nodes in the graph and optionally obtain count of nodes by their type.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a value that may or may not match a node ID in the graph.
#' @param type either a logical value, where TRUE provides a named vector of node count by type and FALSE (the default) provides a total count, or, a string corresponding to one or more node types.
#' @return a logical value
#' @export node_count

node_count <- function(graph,
                       node,
                       type = FALSE){


}
