#' Create, read, update, delete, or report status of a node type definition
#' @description From a graph object of class 'gv_graph', query a node in the graph (using the node ID) and perform operations on the type definition for that node.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a node ID corresponding to the node to be selected.
#' @param mode the type of operation to perform post-query. To remove the type definition from a node, use either 'delete', 'remove', or 'drop'. To add a type definition to a node with no type set, use 'add' or 'create'. To update a node's type definition, use 'update'. To return the value of a node type, use 'read'. To determine whether there is a type set for the selected node, use 'available'.
#' @param value a string denoting the node type, supplied only if 'mode' was set to either 'add', 'create', or 'update'.
#' @return a graph object of class 'gv_graph'.
#' @export edge_relationship

node_type <- function(graph,
                      node,
                      mode = "read",
                      value = NULL){

}

