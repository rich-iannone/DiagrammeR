#' Add a node to an existing graph object
#' @description With a graph object of class 'gv_graph', add a new node of a specified type to extant nodes within the graph.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a node ID for the newly connected node.
#' @param from an optional vector containing node IDs from which edges will be directed to the new node.
#' @param to an optional vector containing node IDs to which edges will be directed from the new node.
#' @param label a character object for supplying an optional label to the node. Setting to TRUE ascribes the node ID to the label. Setting to FALSE yields a blank label.
#' @param type an optional string that describes the entity type for the node to be added.
#' @return a graph object of class 'gv_graph'.
#' @export add_node

add_node <- function(graph,
                     node = NULL,
                     from = NULL,
                     to = NULL,
                     label = NULL,
                     type = NULL){



}
