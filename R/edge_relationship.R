#' Create, read, update, delete, or report status of an edge relationship
#' @description From a graph object of class 'gv_graph', query an edge in the graph (defined by a pair of node IDs extant in the graph) and perform operations on the relationship for that edge.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param from a node ID from which the edge to be queried is outgoing.
#' @param to a node ID to which the edge to be queried is incoming.
#' @param mode the type of operation to perform post-query. To remove a relationship from an edge, use either 'delete', 'remove', or 'drop'. To add a relationship to an edge with no set relationship, use 'add' or 'create'. To update an edge relationship, use 'update'. To return the value of an edge relationship, use 'read'. To determine whether there is a set relationship, use 'available'.
#' @param value a string denoting the relationship, supplied only if 'mode' was set to either 'add', 'create', or 'update'.
#' @return a graph object of class 'gv_graph'.
#' @export edge_relationship

edge_relationship <- function(graph,
                              from,
                              to,
                              mode = "read",
                              value = NULL){

  # Determine if edge is present within the graph
  edge_is_in_graph <- edge_present(graph = graph, from = from, to = to)

  # Stop function if edge is not present within the graph
  if (edge_is_in_graph == FALSE){

    stop("The specified edge is not present in the graph.")
  }

  if (edge_is_in_graph == TRUE){

    edge_row <- which(graph$edges_df$edge_from == from & graph$edges_df$edge_to == to)

    relationship_set <- ifelse(graph$edges_df$relationship[edge_row] == "",
                               FALSE, TRUE)

}
