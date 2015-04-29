#' Get detailed information on nodes
#'
#' Provides detailed information on nodes and their interrelationships within a graph.
#'
#' @param graph a graph object of class 'gv_graph'.
#' @export node_info

node_info <- function(graph){

  if ("edge_from" %in% colnames(graph$edges_df)){
    edge_from <- graph$edges_df$edge_from
  }

  if ("from" %in% colnames(graph$edges_df)){
    edge_from <- graph$edges_df$from
  }

  if ("edge_to" %in% colnames(graph$edges_df)){
    edge_to <- graph$edges_df$edge_to
  }

  if ("to" %in% colnames(graph$edges_df)){
    edge_to <- graph$edges_df$to
  }

  # Get vector of all node IDs
  all_nodes <- get_nodes(graph)
}
