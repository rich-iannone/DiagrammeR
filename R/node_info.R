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

  # Get vector of all labels
  labels <- graph$nodes_df$label

  # Get vector of the top-level nodes
  top_nodes <- unique(edge_from[which(!(edge_from %in% edge_to))])

  # Get vector of the bottom-level nodes
  bottom_nodes <- unique(edge_to[which(!(edge_to %in% edge_from))])

  # Get vector of all nodes neither at the top nor the bottom level
  between_nodes <- all_nodes[which(!(all_nodes %in% c(top_nodes, bottom_nodes)))]

}
