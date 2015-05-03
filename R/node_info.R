#' Get detailed information on nodes
#'
#' Returns a data frame with detailed information on nodes and their interrelationships within a graph.
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

  # Place the nodes in order
  ordered_nodes <- c(top_nodes, between_nodes, bottom_nodes)

  # Create data frame of node properties
  for (i in 1:length(ordered_nodes)){

    if (i == 1){
      node_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 4))
      colnames(node_properties) <- c("node_ID", "label", "predecessors", "successors")
    }

    #
    # Get number of predecessors for each node
    #

    if (ordered_nodes[i] %in% top_nodes){
      predecessors <- 0
    }

    if (!(ordered_nodes[i] %in% top_nodes)){

      for (j in 1:sum(edge_to %in% ordered_nodes[i])){

        if (j == 1) predecessors <- vector(mode = "character")

        predecessors <- c(predecessors, edge_from[which(edge_to %in% ordered_nodes[i])[j]])
      }

      predecessors <- length(predecessors)
    }

    #
    # Get number of successors for each node
    #

    if (ordered_nodes[i] %in% bottom_nodes){
      successors <- 0
    }

    if (!(ordered_nodes[i] %in% bottom_nodes)){

      for (j in 1:sum(edge_from %in% ordered_nodes[i])){

        if (j == 1) successors <- vector(mode = "character")

        successors <- c(successors, edge_from[which(edge_from %in% ordered_nodes[i])[j]])
      }

      successors <- length(successors)
    }

    # Collect information into the 'node_properties' data frame
    node_properties[i, 1] <- ordered_nodes[i]
    node_properties[i, 2] <- labels[which(all_nodes %in% ordered_nodes[i])]
    node_properties[i, 3] <- predecessors
    node_properties[i, 4] <- successors
  }

  return(node_properties)
}
