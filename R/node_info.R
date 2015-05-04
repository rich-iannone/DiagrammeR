#' Get detailed information on nodes
#'
#' Returns a data frame with detailed information on nodes and their interrelationships within a graph.
#'
#' @param graph a graph object of class 'gv_graph'.
#' @return a data frame containing information specific to each node within the graph.
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

  if ("type" %in% colnames(graph$nodes_df)){
    type <- graph$nodes_df$type
  }

  # Get vector of all node IDs
  all_nodes <- get_nodes(graph)

  # Get vector of all labels
  labels <- graph$nodes_df$label

  # For graphs with no edges, create a 'node_properties' data frame that doesn't
  # need to consider any edge information
  if (is.null(graph$edges_df)){

    node_properties <- as.data.frame(mat.or.vec(nr = length(all_nodes), nc = 7))
    colnames(node_properties) <- c("node_ID", "label", "type", "degree",
                                   "predecessors", "successors", "loops")

    node_properties[, 1] <- all_nodes
    node_properties[, 2] <- labels
    node_properties[, 3] <- ifelse(exists("type"),
                                   type, rep(NA, length(all_nodes)))
    node_properties[, 4] <- rep(0, length(all_nodes))
    node_properties[, 5] <- rep(0, length(all_nodes))
    node_properties[, 6] <- rep(0, length(all_nodes))
    node_properties[, 7] <- rep(0, length(all_nodes))

    return(node_properties)
  }

  if (!is.null(graph$edges_df)){

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
        node_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 7))
        colnames(node_properties) <- c("node_ID", "label", "type", "degree",
                                       "predecessors", "successors", "loops")
      }

      #
      # Get degree for each node
      #

      degree <- sum(c(graph$edges_df$edge_from, graph$edges_df$edge_to) %in%
                      ordered_nodes[i])
      #
      # Get number of predecessors for each node
      #

      if (ordered_nodes[i] %in% top_nodes | degree == 0){
        predecessors <- 0
      }

      if (!(ordered_nodes[i] %in% top_nodes) & degree != 0){

        for (j in 1:sum(edge_to %in% ordered_nodes[i])){

          if (j == 1) predecessors <- vector(mode = "character")

          predecessors <- c(predecessors, edge_from[which(edge_to %in% ordered_nodes[i])[j]])
        }

        predecessors <- length(predecessors)
      }

      #
      # Get number of successors for each node
      #

      if (ordered_nodes[i] %in% bottom_nodes | degree == 0){
        successors <- 0
      }

      if (!(ordered_nodes[i] %in% bottom_nodes) & degree != 0){

        for (j in 1:sum(edge_from %in% ordered_nodes[i])){

          if (j == 1) successors <- vector(mode = "character")

          successors <- c(successors, edge_from[which(edge_from %in% ordered_nodes[i])[j]])
        }

        successors <- length(successors)
      }

      #
      # Get number of loops for each node
      #

      loops <- sum(graph$edges_df$edge_from == graph$edges_df$edge_to &
                     graph$edges_df$edge_to == ordered_nodes[i])

      # Collect information into the 'node_properties' data frame
      node_properties[i, 1] <- ordered_nodes[i]
      node_properties[i, 2] <- labels[which(all_nodes %in% ordered_nodes[i])]
      node_properties[i, 3] <- ifelse(exists("type"),
                                      type[which(all_nodes %in% ordered_nodes[i])],
                                      rep(NA, length(ordered_nodes)))
      node_properties[i, 4] <- degree
      node_properties[i, 5] <- predecessors
      node_properties[i, 6] <- successors
      node_properties[i, 7] <- loops
    }

    return(node_properties)
  }
}
