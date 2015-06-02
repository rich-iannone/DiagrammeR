#' Get detailed information on nodes
#' @description Obtain a data frame with detailed information on nodes and their interrelationships within a graph.
#' @param graph a graph object of class \code{dgr_graph}.
#' @return a data frame containing information specific to each node within the graph.
#' @export node_info

node_info <- function(graph){

  # If graph is empty, return an empty data frame
  if (is_graph_empty(graph = graph) == TRUE){

    node_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 7))
    colnames(node_properties) <- c("node_ID", "label", "type", "degree",
                                   "indegree", "outdegree", "loops")

    return(node_properties)
  }

  if ("from" %in% colnames(graph$edges_df)){
    edge_from <- graph$edges_df$from
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
                                   "indegree", "outdegree", "loops")

    node_properties[, 1] <- all_nodes
    node_properties[, 2] <- labels

    if (exists("type")){
      node_properties[, 3] <- type
    } else {
      node_properties[, 3] <- rep(NA, length(all_nodes))
    }

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
                                       "indegree", "outdegree", "loops")
      }

      #
      # Get degree for each node
      #

      degree <- sum(c(graph$edges_df$from, graph$edges_df$to) %in%
                      ordered_nodes[i])
      #
      # Get indegree for each node
      #

      if (ordered_nodes[i] %in% top_nodes | degree == 0){
        indegree <- 0
      }

      if (!(ordered_nodes[i] %in% top_nodes) & degree != 0){

        for (j in 1:sum(edge_to %in% ordered_nodes[i])){

          if (j == 1) indegree <- vector(mode = "character")

          indegree <- c(indegree, edge_from[which(edge_to %in% ordered_nodes[i])[j]])
        }

        indegree <- length(indegree)
      }

      #
      # Get outdegree for each node
      #

      if (ordered_nodes[i] %in% bottom_nodes | degree == 0){
        outdegree <- 0
      }

      if (!(ordered_nodes[i] %in% bottom_nodes) & degree != 0){

        for (j in 1:sum(edge_from %in% ordered_nodes[i])){

          if (j == 1) outdegree <- vector(mode = "character")

          outdegree <- c(outdegree, edge_from[which(edge_from %in% ordered_nodes[i])[j]])
        }

        outdegree <- length(outdegree)
      }

      #
      # Get number of loops for each node
      #

      loops <- sum(graph$edges_df$from == graph$edges_df$to &
                     graph$edges_df$to == ordered_nodes[i])

      # Collect information into the 'node_properties' data frame
      node_properties[i, 1] <- ordered_nodes[i]
      node_properties[i, 2] <- ifelse(!is.null(labels[which(all_nodes %in% ordered_nodes[i])]),
                                      labels[which(all_nodes %in% ordered_nodes[i])],
                                      NA)
      node_properties[i, 3] <- ifelse(exists("type"),
                                      type[which(all_nodes %in% ordered_nodes[i])],
                                      rep(NA, length(ordered_nodes)))
      node_properties[i, 4] <- degree
      node_properties[i, 5] <- indegree
      node_properties[i, 6] <- outdegree
      node_properties[i, 7] <- loops
    }

    return(node_properties)
  }
}
