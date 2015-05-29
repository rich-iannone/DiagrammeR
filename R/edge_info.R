#' Get detailed information on edges
#' @description Obtain a data frame with detailed information on edges and their interrelationships within a graph.
#' @param graph a graph object of class \code{dgr_graph}.
#' @return a data frame containing information specific to each edge within the graph.
#' @export edge_info

edge_info <- function(graph){

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

  if ("relationship" %in% colnames(graph$edges_df)){
    relationship <- graph$edges_df$relationship
  }

  # For graphs with no edges, create an 'edge_properties' data frame that doesn't
  # need to consider any edge information
  if (is.null(graph$edges_df)){

    edge_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 4))
    colnames(edge_properties) <- c("edge_from", "edge_to", "relationship",
                                   "label")

    return(edge_properties)
  }

  # For graphs with no edges, create an 'edge_properties' data frame
  if (!is.null(graph$edges_df)){

    # Create data frame of edge properties
    for (i in 1:length(edge_from)){

      if (i == 1){
        edge_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 4))
        colnames(edge_properties) <- c("edge_from", "edge_to", "relationship",
                                       "label")
      }

      # Collect information into the 'edge_properties' data frame
      edge_properties[i, 1] <- edge_from[i]
      edge_properties[i, 2] <- edge_to[i]
      edge_properties[i, 3] <- ifelse(exists("relationship"),
                                      relationship[which((edge_from %in% edge_from[i]) &
                                                           (edge_to %in% edge_to[i]))],
                                      rep(NA, length(edge_from)))
      edge_properties[i, 4] <- ifelse(exists("label"),
                                      label[which((edge_from %in% edge_from[i]) &
                                                    (edge_to %in% edge_to[i]))],
                                      rep(NA, length(edge_from)))
    }

    return(edge_properties)
  }
}
