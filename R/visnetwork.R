#' Render graph with visNetwork
#' @description Render a graph object with the visNetwork R package.
#' @param graph a \code{dgr_graph} object, created using the
#' \code{create_graph} function.
#' @param nav_buttons a boolean value that determines whether navigation
#' buttons should be displayed along with the rendered graph.
#' @import visNetwork
#' @export visnetwork

visnetwork <- function(graph,

  # Extract node and edge data frames from the graph object
  nodes <- graph$nodes_df
  edges <- graph$edges_df

  # Modify names of columns in 'nodes' for compatibility with
  # visNetwork data frames for nodes
  colnames(nodes)[which(colnames(nodes) == "nodes")] <- "id"
  colnames(nodes)[which(colnames(nodes) == "type")] <- "group"
  colnames(nodes)[which(colnames(nodes) == "fillcolor")] <- "color"

  # Modify names of columns in 'edges' for compatibility with
  # visNetwork data frames for edges
  colnames(edges)[which(colnames(edges) == "relationship")] <- "label"
  colnames(edges)[which(colnames(edges) == "label")] <- "title"
  colnames(edges)[which(colnames(edges) == "penwidth")] <- "value"

  # Render the graph
  visNetwork(nodes = nodes, edges = edges) %>%
    visEdges(arrows = 'to', color = 'gray') %>%
    visInteraction(navigationButtons = TRUE)
}
