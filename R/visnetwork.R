#' Render graph with visNetwork
#' @description Render a graph object with the visNetwork R package.
#' @param graph a \code{dgr_graph} object, created using the
#' \code{create_graph} function.
#' @param nav_buttons a boolean value that determines whether navigation
#' buttons should be displayed along with the rendered graph.
#' @import visNetwork
#' @export visnetwork

visnetwork <- function(graph,
                       nav_buttons = TRUE){

  # Change all X11 colors to hexadecimal values for node colors
  # colors to hexadecimal
  x11_to_hex <- function(graph){

    for (i in 1:length(graph$nodes_df$color)){

      if (i == 1) hex_color_values <- vector(mode = 'character', length = 0)

      a_hex_color <- x11_hex()[which(x11_hex()[,1] %in% tolower(graph$nodes_df$color[i])),2]

      hex_color_values <- c(hex_color_values, a_hex_color)
    }

    graph$nodes_df$color <- hex_color_values

    return(graph)
  }

  graph <- x11_to_hex(graph = graph)

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
    visEdges(arrow = 'to', color = 'gray') %>%
    visInteraction(navigationButtons = TRUE)
}
