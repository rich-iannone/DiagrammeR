#' Render graph with visNetwork
#' @description Render a graph object with the visNetwork R package.
#' @param graph a \code{dgr_graph} object, created using the
#' \code{create_graph} function.
#' @param nav_buttons a boolean value that determines whether navigation
#' buttons should be displayed along with the rendered graph.
#' @examples
#' \dontrun{
#' # Create a node data frame
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d", "e", "f"),
#'                label = TRUE,
#'                fillcolor = c("red", "red", "orange", "pink",
#'                          "purple", "black"),
#'                borderwidth = c(2, 1, 0.5, 1, 1.8, 1),
#'                value = c(2, 1, 0.5, 1, 1.8, 1),
#'                type = c("1", "1", "1", "2", "2", "2"),
#'                x = c(1, 2, 3, 4, 5, 6),
#'                y = c(-2, -1, 0, 6, 4, 1))
#'
#' # Create an edge data frame
#' edges <-
#'   create_edges(from = c("a", "b", "c", "d", "f", "e"),
#'                to = c("d", "c", "a", "c", "a", "d"),
#'                color = c("green", "green", "grey", "grey",
#'                          "blue", "blue"),
#'                relationship = "leading_to")
#'
#' # Create a graph object
#' graph <- create_graph(nodes_df = nodes,
#'                       edges_df = edges)
#'
#' visnetwork(graph)
#' }
#' @import visNetwork
#' @export visnetwork

visnetwork <- function(graph){

  # Extract node and edge data frames from the graph object
  nodes <- graph$nodes_df
  edges <- graph$edges_df

  # Modify names of columns in 'nodes' for compatibility with
  # visNetwork data frames for nodes
  colnames(nodes)[which(colnames(nodes) == "nodes")] <- "id"
  colnames(nodes)[which(colnames(nodes) == "type")] <- "group"
  colnames(nodes)[which(colnames(nodes) == "label")] <- "title"
  colnames(nodes)[which(colnames(nodes) == "fillcolor")] <- "color"

  # Modify names of columns in 'edges' for compatibility with
  # visNetwork data frames for edges
  colnames(edges)[which(colnames(edges) == "relationship")] <- "label"
  colnames(edges)[which(colnames(edges) == "label")] <- "title"
  colnames(edges)[which(colnames(edges) == "penwidth")] <- "value"

}
