#' Render graph with visNetwork
#' @description Render a graph object with the visNetwork R package.
#' @param graph a \code{dgr_graph} object, created using the
#' \code{create_graph} function.
#' @examples
#' \dontrun{
#' # Create a node data frame
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d", "e", "f"),
#'                label = TRUE,
#'                fillcolor = c("lightgrey", "red", "orange", "pink",
#'                          "aqua", "yellow"),
#'                shape = "circle",
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
#'                rel = "leading_to")
#'
#' # Create a graph object
#' graph <- create_graph(nodes_df = nodes,
#'                       edges_df = edges)
#'
#' visnetwork(graph)
#' }
#' @importFrom visNetwork visNetwork visEdges visPhysics
#' @export visnetwork

visnetwork <- function(graph){

  # Extract node and edge data frames from the graph object
  if (!is.null(graph$nodes_df)){

    nodes <- graph$nodes_df
  }

  if (!is.null(graph$edges_df)){

    edges <- graph$edges_df
  }

  # Render an empty graph if no nodes or edges exist
  if (is.null(graph$nodes_df) & is.null(graph$edges_df)){

    nodes <- create_nodes(nodes = "")
    nodes <- nodes[,-1]

    edges <- create_edges(from = "", to = "")
    edges <- edges[,-1]
  }

  # Remove the 'pos' column, if it exists
  if ("pos" %in% colnames(nodes)){
    nodes <- nodes[,-(which(colnames(nodes) %in% "pos"))]
  }

  # Modify names of columns in 'nodes' for compatibility with
  # visNetwork data frames for nodes
  colnames(nodes)[which(colnames(nodes) == "nodes")] <- "id"
  colnames(nodes)[which(colnames(nodes) == "type")] <- "group"
  colnames(nodes)[which(colnames(nodes) == "tooltip")] <- "title"
  colnames(nodes)[which(colnames(nodes) == "fillcolor")] <- "color"

  # Modify names of columns in 'edges' for compatibility with
  # visNetwork data frames for edges
  colnames(edges)[which(colnames(edges) == "rel")] <- "label"
  colnames(edges)[which(colnames(edges) == "tooltip")] <- "title"
  colnames(edges)[which(colnames(edges) == "penwidth")] <- "value"

  # Create the visNetwork object
  vn_obj <- visNetwork(nodes = nodes, edges = edges)

  vn_obj <- visEdges(graph = vn_obj,
                     arrows = list(to = list(enabled = TRUE,
                                             scaleFactor = 1)))

  vn_obj <- visPhysics(graph = vn_obj,
                       solver = "forceAtlas2Based",
                       forceAtlas2Based = list(gravitationalConstant = -10))

  vn_obj <- visPhysics(graph = vn_obj,
                       stabilization = list(enabled = TRUE,
                                            onlyDynamicEdges = FALSE,
                                            fit = TRUE))

  vn_obj
}
