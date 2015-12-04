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
#' @importFrom visNetwork visNetwork visInteraction visEdges visPhysics visNodes visLayout
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

  if (!is.null(graph$edges_df)){

    # Modify names of columns in 'edges' for compatibility with
    # visNetwork data frames for edges
    colnames(edges)[which(colnames(edges) == "rel")] <- "label"
    colnames(edges)[which(colnames(edges) == "tooltip")] <- "title"
    colnames(edges)[which(colnames(edges) == "penwidth")] <- "width"

    # Obtain 'fontcolor' values if the column exists in 'edges'
    if ("fontcolor" %in% colnames(edges)){
      fontcolor <- edges[,-(which(colnames(edges) %in% "fontcolor"))]
    }
  }

  # Create the visNetwork object
  if (all(c("x", "y") %in% colnames(nodes)) == FALSE){

    if (is.null(graph$edges_df)){

      vn_obj <- visNetwork(nodes = nodes)
    }

    if (!is.null(graph$edges_df)){

      vn_obj <- visNetwork(nodes = nodes, edges = edges)

      if (is_graph_directed(graph)){
        vn_obj <- visEdges(graph = vn_obj,
                           arrows = list(to = list(enabled = TRUE,
                                                   scaleFactor = 1)))
      }

      if (is_graph_directed(graph) == FALSE){
        vn_obj <- visEdges(graph = vn_obj,
                           arrows = list(to = list(enabled = FALSE,
                                                   scaleFactor = 1)))
      }
    }

    vn_obj <- visPhysics(graph = vn_obj,
                         solver = "forceAtlas2Based",
                         forceAtlas2Based = list(gravitationalConstant = -10))

    vn_obj <- visPhysics(graph = vn_obj,
                         stabilization = list(enabled = TRUE,
                                              onlyDynamicEdges = FALSE,
                                              fit = TRUE))

    vn_obj <- visLayout(graph = vn_obj,
                        improvedLayout = TRUE)
  }

  if (all(c("x", "y") %in% colnames(nodes))){

    # Reverse y values
    nodes$y <- -as.numeric(nodes$y)

    if (is.null(graph$edges_df)){

      vn_obj <- visNetwork(nodes = nodes)

      vn_obj <- visNodes(graph = vn_obj,
                         physics = FALSE,
                         fixed = FALSE)

      vn_obj <- visPhysics(graph = vn_obj,
                           stabilization = list(enabled = FALSE,
                                                onlyDynamicEdges = FALSE,
                                                fit = TRUE))

      vn_obj <- visInteraction(graph = vn_obj,
                               dragNodes = FALSE)

    }

    if (!is.null(graph$edges_df)){

      if ("arrow" %in% colnames(edges)){
        if (all(edges[which(colnames(edges) %in% "arrow")] == FALSE)){
          arrows_for_edges <- FALSE
        } else {
          arrows_for_edges <- FALSE
        }
      } else {
        arrows_for_edges <- FALSE
      }

      vn_obj <- visNetwork(nodes = nodes, edges = edges)

      vn_obj <- visNodes(graph = vn_obj,
                         physics = FALSE,
                         fixed = FALSE)

      vn_obj <-
        visEdges(graph = vn_obj,
                 arrows = list(to =
                                 list(enabled =
                                        ifelse(arrows_for_edges,
                                               TRUE, FALSE),
                                      scaleFactor = 1)),
                 smooth = FALSE,
                 font = list(color = "#343434",
                             size = 14,
                             face = "arial",
                             background = NULL,
                             strokeWidth = 2,
                             strokeColor = "#ffffff",
                             align = "middle"))

      vn_obj <-
        visPhysics(graph = vn_obj,
                   stabilization = list(enabled = FALSE,
                                        onlyDynamicEdges = FALSE,
                                        fit = TRUE))

      vn_obj <- visInteraction(graph = vn_obj,
                               dragNodes = FALSE)
    }
  }

  return (vn_obj)
}
