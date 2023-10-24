#' Render graph with visNetwork
#'
#' @description
#'
#' Render a graph object with the visNetwork R package.
#'
#' @inheritParams render_graph
#'
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 6,
#'     label = TRUE,
#'     fillcolor = c("lightgrey", "red", "orange",
#'                   "pink", "aqua", "yellow"),
#'     shape = "dot",
#'     size = c(20, 80, 40, 10, 30, 50),
#'     type = c("1", "1", "1", "2", "2", "2")
#'   )
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3, 4, 6, 5),
#'     to = c(4, 3, 1, 3, 1, 4),
#'     color = c("green", "green", "grey",
#'               "grey", "blue", "blue"),
#'     rel = "leading_to"
#'   )
#'
#' # Create a graph object
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf
#'   )
#'
#' # visnetwork(graph)
#'
#' @export
visnetwork <- function(graph) {

  # Extract node and edge data frames from the graph object
  nodes <- graph %>% get_node_df()
  edges <- graph %>% get_edge_df()

  # Render an empty graph if no nodes or edges exist
  if (graph %>% is_graph_empty()) {

    nodes <- create_node_df(n = 1)
    nodes <- nodes[-1, ]

    edges <- create_edge_df(from = 1, to = 1)
    edges <- edges[-1, ]
  }

  # Remove the 'pos' column, if it exists
  if ("pos" %in% colnames(nodes)) {
    nodes <- nodes[, -(which(colnames(nodes) %in% "pos"))]
  }

  # Modify names of columns in `nodes` for compatibility with
  # visNetwork data frames for nodes
  colnames(nodes)[which(colnames(nodes) == "nodes")] <- "id"
  colnames(nodes)[which(colnames(nodes) == "type")] <- "group"
  colnames(nodes)[which(colnames(nodes) == "tooltip")] <- "title"
  colnames(nodes)[which(colnames(nodes) == "fillcolor")] <- "color"

  # Modify names of columns in 'edges' for compatibility with
  # visNetwork data frames for edges
  colnames(edges)[which(colnames(edges) == "rel")] <- "label"
  colnames(edges)[which(colnames(edges) == "tooltip")] <- "title"
  colnames(edges)[which(colnames(edges) == "penwidth")] <- "width"

  # Obtain `fontcolor` values if the column exists in `edges`
  if ("fontcolor" %in% colnames(edges)) {
    fontcolor <- edges[, -(which(colnames(edges) %in% "fontcolor"))]
  }

  # Create the visNetwork object
  if (!all(c("x", "y") %in% colnames(nodes))) {

    if (nrow(graph$edges_df) == 0) {

      vn_obj <- visNetwork(nodes = nodes)
    }

    if (nrow(graph$edges_df) > 0) {

      vn_obj <- visNetwork(nodes = nodes, edges = edges)

      if (is_graph_directed(graph)) {

        vn_obj <-
          visEdges(
            graph = vn_obj,
            arrows = list(
              to = list(
                enabled = TRUE,
                scaleFactor = 1)))
      }

      if (is_graph_undirected(graph)) {

        vn_obj <-
          visEdges(
            graph = vn_obj,
            arrows = list(
              to = list(
                enabled = FALSE,
                scaleFactor = 1)))
      }
    }

    vn_obj <-
      visPhysics(
        graph = vn_obj,
        solver = "barnesHut",
        stabilization = list(
          enabled = TRUE,
          onlyDynamicEdges = FALSE,
          fit = TRUE))

    vn_obj <-
      visLayout(
        graph = vn_obj,
        improvedLayout = TRUE)
  }

  if (all(c("x", "y") %in% colnames(nodes))) {

    # Reverse y values
    nodes$y <- -as.numeric(nodes$y)

    if (is.null(graph$edges_df)) {

      vn_obj <- visNetwork(nodes = nodes)

      vn_obj <-
        visNodes(
          graph = vn_obj,
          physics = FALSE,
          fixed = FALSE)

      vn_obj <-
        visPhysics(
          graph = vn_obj,
          stabilization = list(
            enabled = FALSE,
            onlyDynamicEdges = FALSE,
            fit = TRUE))

      vn_obj <-
        visInteraction(
          graph = vn_obj,
          dragNodes = FALSE)
    }

    if (nrow(graph$edges_df) > 0) {

      if ("arrow" %in% colnames(edges)) {

        if (all(edges[which(colnames(edges) %in% "arrow")] == FALSE)) {

          arrows_for_edges <- FALSE
        } else {
          arrows_for_edges <- FALSE
        }
      } else {
        arrows_for_edges <- FALSE
      }

      vn_obj <-
        visNetwork(
          nodes = nodes,
          edges = edges)

      vn_obj <-
        visNodes(
          graph = vn_obj,
          physics = FALSE,
          fixed = FALSE)

      vn_obj <-
        visEdges(
          graph = vn_obj,
          arrows = list(
            to =
              list(
                enabled = ifelse(arrows_for_edges, TRUE, FALSE),
                scaleFactor = 1)),
          smooth = FALSE,
          font = list(
            color = "#343434",
            size = 14,
            face = "arial",
            background = NULL,
            strokeWidth = 2,
            strokeColor = "#ffffff",
            align = "middle"))

      vn_obj <-
        visPhysics(
          graph = vn_obj,
          stabilization = list(
            enabled = TRUE,
            onlyDynamicEdges = FALSE,
            fit = TRUE))

      vn_obj <-
        visInteraction(
          graph = vn_obj,
          dragNodes = FALSE)
    }
  }

  vn_obj
}
