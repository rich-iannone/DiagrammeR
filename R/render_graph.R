#' Render the graph in various formats
#' @description Using a \code{dgr_graph} object,
#' render the graph in the RStudio Viewer.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param output a string specifying the output type;
#' \code{graph} (the default) renders the graph using
#' the \code{grViz} function and \code{visNetwork}
#' renders the graph using the \code{visnetwork} function.
#' @param title an optional title for a graph when
#' using \code{output = "graph"}.
#' @param width an optional parameter for specifying
#' the width of the resulting graphic in pixels.
#' @param height an optional parameter for specifying
#' the height of the resulting graphic in pixels.
#' @examples
#' \dontrun{
#' # Set a seed
#' set.seed(23)
#'
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 26,
#'     type = "basic",
#'     shape = sample(c("circle", "square"),
#'                    length(1:26),
#'                    replace = TRUE),
#'     fillcolor = sample(c("aqua", "orange",
#'                          "pink", "lightgreen",
#'                          "black", "yellow"),
#'                        length(1:26),
#'                        replace = TRUE))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = sample(1:26, replace = TRUE),
#'     to = sample(1:26, replace = TRUE),
#'     rel = "to_node")
#'
#' # Create a graph object using the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Render the graph using Graphviz
#' render_graph(graph)
#'
#' # Render the graph using visNetwork
#' render_graph(graph, output = "visNetwork")
#' }
#' @export render_graph

render_graph <- function(graph,
                         output = NULL,
                         title = NULL,
                         width = NULL,
                         height = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  if (is.null(output)) {
    output <- "graph"
  }

  if (output == "graph") {

    if (!is.null(title)) {

      graph <-
        add_global_graph_attrs(
          graph, "label", title, "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labelloc", "t", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "labeljust", "c", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontname", "Helvetica", "graph")

      graph <-
        add_global_graph_attrs(
          graph, "fontcolor", "gray30", "graph")
    }

    dot_code <- generate_dot(graph)

    grVizObject <-
      grViz(
        diagram = dot_code,
        engine = layout,
        width = width,
        height = height)

    grVizObject

  } else if (output == "visNetwork") {

    visnetwork(graph)

  }
}
