#' Render graph with VivaGraphJS
#' @description Render a graph object with the VivaGraphJS library.
#' @param graph a \code{dgr_graph} object, created using the \code{create_graph}
#' function.
#' @param layout a \code{string} where \code{"forceDirected"} is the default
#' whereas \code{"constant"} is another layout option.
#' @param positions \code{data.frame} of two columns \code{x} and \code{y} with
#' fixed positions if you intend to provide preset positions for nodes.
#' @param config \code{list} of other config options. While currently this does
#' nothing, we expect to add additional configuration options here.
#' @param width \code{string} or \code{integer} with a valid CSS \code{width}
#' for the container for our htmlwidget.
#' @param height \code{string} or \code{integer} with a valid CSS \code{height}
#' for the container for our htmlwidget.
#' @param elementId \code{string} with a valid CSS \code{id}.
#' @examples
#' \dontrun{
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 6,
#'     label = TRUE,
#'     fillcolor = c("lightgrey", "red", "orange",
#'                   "pink", "aqua", "yellow"),
#'     shape = "circle",
#'     value = c(2, 1, 0.5, 1, 1.8, 1),
#'     type = c("1", "1", "1", "2", "2", "2"),
#'     x = c(1, 2, 3, 4, 5, 6),
#'     y = c(-2, -1, 0, 6, 4, 1))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3, 4, 6, 5),
#'     to = c(4, 3, 1, 3, 1, 4),
#'     color = c("green", "green", "grey",
#'               "grey", "blue", "blue"),
#'     rel = "leading_to")
#'
#' # Create a graph object
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' vivagraph(graph)
#' }
#' @export vivagraph

vivagraph <- function(graph = NULL,
                      layout = "forceDirected",
                      positions = NULL,
                      config = NULL,
                      height = NULL,
                      width = NULL,
                      elementId = NULL) {

  if (inherits(graph, "dgr_graph")) {

    nodes_df <- graph$nodes_df
    edges_df <- graph$edges_df

    # If nodes_df provided then check to make sure
    # there is a column named id if not then name the
    # first column id
    if (is.data.frame(nodes_df)) {

      if (nrow(nodes_df) > 0) {

        if (!("id" %in% colnames(nodes_df))) {

          colnames(nodes_df)[1] <- "id"
        }
      }
    }

    # Get data frame of node positions if it is
    # provided in graph object
    if (all(c("x", "y") %in% colnames(nodes_df))) {

      positions <-
        data.frame(
          x = nodes_df[, which(colnames(nodes_df) %in% "x")],
          y = nodes_df[, which(colnames(nodes_df) %in% "y")])
    }

    # If 'edges_df' provided then check to make sure
    # there is a column named from and to if not
    # then name the first column 'from' and name the
    # second column 'to'
    if (is.data.frame(edges_df)) {

      if (nrow(edges_df) > 0 && ncol(edges_df) > 1) {

        if (!("from" %in% colnames(edges_df)) ||
            !("to" %in% colnames(edges_df))) {

          colnames(edges_df)[1] <- "from"
          colnames(edges_df)[2] <- "to"
        }

      } else if (ncol(edges_df) > 0 && ncol(edges_df) < 2) {
        warning("vivagraph expects edges_df to contain at least two columns for source->target",
                call. = FALSE
        )
      }
    }

    # If 'nodes_df' is a vector then make it a
    # data frame with column named 'id'
    if (is.vector(nodes_df)) {
      nodes_df <- data.frame(id = nodes_df)
    }
  }

  x <-
    list(network = list(nodes_df = nodes_df,
                        edges_df = edges_df),
         layout = layout,
         positions = positions,
         config = config)

  # Create widget
  htmlwidgets::createWidget(
    name = "vivagraph",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR",
    elementId = elementId)
}
