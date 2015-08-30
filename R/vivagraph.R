#' Render graph with VivaGraphJS
#' @description Render a graph object with the VivaGraphJS library.
#' @param graph a \code{dgr_graph} object, created using the \code{create_graph}
#' function.
#' @param igrf \code{igraph} graph. We will try to smartly convert the
#' \code{igrf} using \code{get.data.frame(what = "both")}.
#' @param layout \code{string} or a layout function from \code{igraph}.
#' \code{"forceDirected"} the default and will use the \code{"forceDirected"}
#' layout engine from VivaGraphJS/ngraph.forceDirected. \code{"constant"} is the
#' other \code{string} option for layout. However, if desired, we can also take
#' advantage of a layout function from \code{igraph}, by providing this as our
#' \code{layout} parameter.
#' @param positions \code{data.frame} of two columns \code{x} and \code{y} with
#' fixed positions if you intend to provide preset positions for nodes.
#' @param config \code{list} of other config options. While currently this does
#' nothing, we expect to add additional configuration options here.
#' @param width \code{string} or \code{integer} with a valid CSS \code{width}
#' for the container for our htmlwidget.
#' @param height \code{string} or \code{integer} with a valid CSS \code{height}
#' for the container for our htmlwidget.
#' @param elementId \code{string} with a valid CSS \code{id}.
#'
#' @examples
#' \dontrun{
#' # Create a graph using the \code{create_nodes}, \code{create_edges},
#' # and \code{create_graph} functions
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                type = "letter",
#'                shape = sample(c("circle", "rectangle"),
#'                               length(LETTERS),
#'                               replace = TRUE),
#'                fillcolor = sample(c("aqua", "gray80",
#'                                     "pink", "lightgreen",
#'                                     "azure", "yellow"),
#'                                   length(LETTERS),
#'                                   replace = TRUE))
#'
#' edges <-
#'   create_edges(from = sample(LETTERS, replace = TRUE),
#'                to = sample(LETTERS, replace = TRUE),
#'                relationship = "letter_to_letter")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges,
#'                graph_attrs = "layout = neato",
#'                node_attrs = c("fontname = Helvetica",
#'                               "style = filled"),
#'                edge_attrs = c("color = gray20",
#'                               "arrowsize = 0.5"))
#'
#' # Render graph
#' vivagraph(graph = graph)
#'
#' # Using the igraph examples
#' library(igraph)
#'
#' vivagraph(igrf = graph.ring(10))
#'
#' vivagraph(erdos.renyi.game(100, 1/100))
#'
#' # Utilizing igraph layouts
#' vivagraph(erdos.renyi.game(100, 1/100), layout = layout.circle)
#' vivagraph(graph.famous("Zachary"), layout = layout.grid)
#' }
#' @export vivagraph

vivagraph <- function(graph = NULL,
                      igrf  = NULL,
                      layout = "forceDirected",
                      positions = NULL,
                      config = NULL,
                      height = NULL,
                      width = NULL,
                      elementId = NULL){

  if (inherits(graph, "dgr_graph")){

    nodes_df <- graph$nodes_df
    edges_df <- graph$edges_df


    #  if nodes_df provided then check to make sure there is a column named id
    #  if not then name the first column id
    if (is.data.frame(nodes_df)){

      if (nrow(nodes_df) > 0){

        if (!("id" %in% colnames(nodes_df))){

          colnames(nodes_df)[1] <- "id"
        }
      }
    }

    # Get data frame of node positions if it is provided in graph object
    if (all(c("x", "y") %in% colnames(nodes_df))){

      positions <-
        data.frame(x = nodes_df[, which(colnames(nodes_df) %in% "x")],
                   y = nodes_df[, which(colnames(nodes_df) %in% "y")])
    }

    # If 'edges_df' provided then check to make sure there is a column named from and to
    # if not then name the first column 'from' and name the second column 'to'
    if (is.data.frame(edges_df)){

      if (nrow(edges_df) > 0 && ncol(edges_df) > 1){

        if (!("from" %in% colnames(edges_df)) || !("to" %in% colnames(edges_df))){

          colnames(edges_df)[1] <- "from"
          colnames(edges_df)[2] <- "to"
        }

      } else if (ncol(edges_df) > 0 && ncol(edges_df) < 2){
        warning("vivagraph expects edges_df to contain at least two columns for source->target",
                call. = FALSE
        )
      }
    }

    # If 'nodes_df' is a vector then make it a data frame with column named 'id'
    if (is.vector(nodes_df)){

      nodes_df <- data.frame(id = nodes_df)
    }
  }

  # Check to see if 'graph' is an igraph object
  if (inherits(graph, "igraph")){

    # Try to make this easy if someone accidentally provides an igraph
    # as the first parameter

    igrf <- graph
    graph <- NULL
  }

  # If we are given an igraph, try to smartly convert using 'get.data.frame'
  if(inherits(igrf, "igraph")){

    # Assume if we are given an igraph that igraph is available
    igrf_df <- igraph::get.data.frame(igrf, what = "both")

    # warn if igraph provided as igrf and also nodes and edges
    if(!is.null(graph)) warning("overwriting nodes and edges with igraph igrf",
                                   call. = FALSE)

    if(ncol(igrf_df$vertices) > 0){

      nodes_df <-
        data.frame(id = igrf_df$vertices[,1],
                   igrf_df$vertices[,-1],
                   stringsAsFactors = FALSE)
    } else {

      # If there are no nodes then just take all unique values from edges
      nodes_df <-
        data.frame(id = unique(c(igrf_df$edges$from,
                                 igrf_df$edges$to)))
    }

    edges_df <- igrf_df$edges

    # If position is a function then assume a layout for igraph
    if (is.function(layout)){

      positions <-
        layout.norm(layout(igrf),
                    xmin = -1,
                    xmax = 1,
                    ymin = -1,
                    ymax = 1)

      positions <-
        data.frame(x = positions[,1] * 100,
                   y = -positions[,2] * 100)

      layout <- "constant"
    }
  }

  x <-
    list(network = list(nodes_df = nodes_df,
                        edges_df = edges_df),
         layout = layout,
         positions = positions,
         config = config)

  # Create widget
  htmlwidgets::createWidget(name = "vivagraph",
                            x = x,
                            width = width,
                            height = height,
                            package = "DiagrammeR",
                            elementId = elementId)
}
