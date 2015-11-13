#' Render the graph or output in various formats
#' @description Using a \code{dgr_graph} object, either render graph in the
#' Viewer or output in various formats.
#' @param graph a \code{dgr_graph} object, created using the
#' \code{create_graph} function.
#' @param output a string specifying the output type; \code{graph} (the
#' default) renders the graph using the \code{grViz} function, \code{vivagraph}
#' renders the graph using the \code{vivagraph} function, \code{visNetwork}
#' renders the graph using the \code{visnetwork} function, and \code{DOT}
#' outputs DOT code for the graph.
#' @param layout a string specifying a layout type for a \code{vivagraph}
#' rendering of the graph, either \code{forceDirected} or \code{constant}.
#' @param width an optional parameter for specifying the width of the resulting
#' graphic in pixels.
#' @param height an optional parameter for specifying the height of the
#' resulting graphic in pixels.
#' @examples
#' \dontrun{
#' # Create a graph and then view it in the RStudio Viewer
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                label = TRUE,
#'                type = "letter",
#'                shape = sample(c("circle", "square"),
#'                               length(LETTERS),
#'                               replace = TRUE),
#'                fillcolor = sample(c("aqua", "orange",
#'                                     "pink", "lightgreen",
#'                                     "black", "yellow"),
#'                                   length(LETTERS),
#'                                   replace = TRUE))
#'
#' edges <-
#'   create_edges(from = sample(LETTERS, replace = TRUE),
#'                to = sample(LETTERS, replace = TRUE),
#'                rel = "letter_to_letter")
#'
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges,
#'                graph_attrs = "layout = twopi",
#'                node_attrs = c("fontname = Helvetica",
#'                               "style = filled"),
#'                edge_attrs = c("color = gray20",
#'                               "arrowsize = 0.5"))
#'
#' # Render the graph using Graphviz
#' render_graph(graph)
#'
#' # Render the graph using VivaGraph
#' render_graph(graph, output = "vivagraph")
#'
#' # Render the graph using visNetwork
#' render_graph(graph, output = "visNetwork")
#' }
#' @export render_graph

render_graph <- function(graph,
                         output = NULL,
                         layout = NULL,
                         width = NULL,
                         height = NULL){

  stopifnot(class(graph) == "dgr_graph")

  if (is.null(output) & !is.null(graph$graph_attrs)){

    if ("output = visNetwork" %in% graph$graph_attrs){
     output <- "visNetwork"
    }

    if ("output = vivagraph" %in% graph$graph_attrs){
      output <- "vivagraph"
    }

    if ("output = graph" %in% graph$graph_attrs){
      output <- "graph"
    }

    if ("output = Graphviz" %in% graph$graph_attrs){
      output <- "graph"
    }
  }

  if (is.null(output)){
    output <- "graph"
  }

  if (output == "DOT"){
    return(graph$dot_code)
  }

  if (output == "graph" & is.null(graph$dot_code)){

    graph <- create_graph(nodes_df = graph$nodes_df,
                          edges_df = graph$edges_df,
                          graph_attrs = graph$graph_attrs,
                          node_attrs = graph$node_attrs,
                          edge_attrs = graph$edge_attrs,
                          directed = graph$directed,
                          graph_name = graph$graph_name,
                          graph_time = graph$graph_time,
                          graph_tz = graph$graph_tz,
                          generate_dot = TRUE)
  }

  if (output == "vivagraph"){

    layout <- ifelse(is.null(layout) & node_count(graph) < 1000,
                     "forceDirected", "constant")

    vivagraph(graph = graph,
              layout = layout,
              height = NULL,
              width = NULL)

  } else if (output == "visNetwork"){

    visnetwork(graph)

  } else if (output == "graph"){

    grViz(diagram = graph$dot_code,
          engine = layout,
          width = width,
          height = height)
  }
}
