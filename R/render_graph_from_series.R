#' Render a graph available in a series
#' @description Using a graph series object of type \code{dgr_graph_1D}, either render graph in the Viewer or output in various formats.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param graph_no the index of the graph in the graph series.
#' @param output a string specifying the output type; \code{graph} (the default) renders the graph using the \code{grViz} function, \code{DOT} outputs DOT code for the graph, and \code{SVG} provides SVG code for the rendered graph.
#' @param width an optional parameter for specifying the width of the resulting graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @export render_graph_from_series

render_graph_from_series <- function(graph_series,
                                     graph_no,
                                     output = "graph",
                                     width = NULL,
                                     height = NULL){

  # Stop function if no graphs are available
  if (is.null(graph_series$graphs)){

    message("There are no graphs in this graph series.")
    return(NULL)
  }

  # Stop function if 'graph_no' out of range
  if (!(graph_no %in% 1:graph_count(graph_series))){

    message("The index chosen doesn't correspond to that of a graph in the series.")
    return(NULL)
  }

  # Extract the specified graph from the series
  graph <- graph_series$graphs[[graph_no]]

  render_graph(graph = graph,
               output = output,
               width = width,
               height = height)
}
