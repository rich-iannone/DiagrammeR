#' Render a graph available in a series
#'
#' Using a graph series object of type `dgr_graph_1D`, either render graph in
#' the Viewer or output in various formats.
#'
#' @param graph_series A graph series object of type `dgr_graph_1D`.
#' @param graph_no The index of the graph in the graph series.
#' @param output A string specifying the output type; `graph` (the default)
#'   renders the graph using the [grViz()] function and `visNetwork` renders the
#'   graph using the [visnetwork()] function.
#' @param width An optional parameter for specifying the width of the resulting
#'   graphic in pixels.
#' @param height An optional parameter for specifying the height of the
#'   resulting graphic in pixels.
#' @examples
#' \dontrun{
#' # Create three graphs
#' graph_1 <-
#'   create_graph() %>%
#'   add_path(n = 4)
#'
#' graph_2 <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' graph_3 <-
#'   create_graph() %>%
#'   add_star(n = 6)
#'
#' # Create an empty graph series
#' # and add the graphs
#' series <-
#'   create_graph_series() %>%
#'   add_graph_to_graph_series(
#'     graph = graph_1) %>%
#'   add_graph_to_graph_series(
#'     graph = graph_2) %>%
#'   add_graph_to_graph_series(
#'     graph = graph_3)
#'
#' # View the second graph in
#' # the series in the Viewer
#' render_graph_from_graph_series(
#'   graph_series = series,
#'   graph_no = 2)
#' }
#' @export
render_graph_from_graph_series <- function(graph_series,
                                           graph_no,
                                           output = "graph",
                                           width = NULL,
                                           height = NULL) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Stop function if no graphs are available
  if (is.null(graph_series$graphs)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There are no graphs in this graph series")
  }

  # Stop function if `graph_no` is out of range
  if (!(graph_no %in% 1:count_graphs_in_graph_series(graph_series))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The index chosen doesn't correspond to that of a graph in the series")
  }

  # Extract the specified graph from the series
  graph <- graph_series$graphs[[graph_no]]

  render_graph(
    graph = graph,
    output = output,
    width = width,
    height = height)
}
