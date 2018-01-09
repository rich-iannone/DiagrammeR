#' Get a graph available in a series
#' @description Using a graph series object of type
#' \code{dgr_graph_1D}, get a graph object.
#' @param graph_series a graph series object of type
#' \code{dgr_graph_1D}.
#' @param graph_no the index of the graph in the graph
#' series.
#' @examples
#' # Create three graphs
#' graph_1 <-
#'   create_graph() %>%
#'   add_n_nodes(n = 3) %>%
#'   add_edges_w_string(
#'     edges = "1->3 1->2 2->3")
#'
#' graph_2 <-
#'   graph_1 %>%
#'   add_node() %>%
#'   add_edge(from = 4, to = 3)
#'
#' graph_3 <-
#'   graph_2 %>%
#'   add_node() %>%
#'   add_edge(from = 5, to = 2)
#'
#' # Create an empty graph series and add
#' # the graphs
#' series <-
#'   create_series() %>%
#'   add_to_series(graph_1, .) %>%
#'   add_to_series(graph_2, .) %>%
#'   add_to_series(graph_3, .)
#'
#' # Get the second graph in the series
#' extracted_graph <-
#'   get_graph_from_series(
#'     graph_series = series,
#'     graph_no = 2)
#' @export get_graph_from_series

get_graph_from_series <- function(graph_series,
                                  graph_no) {

  # Stop function if no graphs are available
  if (is.null(graph_series$graphs)) {

    stop(
      "There are no graphs in this graph series.",
      call. = FALSE)
  }

  # Stop function if `graph_no` out of range
  if (!(graph_no %in% 1:graph_count(graph_series))) {

    stop(
      "The index chosen doesn't correspond to that of a graph in the series.",
      call. = FALSE)
  }

  # Extract the specified graph from the series
  graph_series$graphs[[graph_no]]
}
