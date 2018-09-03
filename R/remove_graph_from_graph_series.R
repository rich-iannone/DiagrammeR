#' Remove a graph from a graph series
#'
#' Remove a single graph object from an set of graph objects contained within a
#'   graph series object.
#' @param graph_series a graph series object from which the graph object will be
#'   removed.
#' @param index the index of the graph object to be removed from the graph
#'   series object.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @examples
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
#' # Remove the second graph
#' # from the graph series
#' series <-
#'   series %>%
#'   remove_graph_from_graph_series(
#'     index = 2)
#'
#' # With `get_graph_series_info()`,
#' # we can ensure that a graph
#' # was removed
#' series %>%
#'   get_graph_series_info()
#' @export
remove_graph_from_graph_series <- function(graph_series,
                                           index = "last") {

  if (index == "last") {
    graph_series$graphs[[length(graph_series$graphs)]] <- NULL
    return(graph_series)
  }

  if (index == "first") {
    graph_series$graphs[[1]] <- NULL
    return(graph_series)
  }

  if (inherits(index, "numeric") | inherits(index, "integer")) {
    graph_series$graphs[[index]] <- NULL
    return(graph_series)
  }
}
