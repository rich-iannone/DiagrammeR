#' Remove a graph from a graph series
#' @description Remove a single graph object from an
#' set of graph objects contained within a graph series
#' object.
#' @param graph_series a graph series object from which
#' the graph object will be removed.
#' @param index the index of the graph object to be
#' removed from the graph series object.
#' @return a graph series object of type
#' \code{dgr_graph_1D}.
#' @examples
#' # Create three graphs
#' graph_1 <-
#'   create_graph() %>%
#'   add_n_nodes(n = 3) %>%
#'   add_edges_w_string(
#'     "1->3 1->2 2->3")
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
#' # Remove the second graph (with the name `remove`)
#' # from the graph series
#' series <-
#'   remove_from_series(
#'     graph_series = series,
#'     index = 2)
#'
#' # Use `series_info()` function to ensure that
#' # the graph with the name `remove` was removed
#' series_info(series)
#' #>   graph name date_time   tz nodes edges directed
#' #> 1     1 <NA>      <NA> <NA>     3     3     TRUE
#' #> 2     2 <NA>      <NA> <NA>     5     5     TRUE
#' @export remove_from_series

remove_from_series <- function(graph_series,
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
