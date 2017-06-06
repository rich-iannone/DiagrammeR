#' Count graphs in a graph series object
#' @description Counts the total number of graphs in a
#' graph series object.
#' @param graph_series a graph series object of type
#' \code{dgr_graph_1D}
#' @return a numeric vector representing a count of
#' graphs in a graph series object.
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
#' # Count the number of graphs in the graph series
#' graph_count(series)
#' #> [1] 3
#' @export graph_count

graph_count <- function(graph_series) {

  if (class(graph_series) == "dgr_graph_1D") {
    if (is.null(graph_series$graphs)) {
      return(0)
    }
    return(length(graph_series$graphs))
  }
}
