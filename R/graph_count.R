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
#' # Count the number of graphs
#' # in the graph series
#' series %>%
#'   graph_count()
#' @export graph_count

graph_count <- function(graph_series) {

  if (class(graph_series) == "dgr_graph_1D") {
    if (is.null(graph_series$graphs)) {
      return(0)
    }
    return(length(graph_series$graphs))
  }
}
