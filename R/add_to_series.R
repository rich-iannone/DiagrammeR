#' Add graph object to a graph series object
#' @description Add a graph object to an extant graph
#' series object for storage of multiple graphs across
#' a sequential or temporal one-dimensional array.
#' @param graph a graph object to add to the graph
#' series object.
#' @param graph_series a graph series object to which
#' the graph object will be added.
#' @return a graph series object of type
#' \code{dgr_graph_1D}.
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
#' @export add_to_series

add_to_series <- function(graph,
                          graph_series) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get the series type
  series_type <- graph_series$series_type

  # Stop function if graph series type is not valid
  if (!(series_type %in%
        c("sequential", "temporal"))) {
    stop("The graph series type is neither of the `sequential` nor `temporal` types.")
  }

  # Add graph to series
  graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

  graph_series
}
