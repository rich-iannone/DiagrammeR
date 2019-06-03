#' Add graph object to a graph series object
#'
#' Add a graph object to an extant graph series object for storage of multiple
#' graphs across a sequential or temporal one-dimensional array.
#' @param graph_series a graph series object to which the graph object will be
#'   added.
#' @param graph a graph object to add to the graph series object.
#' @return a graph series object of type `dgr_graph_1D`.
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
#'   count_graphs_in_graph_series()
#' @export
add_graph_to_graph_series <- function(graph_series,
                                      graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Get the series type
  series_type <- graph_series$series_type

  # Stop function if graph series type is not valid
  if (!(series_type %in%
        c("sequential", "temporal"))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph series type is neither of the `sequential` nor `temporal` types")
  }

  # Add graph to series
  graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

  graph_series
}
