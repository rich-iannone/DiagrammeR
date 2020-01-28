#' Create a graph series object
#'
#' Create a graph series object for the storage of multiple graphs across a
#' sequential or temporal one-dimensional array.
#'
#' @param graph A graph object to add to the new graph series object.
#' @param series_name An optional name to ascribe to the series.
#' @param series_type Either a `sequential` type (the default) or a `temporal`
#'   type (which requires date-time strings and time zone codes to be supplied).
#'
#' @return A graph series object of type `dgr_graph_1D`.
#'
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
#'
#' @export
create_graph_series <- function(graph = NULL,
                                series_name = NULL,
                                series_type = "sequential") {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (!is.null(graph))  {
    if (graph_object_valid(graph) == FALSE) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The graph object is not valid")
    }
  }

  # Initialize an empty graph series object
  graph_series <-
    list(
      graphs = NULL,
      series_name = series_name,
      series_type = series_type)

  attr(graph_series, "class") <- "dgr_graph_1D"

  if (is.null(graph)) {
    return(graph_series)
  }

  # Add a graph to the initialized graph series
  graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

  graph_series
}
