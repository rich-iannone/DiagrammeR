#' Is the graph a directed graph?
#'
#' @description
#'
#' Determines whether a graph is set to be directed or not and returns a logical
#' value to that effect.
#'
#' @inheritParams render_graph
#'
#' @return A logical value.
#'
#' @examples
#' # Create an empty graph; by default,
#' # new graphs made by `create_graph()`
#' # are directed
#' graph <- create_graph()
#'
#' # Determine whether the graph
#' # is directed
#' graph %>% is_graph_directed()
#'
#' # Use the `set_graph_undirected()`
#' # function and check again whether
#' # the graph is directed
#' graph %>%
#'   set_graph_undirected() %>%
#'   is_graph_directed()
#'
#' @export
is_graph_directed <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Determine if graph is directed by getting the
  # value at `graph$directed`
  graph$directed
}
