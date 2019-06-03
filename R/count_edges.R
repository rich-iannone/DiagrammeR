#' Get a count of all edges
#'
#' From a graph object of class `dgr_graph`, get a count of edges in the
#'   graph.
#' @inheritParams render_graph
#' @return a single-length numeric vector.
#' @examples
#' # Create a graph with a
#' # path of nodes and 3
#' # unconnected nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   add_n_nodes(n = 3)
#'
#' # Get a count of all edges
#' # in the graph
#' graph %>%
#'   count_edges()
#' @export
count_edges <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0)
  }

  nrow(graph$edges_df)
}
