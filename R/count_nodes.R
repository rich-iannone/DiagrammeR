#' Get a count of all nodes
#'
#' From a graph object of class \code{dgr_graph}, get a count of nodes in the
#'   graph.
#' @inheritParams render_graph
#' @return a numeric vector of single length.
#' @examples
#' # Create a graph with a
#' # path of nodes and 3
#' # unconnected nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   add_n_nodes(n = 3)
#'
#' # Get a count of all nodes
#' # in the graph
#' graph %>%
#'   count_nodes()
#' @export
count_nodes <- function(graph) {

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

  nrow(graph$nodes_df)
}
