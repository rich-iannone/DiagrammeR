#' Get a count of all edges
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, get a count of edges in the graph.
#'
#' @inheritParams render_graph
#'
#' @return A single-length numeric vector.
#'
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
#'
#' @export
count_edges <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0L)
  }

  nrow(graph$edges_df)
}
