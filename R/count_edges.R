#' Get a count of all edges
#' @description From a graph object of
#' class \code{dgr_graph}, get a count
#' of edges in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single-length numeric
#' vector.
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
#' @export count_edges

count_edges <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0)
  }

  nrow(graph$edges_df)
}
