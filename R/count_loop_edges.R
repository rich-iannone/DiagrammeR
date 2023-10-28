#' Get count of all loop edges
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, get a count of all loop edges in
#' the graph.
#'
#' @inheritParams render_graph
#'
#' @return A numeric vector of single length.
#'
#' @examples
#' # Create an undirected, full graph
#' # with 3 nodes and all possible
#' # edges, including loop edges
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_full_graph(
#'     n = 3,
#'     keep_loops = TRUE)
#'
#' # Get a count of all loop edges
#' # in the graph
#' graph %>% count_loop_edges()
#'
#' @export
count_loop_edges <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0L)
  }

  n_match <- length(which( graph$edges_df$from == graph$edges$to))
  n_match
}
