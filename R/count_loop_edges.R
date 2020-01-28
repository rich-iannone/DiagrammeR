#' Get count of all loop edges
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

  return(
    graph$edges_df %>%
      dplyr::filter(
        from == to) %>%
      nrow())
}
