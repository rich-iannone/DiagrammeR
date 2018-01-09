#' Get count of all loop edges in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of all
#' loop edges in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a numeric vector of single length.
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
#' graph %>%
#'   count_loop_edges()
#' #> [1] 3
#' @importFrom dplyr filter
#' @export count_loop_edges

count_loop_edges <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  from <- to <- NULL

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
