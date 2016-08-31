#' Cache a count of edges (available in a
#' selection) in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of edges available in
#' a selection and cache that value in the graph for
#' later retrieval using \code{get_cache}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' # Create a graph with 10 nodes and 9 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6
#'     3->7 4->8 4->9 5->10")
#'
#' # Cache a count of edges after creating a selection
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(2) %>%
#'   cache_edge_count_ws
#'
#' # Get the number of edges stored in the cache
#' graph %>% get_cache
#' #> [1] 3
#' @export cache_edge_count_ws

cache_edge_count_ws <- function(graph) {

  # If no edge selection is available, return the
  # graph unchanged
  if (is.null(graph$selection$edges)) {
    return(graph)
  } else {
    # Cache numeric vector of single length
    # in the graph
    graph$cache <- length(graph$selection$edges$from)
    return(graph)
  }
}
