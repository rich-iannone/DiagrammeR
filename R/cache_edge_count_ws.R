#' Cache a count of edges (available in a
#' selection) in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of edges available in
#' a selection and cache that value in the graph for
#' later retrieval using \code{get_cache}.
#'
#' Selections of edges can be performed using
#' the following \code{select_...} functions:
#' \code{select_edges()},
#' \code{select_last_edge()}, or
#' \code{select_edges_by_node_id()}.
#' Selections of edges can also be performed using
#' the following traversal functions:
#' \code{trav_out_edge()}, \code{trav_in_edge()},
#' or \code{trav_both_edge()}.
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
#'   cache_edge_count_ws()
#'
#' # Get the number of edges stored in the cache
#' graph %>% get_cache()
#' #> [1] 3
#' @export cache_edge_count_ws

cache_edge_count_ws <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges available.")
  }

  # Cache numeric vector of single length
  # in the graph
  graph$cache <- length(graph$selection$edges$from)

  return(graph)
}
