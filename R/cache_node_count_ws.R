#' Cache a count of nodes (available in a selection)
#' in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of nodes available in
#' a selection and cache that value in the graph for
#' later retrieval using \code{get_cache}.
#'
#' Selections of nodes can be performed using
#' the following \code{select_...} functions:
#' \code{select_nodes()},
#' \code{select_last_node()},
#' \code{select_nodes_by_degree()},
#' \code{select_nodes_by_id()}, or
#' \code{select_nodes_in_neighborhood()}.
#' Selections of nodes can also be performed using
#' the following traversal functions:
#' (\code{trav_...}):
#' \code{trav_out()}, \code{trav_in()},
#' \code{trav_both()}, \code{trav_in_node()},
#' \code{trav_out_node()}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 10 nodes and 9 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6
#'      3->7 4->8 4->9 5->10")
#'
#' # Cache a count of nodes after creating a selection
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(2:5) %>%
#'   cache_node_count_ws()
#'
#' # Get the number of nodes stored in the cache
#' graph %>% get_cache()
#' #> [1] 4
#' @export cache_node_count_ws

cache_node_count_ws <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes available.")
  }

  # Cache numeric vector of single length
  # in the graph
  graph$cache <- length(graph$selection$nodes)

  return(graph)
}
