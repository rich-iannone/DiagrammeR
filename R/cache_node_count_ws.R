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
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 5 nodes and 4 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(5)
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

  # Get the time of function start
  time_function_start <- Sys.time()

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
  graph$cache <- nrow(graph$node_selection)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "cache_node_count_ws",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
