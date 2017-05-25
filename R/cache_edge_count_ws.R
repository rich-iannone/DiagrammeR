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
#' \code{dgr_graph}.
#' @param name an optional name for the cached vector.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 5 nodes and 4 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Cache a count of edges after creating a selection
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(nodes = 2) %>%
#'   cache_edge_count_ws(name = "edge_count")
#'
#' # Get the number of edges stored in the cache
#' graph %>%
#'   get_cache(name = "edge_count")
#' #> [1] 2
#' @export cache_edge_count_ws

cache_edge_count_ws <- function(graph,
                                name = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges available.")
  }

  # Cache vector of edge attributes in the
  # graph's `cache` list object
  if (!is.null(name)) {
    graph$cache[[name]] <- nrow(graph$edge_selection)
  } else {
    if (length(graph$cache) == 0) {
      graph$cache[[1]] <- nrow(graph$edge_selection)
      names(graph$cache) <- 1
    } else {
      graph$cache[[(length(graph$cache) + 1)]] <-
        nrow(graph$edge_selection)
    }
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "cache_edge_count_ws",
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
