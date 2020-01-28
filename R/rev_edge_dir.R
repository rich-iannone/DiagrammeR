#' Reverse the direction of all edges in a graph
#'
#' Using a directed graph as input, reverse the direction of all edges in that
#' graph.
#'
#' @inheritParams render_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with a
#' # directed tree
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 2)
#'
#' # Inspect the graph's edges
#' graph %>% get_edges()
#'
#' # Reverse the edge directions
#' # such that edges are directed
#' # toward the root of the tree
#' graph <-
#'   graph %>%
#'   rev_edge_dir()
#'
#' # Inspect the graph's edges
#' # after their reversal
#' graph %>% get_edges()
#'
#' @export
rev_edge_dir <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # If graph is undirected, stop function
  if (graph$directed == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The input graph must be a directed graph")
  }

  # Get the graph nodes in the `from` and `to` columns
  # of the edf
  from <- get_edges(graph, return_type = "df")[, 1]
  to <- get_edges(graph, return_type = "df")[, 2]

  # Extract the graph's edge data frame
  edges <- get_edge_df(graph)

  # Replace the contents of the `from` and `to` columns
  edges$from <- to
  edges$to <- from

  # Modify the graph object
  graph$edges_df <- edges

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
