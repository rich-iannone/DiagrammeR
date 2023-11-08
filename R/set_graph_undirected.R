#' Convert a directed graph to an undirected graph
#'
#' @description
#'
#' Take a graph which is directed and convert it to an undirected graph.
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
#' # Convert this graph from
#' # directed to undirected
#' graph <-
#'   graph %>%
#'   set_graph_undirected()
#'
#' # Perform a check on whether
#' # graph is directed
#' graph %>% is_graph_directed()
#'
#' @export
set_graph_undirected <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Set the `directed` vector to FALSE
  graph$directed <- FALSE

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
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
