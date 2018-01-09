#' Convert a directed graph to an undirected graph
#' @description Take a graph which is
#' directed and convert it to an undirected
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class
#' \code{dgr_graph}.
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
#' graph %>%
#'   is_graph_directed()
#' #> [1] FALSE
#' @export set_graph_undirected

set_graph_undirected <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Set the `directed` vector to FALSE
  graph$directed <- FALSE

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_graph_undirected",
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
