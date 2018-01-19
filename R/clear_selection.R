#' Clear a selection of nodes or edges in a graph
#' @description Clear the selection of nodes or edges
#' within a graph object.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph with nodes and edges
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Select nodes with IDs `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     nodes = c(1, 3))
#'
#' # Verify that a node selection has been made
#' graph %>%
#'   get_selection()
#'
#' # Clear the selection with `clear_selection()`
#' graph <-
#'   graph %>%
#'   clear_selection()
#'
#' # Verify that the node selection has been cleared
#' graph %>%
#'   get_selection()
#' @export clear_selection

clear_selection <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Clear the selection of nodes and edges in the graph
  graph$node_selection <- create_empty_nsdf()
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "clear_selection",
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
