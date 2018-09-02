#' Clear an active selection of nodes or edges
#'
#' Clear the selection of nodes or edges within a graph object.
#' @inheritParams render_graph
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with
#' # a single path
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Select nodes with IDs `1`
#' # and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     nodes = c(1, 3))
#'
#' # Verify that a node selection
#' # has been made
#' graph %>%
#'   get_selection()
#'
#' # Clear the selection with
#' # `clear_selection()`
#' graph <-
#'   graph %>%
#'   clear_selection()
#'
#' # Verify that the node
#' # selection has been cleared
#' graph %>%
#'   get_selection()
#' @export
clear_selection <- function(graph) {

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

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # Clear the selection of nodes and edges in the graph
  graph$node_selection <- create_empty_nsdf()
  graph$edge_selection <- create_empty_esdf()

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

  # Issue a message to the user
  if (n_e_select_properties_in[["selection_count"]] > 0) {

    emit_message(
      fcn_name = fcn_name,
      message_body = glue::glue(
        "cleared an existing selection of \\
       {n_e_select_properties_in[['selection_count_str']]}"))

  } else {

    emit_message(
      fcn_name = fcn_name,
      message_body = "no existing selection to clear; graph unchanged")
  }

  graph
}
