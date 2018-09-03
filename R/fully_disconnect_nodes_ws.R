#' Fully disconnect all nodes in a selection of nodes
#'
#' With a selection of nodes in a graph, remove any edges to or from those
#'   nodes.
#' @inheritParams render_graph
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and
#' # add a path of 6 nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 6)
#'
#' # Select nodes `3` and `4`
#' # and fully disconnect them
#' # from the graph
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = 3:4) %>%
#'   fully_disconnect_nodes_ws()
#'
#' # Get the graph's edge data frame
#' graph %>% get_edge_df()
#' @importFrom dplyr filter
#' @export
fully_disconnect_nodes_ws <- function(graph) {

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

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes, so, there are no nodes to disconnect")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of nodes available.")
  }

  # Create bindings for specific variables
  from <- to <- NULL

  # Get the graph's edf
  edf <- graph$edges_df

  # Get the number of edges in the graph
  edges_graph_1 <-
    graph %>%
    count_edges()

  # Filter edf such that any edges containing
  # nodes in the node selection are removed
  edf_replacement <-
    edf %>%
    dplyr::filter(
      !(from %in% suppressMessages(get_selection(graph)) |
          to %in% suppressMessages(get_selection(graph))))

  # Update the graph's edf
  graph$edges_df <- edf_replacement

  # Scavenge any invalid, linked data frames
  graph <-
    graph %>%
    remove_linked_dfs()

  # Get the updated number of edges in the graph
  edges_graph_2 <-
    graph %>%
    count_edges()

  # Get the number of edges added to
  # the graph
  edges_removed <- edges_graph_2 - edges_graph_1

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_e = edges_removed)

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
