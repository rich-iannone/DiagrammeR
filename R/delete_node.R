#' Delete a node from an existing graph object
#'
#' From a graph object of class `dgr_graph`, delete an existing node by
#' specifying its node ID.
#'
#' @inheritParams render_graph
#' @param node A node ID for the node to be deleted from the graph.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with 5 nodes and
#' # edges between each in a path
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Delete node with ID `3`
#' graph <- delete_node(graph, node = 3)
#'
#' # Verify that the node with ID `3`
#' # is no longer in the graph
#' graph %>% get_node_ids()
#'
#' # Also note that edges are removed
#' # since there were edges between the
#' # removed node to and from other nodes
#' graph %>% get_edges()
#' @family Node creation and removal
#' @export
delete_node <- function(graph,
                        node) {

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
      reasons = "The graph contains no nodes, so, a node cannot be deleted")
  }

  # Verify that `node` is given as a single value
  node_is_single_value <-
    ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "Only a single node can be deleted")
  }

  # Stop function if node is not in the graph
  if (!(node %in% get_node_ids(graph))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The specified node is not available in the graph")
  }

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Get the graph's edge data frame
  edf <- graph$edges_df

  # Remove node from `ndf`
  ndf <-
    ndf %>%
    dplyr::filter(id != node)

  # Remove any edges connected to `node`
  # in the `edf`
  edf <-
    edf %>%
    dplyr::filter(!(from == node | to == node))

  # Reset the row names in the ndf and the edf
  row.names(ndf) <- NULL
  row.names(edf) <- NULL

  # Update the graph's node and edge data frames
  graph$nodes_df <- ndf
  graph$edges_df <- edf

  # Scavenge any invalid, linked data frames
  graph <-
    graph %>%
    remove_linked_dfs()

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- graph %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_deleted <- nodes_graph_2 - nodes_graph_1

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_deleted <- edges_graph_2 - edges_graph_1

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
      d_n = nodes_deleted,
      d_e = edges_deleted)

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
