#' Delete all nodes in a node selection
#'
#' @description
#'
#' In a graph object of class `dgr_graph`, delete all nodes present in a
#' selection of nodes.
#'
#' This function makes use of an active selection of nodes (and the function
#' ending with `_ws` hints at this).
#'
#' Selections of nodes can be performed using the following node selection
#' (`select_*()`) functions: [select_nodes()], [select_last_nodes_created()],
#' [select_nodes_by_degree()], [select_nodes_by_id()], or
#' [select_nodes_in_neighborhood()].
#'
#' Selections of nodes can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out()], [trav_in()], [trav_both()],
#' [trav_out_node()], [trav_in_node()], [trav_out_until()], or
#' [trav_in_until()].
#'
#' @inheritParams render_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with 3 nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 3) %>%
#'   add_edges_w_string(
#'     edges = "1->3 1->2 2->3")
#'
#' # Select node with ID `1`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(nodes = 1)
#'
#' # Delete node in selection (this
#' # also deletes any attached edges)
#' graph <-
#'   graph %>%
#'   delete_nodes_ws()
#'
#' # Get a count of nodes in the graph
#' graph %>% count_nodes()
#'
#' @family node creation and removal
#'
#' @export
delete_nodes_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph, "So, no node can be deleted.")

  # Validation: Graph object has valid node selection
  check_graph_contains_node_selection(graph)

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Get a vector of the nodes to be deleted
  nodes_to_delete <- graph$node_selection$node

  # Delete all nodes in `nodes_to_delete`
  for (i in seq_along(nodes_to_delete)) {

    graph <-
      delete_node(
        graph = graph,
        node = nodes_to_delete[i])
  }

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Remove any `delete_node` records from the graph log
  graph$graph_log <-
    graph$graph_log[-((nrow(graph$graph_log) - (i - 1)):nrow(graph$graph_log)), ]

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

  # Get the name of the function
  fcn_name <- get_calling_fcn()

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
      trigger_graph_actions(graph)
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
