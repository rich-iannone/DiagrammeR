#' Delete all loop edges associated with a selection of nodes
#'
#' @description
#'
#' With a selection of nodes in a graph, remove any associated loop edges.
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
#' # Create an undirected, full graph
#' # of 5 nodes with loops retained
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_full_graph(
#'     n = 5,
#'     keep_loops = TRUE)
#'
#' # Select nodes `3` and `4`
#' # and remove the loop edges
#' # associated with those nodes
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = 3:4) %>%
#'   delete_loop_edges_ws()
#'
#' # Count the number of loop
#' # edges remaining in the graph
#' graph %>% count_loop_edges()
#'
#' @family edge creation and removal
#'
#' @export
delete_loop_edges_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph, "So, there cannot be edges to delete.")

  # Validation: Graph object has valid node selection
  check_graph_contains_node_selection(graph)

  # Get the graph's edf
  edf <- graph$edges_df

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Filter edf such that any loop edges
  # associated with the selected nodes
  # are removed
  selected_nodes <- suppressMessages(get_selection(graph))

  edges_to_remove <-
    selected_nodes %>%
    purrr::map_df(
      .f = function(x) {
        edf %>%
          dplyr::filter(
            (from == x &
               to == x))
      })

  edf_replacement <-
    dplyr::setdiff(edf, edges_to_remove)

  # Update the graph's edf
  graph$edges_df <- edf_replacement

  # Scavenge any invalid, linked data frames
  graph <-
    graph %>%
    remove_linked_dfs()

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_removed <- edges_graph_2 - edges_graph_1

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
      edges = nrow(graph$edges_df),
      d_e = edges_removed)

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
