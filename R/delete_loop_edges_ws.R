#' Delete all loop edges associated with a selection of nodes
#' @description With a selection of nodes in a
#' graph, remove any associated loop edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class
#' \code{dgr_graph}.
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
#' graph %>%
#'   count_loop_edges()
#' @importFrom dplyr setdiff filter
#' @importFrom purrr map_df
#' @export delete_loop_edges_ws

delete_loop_edges_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, there are no nodes to disconnect.",
      call. = FALSE)
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    stop(
      "There is no selection of nodes, so, no nodes can be disconnected.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  from <- to <- NULL

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

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "delete_loop_edges_ws",
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
