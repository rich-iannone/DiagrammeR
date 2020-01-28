#' Fully connect all nodes in a selection of nodes
#'
#' With a selection of nodes in a graph, add any remaining edges required to
#' fully connect this group of edges to each other.
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
#' # Create an empty graph and
#' # then add a path of 3 nodes
#' # and two isolated nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   add_n_nodes(n = 2)
#'
#' # Select a node in the path
#' # of nodes (node `3`) and
#' # the two isolated nodes (`4`
#' # and `5`); then, and fully
#' # connect these nodes together
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = 3:5) %>%
#'   fully_connect_nodes_ws()
#'
#' # Get the graph's edge data frame
#' graph %>% get_edge_df()
#'
#' # Create an undirected, empty
#' # graph; add a path of 3 nodes
#' # and two isolated nodes
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_path(n = 3) %>%
#'   add_n_nodes(n = 2)
#'
#' # Select a node in the path
#' # of nodes (node `3`) and
#' # the two isolated nodes (`4`
#' # and `5`); then, and fully
#' # connect these nodes together
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = 3:5) %>%
#'   fully_connect_nodes_ws()
#'
#' # Get the graph's edge data
#' # frame; in the undirected
#' # case, reverse edges aren't
#' # added
#' graph %>% get_edge_df()
#'
#' @export
fully_connect_nodes_ws <- function(graph) {

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
      reasons = "The graph contains no nodes, so, there are no nodes to connect")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of nodes available.")
  }

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Get the graph's edf
  edf <- graph$edges_df

  # Get the combination of edges
  edge_candidates <-
    utils::combn(suppressMessages(get_selection(graph = graph)), 2) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(from = V1, to = V2)

  # Determine the complete set of edges
  # to add to the graph

  if (is_graph_directed(graph)) {

    edges_to_add <-
      dplyr::setdiff(
        dplyr::bind_rows(
          edge_candidates,
          edge_candidates %>%
            dplyr::select(to, from) %>%
            dplyr::rename(from = to, to = from)),
        edf %>%
          dplyr::select(from, to))

  } else {

    edges_to_add <-
      dplyr::setdiff(
        edge_candidates,
        edf %>%
          dplyr::select(from, to))
  }

  # Add new edges to the graph for every edge
  # in the `edges_to_add` df
  if (nrow(edges_to_add) > 0) {
    for (i in 1:nrow(edges_to_add)) {

      # Create a graph edge
      graph <-
        add_edge(
          graph = graph,
          from = edges_to_add[i, 1],
          to = edges_to_add[i, 2])

      # Redact the signing of the action to the log
      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]
    }
  }

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

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
      d_e = edges_added)

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
