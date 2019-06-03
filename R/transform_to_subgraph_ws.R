#' Create a subgraph using a node or edge selection
#'
#' Create a subgraph based on a selection of nodes or edges stored in the graph
#'   object.
#'
#' This function makes use of an active selection of nodes or edges (and the
#' function ending with `_ws` hints at this).
#'
#' Selections of nodes can be performed using the following node selection
#' (`select_*()`) functions:
#' [select_nodes()],
#' [select_last_nodes_created()],
#' [select_nodes_by_degree()],
#' [select_nodes_by_id()], or
#' [select_nodes_in_neighborhood()].
#'
#' Selections of edges can be performed using the following edge selection
#' (`select_*()`) functions:
#' [select_edges()],
#' [select_last_edges_created()],
#' [select_edges_by_edge_id()], or
#' [select_edges_by_node_id()].
#'
#' Selections of nodes or edges can also be performed using the following
#' traversal (`trav_*()`) functions:
#' [trav_out()],
#' [trav_in()],
#' [trav_both()],
#' [trav_out_node()],
#' [trav_in_node()],
#' [trav_out_until()],
#' [trav_in_until()],
#' [trav_out_edge()],
#' [trav_in_edge()],
#' [trav_both_edge()], or
#' [trav_reverse_edge()].
#' @inheritParams render_graph
#' @return a graph object of class `dgr_graph`.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 6,
#'     value =
#'       c(3.5, 2.6, 9.4,
#'         2.7, 5.2, 2.1))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 4, 5, 2, 6, 2),
#'       to = c(2, 4, 1, 3, 5, 5, 4))
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Create a selection of nodes, this selects
#' # nodes `1`, `3`, and `5`
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     conditions = value > 3)
#'
#' # Create a subgraph based on the selection
#' subgraph <-
#'   graph %>%
#'   transform_to_subgraph_ws()
#'
#' # Display the graph's node data frame
#' subgraph %>% get_node_df()
#'
#' # Display the graph's edge data frame
#' subgraph %>% get_edge_df()
#'
#' # Create a selection of edges, this selects
#' # edges `1`, `2`
#' graph <- graph %>%
#'   clear_selection() %>%
#'   select_edges(
#'   edges = c(1,2))
#'
#' # Create a subgraph based on the selection
#'   subgraph <-
#'   graph %>%
#'   transform_to_subgraph_ws()
#'
#' # Display the graph's node data frame
#'   subgraph %>% get_node_df()
#'
#' # Display the graph's edge data frame
#'   subgraph %>% get_edge_df()
#' @importFrom dplyr filter semi_join
#' @importFrom stringr str_split
#' @export
transform_to_subgraph_ws <- function(graph) {

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

  # Validation: Graph object has valid selection of
  # nodes or edges
  if (!(graph_contains_node_selection(graph) |
        graph_contains_edge_selection(graph))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of node or edges available.")
  }

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Filter the nodes in the graph
  if (graph_contains_node_selection(graph)) {

    selection <- graph$node_selection$node

    ndf <-
      graph$nodes_df %>%
      dplyr::filter(id %in% selection)

    edf <-
      graph$edges_df %>%
      dplyr::filter(from %in% selection & to %in% selection)

    # Create a subgraph
    graph$nodes_df <- ndf
    graph$edges_df <- edf
  }

  # Filter the edges in the graph
  if (graph_contains_edge_selection(graph)) {

    selection <- graph$edge_selection$edge

    selection_df <-
      data.frame(id = selection)

    edf <-
      graph$edges_df %>%
      dplyr::semi_join(selection_df, by = c("id"))

    ndf <-
      graph$nodes_df %>%
      dplyr::filter(id %in% unique(c(edf$from, edf$to)))

    # Create a subgraph
    graph$nodes_df <- ndf
    graph$edges_df <- edf
  }

  # Scavenge any invalid, linked data frames
  graph <-
    graph %>%
    remove_linked_dfs()

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- graph %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_added <- nodes_graph_2 - nodes_graph_1

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
      d_n = nodes_added,
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
