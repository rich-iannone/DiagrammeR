#' Set node attributes with a node selection
#'
#' @description
#'
#' From a graph object of class `dgr_graph` or a node data frame, set node
#' attribute properties for nodes present in a node selection.
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
#' @param node_attr The name of the attribute to set.
#' @param value The value to be set for the chosen attribute for the nodes in
#'   the current selection.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 6)
#'
#' # Select specific nodes from the graph and
#' # apply the node attribute `color = blue` to
#' # those selected nodes
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = 1:4) %>%
#'   trav_out() %>%
#'   set_node_attrs_ws(
#'     node_attr = color,
#'     value = "blue")
#'
#' # Show the internal node data frame to verify
#' # that the node attribute has been set for
#' # specific node
#' graph %>% get_node_df()
#'
#' @family node creation and removal
#'
#' @export
set_node_attrs_ws <- function(
    graph,
    node_attr,
    value
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Validation: Graph object has valid node selection
  check_graph_contains_node_selection(graph)

  # Get the requested `node_attr`
  node_attr <-
    rlang::enquo(node_attr) %>% rlang::get_expr() %>% as.character()

  # Get vector of node ID values
  nodes <- graph$node_selection$node

  node_attr_2 <- rlang::enquo(node_attr)

  # Call the `set_node_attrs()` function
  # and update the graph
  graph <-
    set_node_attrs(
      graph = graph,
      node_attr = !!node_attr_2,
      values = value,
      nodes = nodes
    )

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
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

  graph
}
