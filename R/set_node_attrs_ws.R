#' Set node attributes with a node selection
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, set node
#' attribute properties for nodes present in a node
#' selection.
#'
#' Selections of nodes can be performed using
#' the following \code{select_...} functions:
#' \code{select_nodes()},
#' \code{select_last_nodes_created()},
#' \code{select_nodes_by_degree()},
#' \code{select_nodes_by_id()}, or
#' \code{select_nodes_in_neighborhood()}.
#' Selections of nodes can also be performed using
#' the following traversal functions:
#' (\code{trav_...}):
#' \code{trav_out()}, \code{trav_in()},
#' \code{trav_both()}, \code{trav_in_node()},
#' \code{trav_out_node()}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr the name of the attribute to set.
#' @param value the value to be set for the chosen
#' attribute for the nodes in the current selection.
#' @return a graph object of class \code{dgr_graph}.
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
#' graph %>%
#'   get_node_df()
#' @importFrom rlang enquo UQ
#' @export set_node_attrs_ws

set_node_attrs_ws <- function(graph,
                              node_attr,
                              value) {

  # Get the time of function start
  time_function_start <- Sys.time()

  node_attr <- rlang::enquo(node_attr)
  node_attr <- (rlang::UQ(node_attr) %>% paste())[2]

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, no node attributes can be set.",
      call. = FALSE)
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    stop(
      "There is no selection of nodes available.",
      call. = FALSE)
  }

  # Get vector of node ID values
  nodes <- graph$node_selection$node

  node_attr_2 <- rlang::enquo(node_attr)

  # Call the `set_node_attrs()` function
  # and update the graph
  graph <-
    set_node_attrs(
      graph = graph,
      node_attr = rlang::UQ(node_attr_2),
      values = value,
      nodes = nodes)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_node_attrs_ws",
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
