#' Get node attribute values from a selection of nodes
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, get node attribute values from
#' nodes currently active as a selection.
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
#' @param node_attr The name of the attribute for which to get values.
#'
#' @return A named vector of node attribute values for the attribute given by
#'   `node_attr` by node ID.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 4,
#'     m = 4,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = c(2.5, 8.2, 4.2, 2.4))
#'
#' # Select nodes with ID values
#' # `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = c(1, 3))
#'
#' # Get the node attribute values
#' # for the `value` attribute, limited
#' # to the current node selection
#' graph %>%
#'   get_node_attrs_ws(
#'     node_attr = value)
#'
#' @export
get_node_attrs_ws <- function(
    graph,
    node_attr
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph object has a valid node selection
  check_graph_contains_node_selection(graph)

  node_attr <- rlang::enquo(node_attr)

  if (rlang::enquo(node_attr) %>%
      rlang::get_expr() %>%
      as.character() %in% c("id", "nodes")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "This is not a node attribute.")
  }

  # Extract the node data frame (ndf)
  # from the graph
  ndf <- graph$nodes_df

  # Get the node IDs from the node selection
  nodes <- sort(graph$node_selection$node)

  # Filter the ndf by the supplied
  # node ID values
  ndf <-
    ndf %>%
    dplyr::filter(id %in% nodes)

  # Extract the node attribute values
  node_attr_vals <- ndf %>% dplyr::pull(!!node_attr)

  # Add names to each of the values
  names(node_attr_vals) <- nodes

  node_attr_vals
}
