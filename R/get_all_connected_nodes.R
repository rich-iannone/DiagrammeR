#' Get all nodes connected to a specified node
#'
#' @description
#'
#' With a single node serving as the starting point get all nodes connected
#' (i.e., reachable with a traversable path) to that node.
#'
#' @inheritParams render_graph
#' @param node a single-length vector containing a node ID value.
#'
#' @return A vector of node ID values.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function; it
#' # has an unconnected node (`6`)
#' graph_1 <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 20,
#'     m = 32,
#'     set_seed = 23)
#'
#' # There won't be any connected
#' # nodes to `6` so when specifying
#' # this node with `get_all_connected_nodes()`
#' # we get NA back
#' graph_1 %>%
#'   get_all_connected_nodes(
#'     node = 6)
#'
#' # Any other node in `graph_1` will
#' # provide a vector of all the nodes
#' # other than `6`
#' graph_1 %>%
#'   get_all_connected_nodes(
#'     node = 1)
#'
#' # The following graph has two
#' # clusters of nodes (i.e., the
#' # graph has two connected components)
#' graph_2 <-
#'   create_graph() %>%
#'   add_path(n = 6) %>%
#'   add_path(n = 4)
#'
#' # In `graph_2`, node `1` is in
#' # the larger of the two
#' # connected components
#' graph_2 %>%
#'   get_all_connected_nodes(
#'     node = 1)
#'
#' # Also in `graph_2`, node `8`
#' # is in the smaller of the two
#' # connected components
#' graph_2 %>%
#'   get_all_connected_nodes(
#'     node = 8)
#'
#' @export
get_all_connected_nodes <- function(
    graph,
    node
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Verify that the node ID provided is in the graph
  if (!(node %in% get_node_ids(graph))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The node ID provided is not in the graph")
  }

  # Get a data frame of the weakly-connected
  # components for the entire graph
  wc_components_df <- get_w_connected_cmpts(graph)

  # Get single-row data frame with the node's
  # `id` and `wc_component` grouping
  wc_component <-
    wc_components_df %>%
    dplyr::filter(id == node)

  # Extract the `wc_component` grouping as
  # a single-length vector
  wc_component_val <- wc_component$wc_component

  # Filter the table of weakly-connected
  # components for the nodes in the `wc_component`
  # grouping (and removing the original node as well)
  connected_nodes <-
    wc_components_df %>%
    dplyr::filter(id != node) %>%
    dplyr::filter(wc_component == wc_component_val)

  # If the resulting df has rows, get the node ID
  # values from each of the rows; otherwise, return NA
  if (nrow(connected_nodes) > 0) {
    connected_nodes <-
      connected_nodes$id
  } else {
    connected_nodes <- NA
  }

  connected_nodes
}
