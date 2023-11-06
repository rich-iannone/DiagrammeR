#' Determine whether a specified node is present
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, determine whether a specified node
#' is present.
#'
#' @inheritParams render_graph
#' @param node Either a node ID value or a node label to test for presence in
#'   the graph.
#'
#' @return A logical value.
#'
#' @examples
#' # Create a simple graph with
#' # a path of four nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 4,
#'     type = "path",
#'     label = c(
#'       "one", "two",
#'       "three", "four"))
#'
#' # Determine if there is a node
#' # with ID `1` in the graph
#' graph %>%
#'   is_node_present(node = 1)
#'
#' # Determine if there is a node
#' # with ID `5` in the graph
#' graph %>%
#'   is_node_present(node = 5)
#'
#' # Determine if there is a node
#' # with label `two` in the graph
#' graph %>%
#'   is_node_present(node = "two")
#' @export
is_node_present <- function(
    graph,
    node
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Stop function if `node` not a single value
  if (length(node) != 1) {

    cli::cli_abort(
      "Only a single node can be queried using `is_node_present()`")
  }

  if (inherits(node, "character")) {

    # Determine whether the label value
    # corresponds to a label in the graph
    node_is_present <- node %in% graph$nodes_df$label

  } else if (inherits(node, "numeric")) {

    # Determine whether the node ID value
    # corresponds to a node ID in the graph
    node_is_present <- node %in% get_node_ids(graph)
  }

  node_is_present
}
