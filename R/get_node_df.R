#' Get a node data frame from a graph
#'
#' @description
#'
#' From a graph, obtain a node data frame with all current node attributes.
#'
#' @inheritParams render_graph
#'
#' @return A node data frame.
#'
#' @examples
#' # Create a graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(
#'     n = 1,
#'     type = "a") %>%
#'   select_last_nodes_created() %>%
#'   add_n_nodes_ws(
#'     n = 5,
#'     direction = "from",
#'     type = "b") %>%
#'   select_nodes_by_id(
#'     nodes = 1) %>%
#'   set_node_attrs_ws(
#'     node_attr = value,
#'     value = 25.3) %>%
#'   clear_selection() %>%
#'   select_nodes_by_id(
#'     nodes = 2:4) %>%
#'   set_node_attrs_ws(
#'     node_attr = color,
#'     value = "grey70") %>%
#'   invert_selection() %>%
#'   set_node_attrs_ws(
#'     node_attr = color,
#'     value = "grey80") %>%
#'   clear_selection()
#'
#' # Get the graph's internal node
#' # data frame (ndf)
#' graph %>%
#'   get_node_df()
#'
#' @export
get_node_df <- function(graph) {
  check_graph_valid(graph)
  graph$nodes_df
}
