#' Get a node data frame from a graph
#'
#' From a graph, obtain a node data frame with all current node attributes.
#'
#' @inheritParams render_graph
#' @return A node data frame.
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

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  graph$nodes_df
}
