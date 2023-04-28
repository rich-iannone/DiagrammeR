#' Get an edge data frame from a graph
#'
#' @description
#'
#' From a graph, obtain an edge data frame with all current edge attributes.
#'
#' @inheritParams render_graph
#'
#' @return An edge data frame.
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
#'   select_edges_by_node_id(
#'     nodes = 3:5) %>%
#'   set_edge_attrs_ws(
#'     edge_attr = color,
#'     value = "green") %>%
#'   set_edge_attrs_ws(
#'     edge_attr = rel,
#'     value = "a") %>%
#'   invert_selection %>%
#'   set_edge_attrs_ws(
#'     edge_attr = color,
#'     value = "blue") %>%
#'   set_edge_attrs_ws(
#'     edge_attr = rel,
#'     value = "b") %>%
#'   clear_selection()
#'
#' # Get the graph's internal
#' # edge data frame (edf)
#' graph %>% get_edge_df()
#'
#' @export
get_edge_df <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  graph$edges_df
}
