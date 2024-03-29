#' Get information on any available graph actions
#'
#' @description
#'
#' Get a tibble of the available graph actions, which contains information on
#' function invocations to be called on the graph at every transformation step,
#' or, when manually invoked with the [trigger_graph_actions()] function.
#'
#' @inheritParams render_graph
#'
#' @return A `df_tbl` object.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Add a graph action that sets a node
#' # attr column with a function; the
#' # main function `set_node_attr_w_fcn()`
#' # uses the `get_betweenness()` function
#' # to provide betweenness values in the
#' # `btwns` column
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "set_node_attr_w_fcn",
#'     node_attr_fcn = "get_betweenness",
#'     column_name = "btwns",
#'     action_name = "get_btwns")
#'
#' # To ensure that the action is
#' # available in the graph, use the
#' # `get_graph_actions()` function
#' graph %>% get_graph_actions()
#'
#' @export
get_graph_actions <- function(graph) {
  check_graph_valid(graph)
  dplyr::as_tibble(graph$graph_actions)
}
