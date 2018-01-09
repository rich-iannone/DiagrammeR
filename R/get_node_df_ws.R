#' Get the graph's ndf filtered by a selection of nodes
#' @description From a graph object of class
#' \code{dgr_graph}, get the graph's internal
#' node data frame that is filtered by the node
#' ID values currently active as a selection.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a node data frame.
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
#' # Get the node data frame that's
#' # limited to the rows that correspond
#' # to the node selection
#' graph %>%
#'   get_node_df_ws()
#' #>   id type label value
#' #> 1  1 <NA>  <NA>   2.5
#' #> 2  3 <NA>  <NA>   4.2
#' @importFrom dplyr filter
#' @export get_node_df_ws

get_node_df_ws <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph object has a valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    stop(
      "There is no selection of nodes available.",
      call. = FALSE)
  }

  # Create binding for specific variable
  id <- NULL

  # Extract the node data frame (ndf)
  # from the graph and get only those nodes
  # from the node selection
  graph$nodes_df %>%
    dplyr::filter(id %in% graph$node_selection$node)
}
