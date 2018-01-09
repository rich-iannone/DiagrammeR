#' Get node attribute values from a selection of nodes
#' @description From a graph object of class
#' \code{dgr_graph}, get node attribute values from nodes
#' currently active as a selection.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr the name of the attribute for which
#' to get values.
#' @return a named vector of node attribute values for
#' the attribute given by \code{node_attr} by node ID.
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
#' #>   1   3
#' #> 2.5 4.2
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ
#' @export get_node_attrs_ws

get_node_attrs_ws <- function(graph,
                              node_attr) {

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

  node_attr <- rlang::enquo(node_attr)

  # Create binding for a specific variable
  id <- NULL

  if ((rlang::UQ(node_attr) %>% paste())[2] %in% c("id", "nodes")) {

    stop(
      "This is not a node attribute.",
      call. = FALSE)
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
  node_attr_vals <-
    ndf %>%
    dplyr::pull(rlang::UQ(node_attr))

  # Add names to each of the values
  names(node_attr_vals) <- nodes

  node_attr_vals
}
