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
#' # With the `create_random_graph()` function, get
#' # a simple graph with a node attribute called
#' # `value`
#' graph <-
#'   create_random_graph(
#'     n = 4, m = 4,
#'     set_seed = 23)
#'
#' # Select nodes with ID values `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(nodes = c(1, 3))
#'
#' # Return the node attribute values for the nodes
#' # in the active selection
#' graph %>%
#'   get_node_attrs_ws(node_attr = "value")
#' #>   1   3
#' #> 6.0 3.5
#' @export get_node_attrs_ws

get_node_attrs_ws <- function(graph,
                              node_attr) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has a valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes available.")
  }

  if (node_attr %in% c("id", "nodes")) {
    stop("This is not a node attribute.")
  }

  # Extract the node data frame (ndf)
  # from the graph
  ndf <- graph$nodes_df

  # Get the node IDs from the node selection
  nodes <- sort(graph$node_selection$node)

  # Extract the node attribute values
  node_attr_vals <-
    ndf[
      which(ndf[, 1] %in% nodes),
      which(colnames(ndf) == node_attr)]

  # Add names to each of the values
  names(node_attr_vals) <- nodes

  return(node_attr_vals)
}
