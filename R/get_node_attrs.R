#' Get node attribute values
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, get node
#' attribute values for one or more nodes.
#' @param x either a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}, or a node data frame.
#' @param nodes an optional vector of node IDs for
#' filtering list of nodes present in the graph or
#' node data frame.
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
#' # Get all of the values from the `value` node
#' # attribute as a named vector
#' graph %>% get_node_attrs("value")
#' #>   1   2   3   4
#' #> 6.0 2.5 3.5 7.5
#'
#' # To only return node attribute values for specified
#' # nodes, use the `nodes` argument
#' graph %>%
#'   get_node_attrs("value", nodes = c(1, 3))
#' #>   1   3
#' #> 6.0 3.5
#' @export get_node_attrs

get_node_attrs <- function(x,
                           node_attr,
                           nodes = NULL) {

  if (node_attr == "nodes") {
    stop("This is not a node attribute.")
  }

  if (class(x) == "dgr_graph") {
    object_type <- "dgr_graph"
    nodes_df <- x$nodes_df
  }

  if (inherits(x, "data.frame")) {
    if ("nodes" %in% colnames(x)) {
      object_type <- "node_df"
      nodes_df <- x
    }
  }

  if (is.null(nodes)) {
    nodes <- nodes_df[, 1]
  }

  if (!is.null(nodes)) {
    nodes <- sort(nodes)
  }

  # Extract the node attribute values
  node_attr_vals <-
    nodes_df[
      which(nodes_df[, 1] %in% nodes),
      which(colnames(nodes_df) == node_attr)]

  # Add names to each of the values
  names(node_attr_vals) <- nodes

  return(node_attr_vals)
}
