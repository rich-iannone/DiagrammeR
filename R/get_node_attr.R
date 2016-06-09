#' Get node attributes
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, get node
#' attribute properties for one or more nodes.
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
#' @export get_node_attr

get_node_attr <- function(x,
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
    nodes <- nodes_df$nodes
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

  # Determine whether `node_attr_vals` values
  # are numeric
  node_attr_vals_numeric <-
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(node_attr_vals)))),
      FALSE, TRUE)

  if (node_attr_vals_numeric == TRUE) {
    node_attr_vals <- as.numeric(node_attr_vals)
    names(node_attr_vals) <- nodes
  }

  return(node_attr_vals)
}
