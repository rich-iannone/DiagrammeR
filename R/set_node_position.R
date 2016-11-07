#' Apply a layout position to a single node
#' @description Apply position information for a
#' single node. This is done by setting the \code{x}
#' and \code{y} attrs for the node ID supplied in
#' \code{node}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param a single-length vector containing a
#' node ID value for which position information
#' should be applied.
#' @param x the x coordinate to set for the node.
#' @param y the y coordinate to set for the node.
#' @return a graph object of class \code{dgr_graph}.
#' @importFrom dplyr case_when mutate coalesce
#' @export set_node_position

set_node_position <- function(graph,
                              node,
                              x,
                              y) {

  # Get the graph's node data frame as an object; stop
  # function if this doesn't exist
  if (is.null(graph$nodes_df)) {
    stop("This graph does not contain any nodes.")
  } else {
    ndf <- graph$nodes_df
  }

  # Stop function if the node ID provided doesn't
  # exist in the graph
  if (!(node %in% graph$nodes_df[, 1])) {
    stop("The node ID provided doesn't exist in the graph.")
  }

  # If the `x` node attribute doesn't exist, create
  # that column in the `ndf`
  if (!("x" %in% colnames(ndf))) {
    ndf <-
      ndf %>%
      dplyr::mutate(x = as.numeric(NA))
  }

  # If the `y` node attribute doesn't exist, create
  # that column in the `ndf`
  if (!("y" %in% colnames(ndf))) {
    ndf <-
      ndf %>%
      dplyr::mutate(y = as.numeric(NA))
  }

  # Use a `case_when` statement to selectively perform
  # a vectorized `if` statement across all nodes for
  # the `x` node attribute
  x_attr_new <-
    dplyr::case_when(
      ndf$id == node ~ x,
      TRUE ~ as.numeric(ndf$x))

  # Replace the `x` column to the ndf with a
  # coalesced version of the column contents
  ndf$x <- dplyr::coalesce(x_attr_new, ndf$x)

  # Use a `case_when` statement to selectively perform
  # a vectorized `if` statement across all nodes for
  # the `y` node attribute
  y_attr_new <-
    dplyr::case_when(
      ndf$id == node ~ y,
      TRUE ~ as.numeric(ndf$y))

  # Replace the `y` column to the ndf with a
  # coalesced version of the column contents
  ndf$y <- dplyr::coalesce(y_attr_new, ndf$y)

  # Replace the graph's node data frame with `ndf`
  graph$nodes_df <- ndf

  return(graph)
}
