#' Set node attributes
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, set node
#' attribute properties for one or more nodes.
#' @param x either a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}, or a node data frame.
#' @param node_attr the name of the attribute to set.
#' @param values the values to be set for the chosen
#' attribute for the chosen nodes.
#' @param nodes an optional vector of node IDs for
#' filtering the list of nodes present in the graph.
#' @return either a graph object of class
#' \code{dgr_graph} or a node data frame, depending on
#' what type of object was supplied to \code{x}.
#' @examples
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges)
#'
#' # Set attribute `color = "green"` for
#' # nodes `1` and `3` using the graph object
#' graph <-
#'   set_node_attrs(
#'     x = graph,
#'     node_attr = "color",
#'     values = "green",
#'     nodes = c(1, 3))
#'
#' # View the graph's node data frame
#' get_node_df(graph)
#' #>   id  type label value color
#' #> 1  1 basic     1   3.5 green
#' #> 2  2 basic     2   2.6
#' #> 3  3 basic     3   9.4 green
#' #> 4  4 basic     4   2.7
#'
#' # Set attribute `color = "green"` for
#' # nodes `1` and `3` using the node data frame
#' nodes <-
#'   set_node_attrs(
#'     x = nodes,
#'     node_attr = "color",
#'     values = "green",
#'     nodes = c(1, 3))
#'
#' # Display the `nodes` ndf
#' nodes
#' #>   id  type label value color
#' #> 1  1 basic     1   3.5 green
#' #> 2  2 basic     2   2.6
#' #> 3  3 basic     3   9.4 green
#' #> 4  4 basic     4   2.7
#'
#' # Set attribute `color = "blue"` for
#' # all nodes in the node data frame
#' nodes <-
#'   set_node_attrs(
#'     x = nodes,
#'     node_attr = "color",
#'     values = "blue")
#'
#' # Display the `nodes` ndf
#' nodes
#' #>   id  type label value color
#' #> 1  1 basic     1   3.5  blue
#' #> 2  2 basic     2   2.6  blue
#' #> 3  3 basic     3   9.4  blue
#' #> 4  4 basic     4   2.7  blue
#' @export set_node_attrs

set_node_attrs <- function(x,
                           node_attr,
                           values,
                           nodes = NULL) {

  if (node_attr == "id") {
    stop("You cannot change the node ID.")
  }

  if (inherits(x, "dgr_graph")) {
    object_type <- "dgr_graph"
    nodes_df <- x$nodes_df

    # Get the number of nodes ever created for
    # this graph
    nodes_created <- x$last_node
  }

  if (inherits(x, "data.frame")) {
    if ("id" %in% colnames(x)) {
      object_type <- "node_df"
      nodes_df <- x
    }
  }

  if (length(values) != 1 &
      length(values) != nrow(nodes_df)) {
    stop("The length of values provided must either be 1 or that of the number of rows in the ndf.")
  }

  if (length(values) == 1) {
    if (node_attr %in% colnames(nodes_df)) {
      if (is.null(nodes)) {
        nodes_df[, which(colnames(nodes_df) %in%
                           node_attr)] <- values
      } else {
        nodes_df[which(nodes_df[, 1] %in% nodes),
                 which(colnames(nodes_df) %in%
                         node_attr)] <- values
      }
    }

    if (!(node_attr %in% colnames(nodes_df))) {
      nodes_df <-
        cbind(nodes_df, rep("", nrow(nodes_df)))

      nodes_df[, ncol(nodes_df)] <-
        as.character(nodes_df[,ncol(nodes_df)])

      colnames(nodes_df)[ncol(nodes_df)] <- node_attr

      if (is.null(nodes)) {
        nodes_df[, ncol(nodes_df)] <- values
      } else {
        nodes_df[
          which(nodes_df[, 1] %in%
                  nodes), ncol(nodes_df)] <- values
      }
    }
  }

  if (length(values) == nrow(nodes_df)) {
    if (length(values) == nrow(nodes_df)) {

      if (node_attr %in% colnames(nodes_df)) {
        nodes_df[, which(colnames(nodes_df) %in%
                           node_attr)] <- values
      }

      if (!(node_attr %in% colnames(nodes_df))) {
        nodes_df <-
          cbind(nodes_df, rep("", nrow(nodes_df)))

        nodes_df[, ncol(nodes_df)] <-
          as.character(nodes_df[,ncol(nodes_df)])

        colnames(nodes_df)[ncol(nodes_df)] <-
          node_attr

        nodes_df[, ncol(nodes_df)] <- values
      }
    }
  }

  if (object_type == "dgr_graph") {

    # Replace the graph's ndf with the
    # revised version
    graph$nodes_df <- nodes_df

    # Retain the node selection if one was
    # available initially
    graph$selection <- x$selection

    # Update the `last_node` counter
    graph$last_node <- nodes_created

    return(graph)
  }

  if (object_type == "node_df") {
    return(nodes_df)
  }
}
