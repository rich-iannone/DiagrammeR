#' Set node attributes with a node selection
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, set node
#' attribute properties for nodes present in a node
#' selection.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr the name of the attribute to set.
#' @param value the value to be set for the chosen
#' attribute for the nodes in the current selection.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_path(6)
#'
#' # Select specific nodes from the graph and
#' # apply the node attribute `color = blue` to
#' # those selected nodes
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(1:4) %>%
#'   trav_out %>%
#'   set_node_attrs_ws("color", "blue")
#'
#' # Show the internal node data frame to verify
#' # that the node attribute has been set for
#' # specific node
#' get_node_df(graph)
#' #>   id type label color
#' #> 1  1          1  <NA>
#' #> 2  2          2  blue
#' #> 3  3          3  blue
#' #> 4  4          4  blue
#' #> 5  5          5  blue
#' #> 6  6          6  <NA>
#' @export set_node_attrs_ws

set_node_attrs_ws <- function(graph,
                              node_attr,
                              value) {

  # Get vector of node ID values
  nodes <- graph$selection$nodes

  # Call the `set_node_attrs()` function
  # and update the graph
  graph <-
    set_node_attrs(
      x = graph,
      node_attr = node_attr,
      values = value,
      nodes = nodes)

  return(graph)
}
