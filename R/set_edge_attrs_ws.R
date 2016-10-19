#' Set edge attributes with an edge selection
#' @description From a graph object of class
#' \code{dgr_graph} or an edge data frame, set edge
#' attribute properties for one or more edges
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr the name of the attribute to set.
#' @param value the value to be set for the chosen
#' attribute for the edges in the current selection.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_path(6)
#'
#' # Select specific edges from the graph and
#' # apply the edge attribute `color = blue` to
#' # those selected edges
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(2:4) %>%
#'   trav_out_edge %>%
#'   set_edge_attrs_ws("color", "blue")
#'
#' # Show the internal edge data frame to verify
#' # that the edge attribute has been set for
#' # specific edges
#' get_edge_df(graph)
#' #>   from to rel color
#' #> 1    1  2      <NA>
#' #> 2    2  3      blue
#' #> 3    3  4      blue
#' #> 4    4  5      blue
#' #> 5    5  6      <NA>
#' @export set_edge_attrs_ws

set_edge_attrs_ws <- function(graph,
                              edge_attr,
                              value) {

  # Get vectors of node ID values for the
  # `from` and `to` nodes
  from <- graph$selection$edges$from
  to <- graph$selection$edges$to

  # Call the `set_edge_attrs()` function
  # and update the graph
  graph <-
    set_edge_attrs(
      x = graph,
      edge_attr = edge_attr,
      values = value,
      from = from,
      to = to)

  return(graph)
}
