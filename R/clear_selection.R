#' Clear a selection of nodes or edges in a graph
#' @description Clear the selection of nodes or edges
#' within a graph object.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' nodes <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges)
#'
#' # Select nodes with IDs `1` and `3`
#' graph <-
#'   select_nodes(
#'     graph = graph,
#'     nodes = c(1, 3))
#'
#' # Verify that a node selection has been made
#' get_selection(graph)
#' #> [1] 1 3
#'
#' # Clear the selection with `clear_selection()`
#' graph <- clear_selection(graph = graph)
#'
#' # Verify that the node selection has been cleared
#' get_selection(graph)
#' #> [1] NA
#' @export clear_selection

clear_selection <- function(graph) {

  # Clear the selection in the graph, if it exists
  if (!is.null(graph$selection)) {
    graph$selection <- NULL
  }

  # Return the graph
  return(graph)
}
