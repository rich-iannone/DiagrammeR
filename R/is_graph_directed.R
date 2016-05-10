#' Is the graph a directed graph?
#' @description Determines whether a graph is set to be
#' directed or not and returns a logical value to that
#' effect.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' \dontrun{
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_nodes(
#'     nodes = c("a", "b", "c", "d"))
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edges(
#'     from = c("a", "b", "c"),
#'     to = c("d", "c", "a"))
#'
#' # Create a graph
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Determine whether the graph is directed
#' is_graph_directed(graph)
#' #> [1] TRUE
#' }
#' @export is_graph_directed

is_graph_directed <- function(graph) {

  # Determine if graph is directed by getting the
  # value at `graph$directed`
  graph_is_directed <- graph$directed

  return(graph_is_directed)
}
