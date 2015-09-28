#' Is the graph empty?
#' @description Provides a logical value on whether the graph is empty (i.e.,
#' contains no nodes).
#' @param graph a graph object of class \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' \dontrun{
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Determine whether the graph is empty
#' is_graph_empty(graph)
#' #> [1] TRUE
#'
#' # Create a graph with nodes and edges
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"))
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Determine whether the graph is empty
#' is_graph_empty(graph)
#' #> [1] FALSE
#' }
#' @export is_graph_empty

is_graph_empty <- function(graph){

  # Determine if graph is empty by checking for NULL value
  graph_is_empty <- is.null(graph$nodes_df)

  return(graph_is_empty)
}
