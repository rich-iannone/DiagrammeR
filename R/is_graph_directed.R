#' Is the graph a directed graph?
#' Determines whether a graph is set to be directed or not and returns a logical value to that effect.
#' @param graph a graph object of class \code{dgr_graph}.
#' @return a logical value.
#' @export is_graph_directed

is_graph_directed <- function(graph){

  # Determine if graph is directed by getting the value at 'graph$directed'
  graph_is_directed <- graph$directed

  return(graph_is_directed)
}
