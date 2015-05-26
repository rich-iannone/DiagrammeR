#' Is the graph empty?
#' @description Determines whether a graph contains any nodes and returns a logical value to reflect emptiness.
#' @param graph a graph object of class \code{dgr_graph}.
#' @return a logical value.
#' @export is_graph_empty

is_graph_empty <- function(graph){

  # Determine if graph is empty by checking for NULL value
  graph_is_empty <- is.null(graph$nodes_df)

  return(graph_is_empty)

}
