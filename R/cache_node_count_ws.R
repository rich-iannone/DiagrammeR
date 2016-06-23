#' Cache a count of nodes (available in a selection)
#' in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of nodes available in
#' a selection and cache that value in the graph for
#' later retrieval using \code{get_cache}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export cache_node_count_ws

cache_node_count_ws <- function(graph) {

  # If no node selection is available, return the
  # graph unchanged
  if (is.null(graph$selection$nodes)) {
    return(graph)
  } else {
    # Cache numeric vector of single length
    # in the graph
    graph$cache <- length(graph$selection$nodes)

    return(graph)
  }
}
