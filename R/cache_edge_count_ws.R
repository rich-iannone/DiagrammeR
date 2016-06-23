#' Cache a count of edges (available in a
#' selection) in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of edges available in
#' a selection and cache that value in the graph for
#' later retrieval using \code{get_cache}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export cache_edge_count_ws

cache_edge_count_ws <- function(graph) {

  # If no edge selection is available, return the
  # graph unchanged
  if (is.null(graph$selection$edges)) {
    return(graph)
  } else {
    # Cache numeric vector of single length
    # in the graph
    graph$cache <- length(graph$selection$edges$from)
    return(graph)
  }
}
