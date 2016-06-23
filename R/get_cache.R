#' Get a cached vector from a graph object
#' @description Get the vector cached in a graph object
#' of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a vector.
#' @export get_cache

get_cache <- function(graph) {

  # If there is no cached vector available, return NA
  if (is.null(graph$cache)) {
    return(NA)
  }

  return(graph$cache)
}
