#' Get the graph diameter
#' @description Get the graph diameter, which is the
#' maximum eccentricity.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return the graph diameter as a single-length
#' vector.
#' @export get_graph_diameter

get_graph_diameter <- function(graph) {

  return(max(get_eccentricity(graph)))
}
