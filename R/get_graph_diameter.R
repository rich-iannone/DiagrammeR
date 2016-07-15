#' Get the graph diameter
#' @description Get the graph diameter, which is the
#' maximum eccentricity.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return the graph diameter as a single-length
#' vector.
#' @examples
#' # Get the graph diameter for a randomly-created
#' # graph
#' get_graph_diameter(
#'   create_random_graph(
#'     5, 7,
#'     fully_connected = TRUE,
#'     set_seed = 20))
#' #> [1] 4
#' @export get_graph_diameter

get_graph_diameter <- function(graph) {

  return(max(get_eccentricity(graph)))
}
