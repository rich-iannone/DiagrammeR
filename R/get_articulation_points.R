#' Get articulation points
#' @description Get the nodes in the graph that are
#' identified as articulation points.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector of node IDs.
#' @examples
#' \dontrun{
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the articulation points in the graph (i.e.,
#' # those nodes that if any were to be removed, the
#' # graph would become disconnected)
#'
#' }
#' @importFrom igraph articulation_points
#' @export get_articulation_points

get_articulation_points <- function(graph) {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the graph's articulation points
  articulation_points <-
    igraph::articulation_points(ig_graph)


  return(articulation_points)
}
