#' Get degree distribution data for a graph
#' @description Get degree distribution data for
#' a graph. Graph degree is represented as a
#' frequency of degree values over all nodes in
#' the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a named vector of degree frequencies
#' where the degree values serve as names.
#' @examples
#' # Create a random, directed graph with 18 nodes
#' # and 22 edges
#' graph <-
#'   create_random_graph(
#'     n = 18, m = 22,
#'     set_seed = 23)
#'
#' # Get degree distribution data for `random_graph`
#' graph %>% get_degree_distribution()
#' #> 0          1          2          3
#' #> 0.05555556 0.22222222 0.22222222 0.22222222
#' #> 4
#' #> 0.27777778
#' @importFrom igraph degree_distribution
#' @export get_degree_distribution

get_degree_distribution <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the degree distribution for the graph
  deg_dist <- degree_distribution(ig_graph)

  # Transform to a named vector where the names are
  # the number of degrees
  names(deg_dist) <- seq(0, length(deg_dist) - 1)

  return(deg_dist)
}
