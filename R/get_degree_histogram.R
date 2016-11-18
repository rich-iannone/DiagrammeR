#' Get histogram data for a graph's degree frequency
#' @description Get histogram data for a graph's
#' degree frequency. The bin width is set to 1 and
#' zero-value degrees are omitted from the output.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a named vector of degree counts (with
#' bin width equal to 1) where the degree values
#' serve as names.
#' @examples
#' # Create a random, directed graph with 18 nodes
#' # and 22 edges
#' graph <-
#'   create_random_graph(
#'     n = 18, m = 22,
#'     set_seed = 23)
#'
#' # Get degree histogram data for `random_graph`
#' graph %>% get_degree_histogram()
#' #> 0 1 2 3 4
#' #> 1 4 4 4 5
#' @importFrom igraph degree_distribution
#' @export get_degree_histogram

get_degree_histogram <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the degree distribution for the graph
  # and multiply by the total number of nodes to
  # resolve counts of nodes by degree
  deg_hist <-
    degree_distribution(ig_graph) *
    node_count(graph)

  # Transform to a named vector where the names are
  # the number of degrees
  names(deg_hist) <- seq(0, length(deg_hist) - 1)

  return(deg_hist)
}
