#' Get total degree distribution data for a graph
#' @description Get degree distribution data for
#' a graph. Graph degree is represented as a
#' frequency of total degree values over all nodes
#' in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param mode using \code{total} (the default),
#' degree considered for each node will be the total
#' degree. With \code{in} and \code{out} the degree
#' used will be the in-degree and out-degree,
#' respectively.
#' @return a data frame with degree frequencies.
#' @examples
#' # Create a random, directed graph with
#' # 18 nodes and 22 edges
#' graph <-
#'   create_random_graph(
#'     n = 18, m = 22,
#'     set_seed = 23)
#'
#' # Get the total degree distribution for
#' # the `random_graph` graph
#' graph %>%
#'   get_degree_distribution(mode = "total")
#' #>   degree total_degree_dist
#' #> 1      0        0.05555556
#' #> 2      1        0.22222222
#' #> 3      2        0.22222222
#' #> 4      3        0.22222222
#' #> 5      4        0.27777778
#' @importFrom igraph degree_distribution
#' @export get_degree_distribution

get_degree_distribution <- function(graph,
                                    mode = "total") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, a degree distribution data frame cannot be produced.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the total degree distribution for the graph
  if (mode %in% c("all", "total", "both")) {
    deg_dist <- degree_distribution(ig_graph, mode = "all")

    # Transform to a data frame
    deg_dist_df <-
      data.frame(
        degree = seq(0, length(deg_dist) - 1),
        total_degree_dist = deg_dist,
        stringsAsFactors = FALSE)
  }

  # Get the total in-degree distribution for the graph
  if (mode == "in") {
    deg_dist <- degree_distribution(ig_graph, mode = "in")

    # Transform to a data frame
    deg_dist_df <-
      data.frame(
        degree = seq(0, length(deg_dist) - 1),
        indegree_dist = deg_dist,
        stringsAsFactors = FALSE)
  }

  # Get the total out-degree distribution for the graph
  if (mode == "out") {
    deg_dist <- degree_distribution(ig_graph, mode = "out")

    # Transform to a data frame
    deg_dist_df <-
      data.frame(
        degree = seq(0, length(deg_dist) - 1),
        outdegree_dist = deg_dist,
        stringsAsFactors = FALSE)
  }

  deg_dist_df
}
