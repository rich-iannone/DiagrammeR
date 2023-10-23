#' Get total degree distribution data for a graph
#'
#' @description
#'
#' Get degree distribution data for a graph. Graph degree is represented as a
#' frequency of total degree values over all nodes in the graph.
#'
#' @inheritParams render_graph
#' @param mode using `total` (the default), degree considered for each node
#'   will be the total degree. With `in` and `out` the degree used
#'   will be the in-degree and out-degree, respectively.
#'
#' @return A data frame with degree frequencies.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the total degree
#' # distribution for the graph
#' graph %>%
#'   get_degree_distribution(
#'     mode = "total")
#'
#' @export
get_degree_distribution <- function(
    graph,
    mode = "total"
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the total degree distribution for the graph
  if (mode %in% c("all", "total", "both")) {
    deg_dist <- igraph::degree_distribution(ig_graph, mode = "all")

    # Transform to a data frame
    deg_dist_df <-
      data.frame(
        degree = seq(0, length(deg_dist) - 1),
        total_degree_dist = deg_dist,
        stringsAsFactors = FALSE)
  }

  # Get the total in-degree distribution for the graph
  if (mode == "in") {
    deg_dist <- igraph::degree_distribution(ig_graph, mode = "in")

    # Transform to a data frame
    deg_dist_df <-
      data.frame(
        degree = seq(0, length(deg_dist) - 1),
        indegree_dist = deg_dist,
        stringsAsFactors = FALSE)
  }

  # Get the total out-degree distribution for the graph
  if (mode == "out") {
    deg_dist <- igraph::degree_distribution(ig_graph, mode = "out")

    # Transform to a data frame
    deg_dist_df <-
      data.frame(
        degree = seq(0, length(deg_dist) - 1),
        outdegree_dist = deg_dist,
        stringsAsFactors = FALSE)
  }

  deg_dist_df
}
