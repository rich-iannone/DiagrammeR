#' Get node eccentricities
#'
#' @description
#'
#' Get a data frame with node eccentricity values.
#'
#' @inheritParams render_graph
#' @param mode the mode with which the shortest paths to or from the given
#'   vertices should be calculated for directed graphs. If `out` (the
#'   default) then the shortest paths from the node, if `in` then only
#'   shortest paths to each node are considered. If `all` is used, then the
#'   corresponding undirected graph will be used and edge directions will be
#'   ignored. For undirected graphs, this argument is ignored.
#'
#' @return A data frame containing eccentricity values by node ID value.
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
#' # Get the eccentricity values for
#' # all nodes in the graph
#' graph %>% get_eccentricity()
#'
#' @export
get_eccentricity <- function(
    graph,
    mode = "out"
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the eccentricity with the given mode
  eccentricity <-
    igraph::eccentricity(
      graph = ig_graph,
      mode = mode)

  # Create a data frame with node ID values
  # and eccentrity values
  data.frame(
    id = eccentricity %>%
      names() %>%
      as.integer(),
    eccentricity = eccentricity,
    stringsAsFactors = FALSE)
}
