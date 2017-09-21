#' Get node eccentricities
#' @description Get a data frame with node
#' eccentricity values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param mode the mode with which the shortest
#' paths to or from the given vertices should
#' be calculated for directed graphs. If
#' \code{out} (the default) then the shortest
#' paths from the node, if \code{in} then only
#' shortest paths to each node are considered.
#' If \code{all} is used, then the corresponding
#' undirected graph will be used and edge
#' directions will be ignored. For undirected
#' graphs, this argument is ignored.
#' @return a data frame containing eccentricity
#' values by node ID value.
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
#' graph %>%
#'   get_eccentricity()
#' #>    id eccentricity
#' #> 1   1            2
#' #> 2   2            3
#' #> 3   3            3
#' #> 4   4            2
#' #> 5   5            3
#' #> 6   6            2
#' #> 7   7            3
#' #> 8   8            3
#' #> 9   9            2
#' #> 10 10            0
#' @importFrom igraph eccentricity
#' @export get_eccentricity

get_eccentricity <- function(graph,
                             mode = "out") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, eccentricity values cannot be determined.")
  }

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
