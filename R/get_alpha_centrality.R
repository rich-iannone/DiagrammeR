#' Get the alpha centrality for nodes in the graph
#' @description Get the alpha centrality values for
#' all nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param alpha the parameter that specifies the
#' relative importance of endogenous versus exogenous
#' factors in the determination of centrality.
#' @param exo the exogenous factors, in most cases
#' this is either a constant (which applies the same
#' factor to every node), or a vector giving the factor
#' for every node.
#' @param weights_attr an optional name of the edge
#' attribute to use in the adjacency matrix. If
#' \code{NULL} then, if it exists, the \code{weight}
#' edge attribute of the graph will be used. Failing
#' that, the standard adjacency matrix will be used
#' in calculations.
#' @param tol the tolerance for near-singularities
#' during matrix inversion. Default value is set to
#' \code{1e-7}.
#' @return a data frame with alpha centrality scores
#' for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the alpha centrality scores for nodes
#' # in the graph
#' get_alpha_centrality(graph)
#' #>    id alpha_centrality
#' #> 1   1                1
#' #> 2   2                1
#' #> 3   3                2
#' #> 4   4                1
#' #> 5   5                4
#' #> 6   6                9
#' #> 7   7                5
#' #> 8   8               11
#' #> 9   9               18
#' #> 10 10               34
#' @importFrom igraph alpha_centrality
#' @export get_alpha_centrality

get_alpha_centrality <- function(graph,
                                 alpha = 1,
                                 exo = 1,
                                 weights_attr = NULL,
                                 tol = 1e-7) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the alpha centrality values for each of the
  # graph's nodes
  alpha_centrality_values <-
    igraph::alpha_centrality(
      graph = ig_graph,
      alpha = alpha,
      exo = exo,
      weights = weights_attr,
      tol = tol)

  # Create df with alpha centrality values
  data.frame(
    id = alpha_centrality_values %>%
      names() %>%
      as.integer(),
    alpha_centrality = alpha_centrality_values,
    stringsAsFactors = FALSE)
}
