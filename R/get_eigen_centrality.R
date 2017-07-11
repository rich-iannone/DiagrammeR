#' Get the eigen centrality for nodes in the graph
#' @description Get the eigen centrality values for
#' all nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param weights_attr an optional name of the edge
#' attribute to use in the adjacency matrix. If
#' \code{NULL} then, if it exists, the \code{weight}
#' edge attribute of the graph will be used. If
#' \code{NA} then no edge weights will be used.
#' @return a data frame with eigen centrality scores
#' for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the eigen centrality scores for nodes
#' # in the graph
#' get_eigen_centrality(graph)
#' #>    id eigen_centrality
#' #> 1   1        0.5893005
#' #> 2   2        0.8160297
#' #> 3   3        0.6985080
#' #> 4   4        0.5669045
#' #> 5   5        0.8129328
#' #> 6   6        1.0000000
#' #> 7   7        0.6266077
#' #> 8   8        0.3372831
#' #> 9   9        0.8487792
#' #> 10 10        0.8583592
#' @importFrom igraph eigen_centrality
#' @export get_eigen_centrality

get_eigen_centrality <- function(graph,
                                 weights_attr = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  if (!is.null(weights_attr)) {

    if (inherits(weights_attr, "character")) {
      # Stop function if the edge attribute does not exist
      if (!(weights_attr %in% colnames(graph$edges_df))) {
        stop("The edge attribute to be used as weights does not exist in the graph.")
      }

      # Stop function if the edge attribute is not numeric
      if (!is.numeric(graph$edges_df[, which(colnames(graph$edges_df) == weights_attr)])) {
        stop("The edge attribute to be used as weights is not numeric.")
      }

      weights_attr <- graph$edges_df[, which(colnames(graph$edges_df) == weights_attr)]
    }
  }

  # Get the eigen centrality values for each of the
  # graph's nodes
  eigen_centrality_values <-
    igraph::eigen_centrality(
      graph = ig_graph,
      weights = weights_attr)

  # Create df with eigen centrality values
  eigen_centrality_values_df <-
    data.frame(
      id = names(eigen_centrality_values$vector),
      eigen_centrality = unname(eigen_centrality_values$vector),
      stringsAsFactors = FALSE)

  return(eigen_centrality_values_df)
}
