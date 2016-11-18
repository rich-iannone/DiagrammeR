#' Get community membership by leading eigenvector
#' @description Through the calculation of the leading
#' non-negative eigenvector of the modularity matrix
#' of the graph, obtain the group membership values for
#' each of the nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with group membership
#' assignments for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the group membership values for all nodes
#' # in the graph through calculation of the leading
#' # non-negative eigenvector of the modularity matrix
#' # of the graph
#' get_cmty_l_eigenvec(graph)
#' #>    id l_eigenvec_group
#' #> 1   1                1
#' #> 2   2                1
#' #> 3   3                1
#' #> 4   4                2
#' #> 5   5                1
#' #> 6   6                2
#' #> 7   7                2
#' #> 8   8                2
#' #> 9   9                2
#' #> 10 10                1
#'
#' # Add the group membership values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(get_cmty_l_eigenvec(.))
#' @importFrom igraph cluster_leading_eigen membership
#' @export get_cmty_l_eigenvec

get_cmty_l_eigenvec <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If graph is directed, transform to undirected
  graph <- set_graph_undirected(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_leading_eigen()` function
  cmty_l_eigenvec_obj <-
    igraph::cluster_leading_eigen(ig_graph)

  # Create df with node memberships
  cmty_l_eigenvec_df <-
    data.frame(
      id = as.integer(names(igraph::membership(cmty_l_eigenvec_obj))),
      l_eigenvec_group = as.vector(igraph::membership(cmty_l_eigenvec_obj)),
      stringsAsFactors = FALSE)

  return(cmty_l_eigenvec_df)
}
