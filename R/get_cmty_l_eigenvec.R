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
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the group membership values for all
#' # nodes in the graph through calculation of
#' # the leading non-negative eigenvector of the
#' # modularity matrix of the graph
#' get_cmty_l_eigenvec(graph)
#' #>    id l_eigenvec_group
#' #> 1   1                1
#' #> 2   2                2
#' #> 3   3                2
#' #> 4   4                1
#' #> 5   5                1
#' #> 6   6                1
#' #> 7   7                2
#' #> 8   8                1
#' #> 9   9                2
#' #> 10 10                1
#'
#' # Add the group membership values to the
#' # graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_cmty_l_eigenvec(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value l_eigenvec_group
#' #> 1   1 <NA>     1   6.0                1
#' #> 2   2 <NA>     2   2.5                2
#' #> 3   3 <NA>     3   3.5                2
#' #> 4   4 <NA>     4   7.5                1
#' #> 5   5 <NA>     5   8.5                1
#' #> 6   6 <NA>     6   4.5                1
#' #> 7   7 <NA>     7  10.0                2
#' #> 8   8 <NA>     8  10.0                1
#' #> 9   9 <NA>     9   8.5                2
#' #> 10 10 <NA>    10  10.0                1
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
