#' Get community membership by edge betweenness
#' @description Using edge betweenness, obtain the
#' group membership values for each of the nodes in
#' the graph.
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
#' get_cmty_edge_btwns(graph)
#' #>    id edge_btwns_group
#' #> 1   1                1
#' #> 2   2                1
#' #> 3   3                1
#' #> 4   4                2
#' #> 5   5                1
#' #> 6   6                2
#' #> 7   7                1
#' #> 8   8                1
#' #> 9   9                2
#' #> 10 10                1
#'
#' # Add the group membership values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(get_cmty_edge_btwns(.))
#' @importFrom igraph cluster_edge_betweenness membership
#' @export get_cmty_edge_btwns

get_cmty_edge_btwns <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_edge_betweenness()` function
  cmty_edge_btwns_obj <-
    igraph::cluster_edge_betweenness(ig_graph)

  # Create df with node memberships
  cmty_edge_btwns_df <-
    data.frame(
      id = as.integer(names(igraph::membership(cmty_edge_btwns_obj))),
      edge_btwns_group = as.vector(igraph::membership(cmty_edge_btwns_obj)),
      stringsAsFactors = FALSE)

  return(cmty_edge_btwns_df)
}
