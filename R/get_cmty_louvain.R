#' Get community membership by Louvain optimization
#' @description Through the use of multi-level
#' optimization of a modularity score, obtain the
#' group membership values for each of the nodes in
#' the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with group membership
#' assignments for each of the nodes.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the group membership values for all nodes
#' # in the graph through the multi-level optimization
#' # of modularity algorithm
#' get_cmty_louvain(graph)
#' #>    node louvain_group
#' #> 1     1             1
#' #> 2     2             3
#' #> 3     3             3
#' #> 4     4             2
#' #> 5     5             1
#' #> 6     6             2
#' #> 7     7             2
#' #> 8     8             1
#' #> 9     9             2
#' #> 10   10             3
#'
#' # Add the group membership values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(get_cmty_louvain(.))
#' @importFrom igraph cluster_louvain membership
#' @export get_cmty_louvain

get_cmty_louvain <- function(graph) {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_louvain()` function
  cmty_louvain_obj <-
    igraph::cluster_louvain(ig_graph)

  # Create df with node memberships
  cmty_louvain_df <-
    data.frame(
      node = names(igraph::membership(cmty_louvain_obj)),
      louvain_group = as.vector(igraph::membership(cmty_louvain_obj)),
      stringsAsFactors = FALSE)

  return(cmty_louvain_df)
}
