#' Get community membership using the Walktrap method
#' @description With the Walktrap community finding
#' algorithm, obtain the group membership values for
#' each of the nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param steps the number of steps to take for each
#' of the random walks.
#' @return a data frame with group membership
#' assignments for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the group membership values for all nodes
#' # in the graph through the Walktrap community
#' # finding algorithm
#' get_cmty_walktrap(graph)
#' #>    id walktrap_group
#' #> 1   1              1
#' #> 2   2              1
#' #> 3   3              1
#' #> 4   4              2
#' #> 5   5              1
#' #> 6   6              2
#' #> 7   7              1
#' #> 8   8              1
#' #> 9   9              2
#' #> 10 10              1
#'
#' # Add the group membership values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(get_cmty_walktrap(.))
#' @importFrom igraph cluster_walktrap membership
#' @export get_cmty_walktrap

get_cmty_walktrap <- function(graph,
                              steps = 4) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_walktrap()` function
  cmty_walktrap_obj <-
    igraph::cluster_walktrap(ig_graph, steps = steps)

  # Create df with node memberships
  cmty_walktrap_df <-
    data.frame(
      id = as.integer(names(igraph::membership(cmty_walktrap_obj))),
      walktrap_group = as.vector(igraph::membership(cmty_walktrap_obj)),
      stringsAsFactors = FALSE)

  return(cmty_walktrap_df)
}
